module Stackctl.Spec.Deploy
  ( DeployOptions(..)
  , DeployConfirmation(..)
  , runDeployOptions
  , runDeploy
  ) where

import Stackctl.Prelude

import Options.Applicative
import RIO.Directory (createDirectoryIfMissing)
import RIO.List (headMaybe)
import qualified RIO.Text as T
import RIO.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Prompt
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec

data DeployOptions = DeployOptions
  { sdoSaveChangeSets :: Maybe FilePath
  , sdoDeployConfirmation :: DeployConfirmation
  , sdoClean :: Bool
  }

-- brittany-disable-next-binding

runDeployOptions :: Parser DeployOptions
runDeployOptions = DeployOptions
  <$> optional (strOption
    (  long "save-change-sets"
    <> metavar "DIRECTORY"
    <> help "Save executed changesets to DIRECTORY"
    ))
  <*> flag DeployWithConfirmation DeployWithoutConfirmation
    (  long "no-confirm"
    <> help "Don't confirm changes before executing"
    )
  <*> switch
    (  long "clean"
    <> help "Remove all changesets from Stack after deploy"
    )

runDeploy
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => DeployOptions
  -> m ()
runDeploy DeployOptions {..} = do
  specs <- discoverSpecs

  for_ specs $ \spec -> do
    logStackSpec spec

    handleRollbackComplete sdoDeployConfirmation $ stackSpecStackName spec

    emChangeSet <- createChangeSet spec

    case emChangeSet of
      Left err -> do
        logError $ display err
        exitFailure
      Right Nothing -> logInfo "Stack is up to date"
      Right (Just changeSet) -> do
        let stackName = stackSpecStackName spec

        for_ sdoSaveChangeSets $ \dir -> do
          let out = dir </> unpack (unStackName stackName) <.> "json"
          logInfo $ "Recording changeset details to " <> fromString out
          createDirectoryIfMissing True dir
          writeFileUtf8 out $ changeSetJSON changeSet

        deployChangeSet sdoDeployConfirmation changeSet
        when sdoClean $ awsCloudFormationDeleteAllChangeSets stackName

data DeployConfirmation
  = DeployWithConfirmation
  | DeployWithoutConfirmation
  deriving stock Eq

handleRollbackComplete
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => DeployConfirmation
  -> StackName
  -> m ()
handleRollbackComplete confirmation stackName = do
  mStack <- awsCloudFormationDescribeStackMaybe stackName

  when (maybe False stackIsRollbackComplete mStack) $ do
    logWarn
      $ "Stack "
      <> display stackName
      <> " is in ROLLBACK_COMPLETE state and must be deleted before proceeding"

    case confirmation of
      DeployWithConfirmation -> promptContinue
      DeployWithoutConfirmation -> do
        logError "Refusing to delete without confirmation"
        exitFailure

    result <- awsCloudFormationDeleteStack stackName

    case result of
      StackDeleteSuccess -> logInfo $ display result
      StackDeleteFailure{} ->
        logWarn $ display result <> ", deployment may fail"

deployChangeSet
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => DeployConfirmation
  -> ChangeSet
  -> m ()
deployChangeSet confirmation changeSet = do
  colors <- getColorsLogFunc
  logInfo $ formatTTY colors stackName $ Just changeSet

  case confirmation of
    DeployWithConfirmation -> promptContinue
    DeployWithoutConfirmation -> pure ()

  -- It can take a minute to get this batch of events to work out where we're
  -- tailing from, so do that part synchronously
  mLastId <- awsCloudFormationGetMostRecentStackEventId stackName
  asyncTail <- async $ tailStackEventsSince stackName mLastId

  logInfo $ "Executing ChangeSet " <> display changeSetId
  result <- do
    awsCloudFormationExecuteChangeSet changeSetId
    awsCloudFormationWait stackName

  cancel asyncTail

  let
    onSuccess = logInfo $ display result
    onFailure = do
      logError $ display result
      exitFailure

  case result of
    StackCreateSuccess -> onSuccess
    StackCreateFailure{} -> onFailure
    StackUpdateSuccess -> onSuccess
    StackUpdateFailure{} -> onFailure
 where
  stackName = csStackName changeSet
  changeSetId = csChangeSetId changeSet

tailStackEventsSince
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> Maybe Text -- ^ StackEventId
  -> m a
tailStackEventsSince stackName mLastId = do
  colors <- getColorsLogFunc
  events <- awsCloudFormationDescribeStackEvents stackName mLastId
  traverse_ (logInfo <=< formatStackEvent colors) $ reverse events

  -- Without this small delay before looping, our requests seem to hang
  -- intermittently (without errors) and often we miss events.
  threadDelay $ 1 * 1000000

  -- Tail from the next "last id". If we got no events, be sure to pass along
  -- any last-id we were given
  tailStackEventsSince stackName $ getLastEventId events <|> mLastId

formatStackEvent :: MonadIO m => Colors -> StackEvent -> m Utf8Builder
formatStackEvent Colors {..} e = do
  timestamp <-
    liftIO $ formatTime defaultTimeLocale "%F %T %Z" <$> utcToLocalZonedTime
      (e ^. stackEvent_timestamp)

  pure $ mconcat
    [ fromString timestamp
    , " | "
    , maybe "" colorStatus $ e ^. stackEvent_resourceStatus
    , maybe "" (magenta . display . (" " <>))
    $ e
    ^. stackEvent_logicalResourceId
    , maybe "" ((\x -> " (" <> x <> ")") . display . T.strip)
    $ e
    ^. stackEvent_resourceStatusReason
    ]
 where
  colorStatus = \case
    ResourceStatus' x
      | "ROLLBACK" `T.isInfixOf` x -> red (display x)
      | "COMPLETE" `T.isSuffixOf` x -> green (display x)
      | "FAILED" `T.isSuffixOf` x -> red (display x)
      | "IN_PROGRESS" `T.isSuffixOf` x -> blue (display x)
      | "SKIPPED" `T.isSuffixOf` x -> yellow (display x)
      | otherwise -> display x

getLastEventId :: [StackEvent] -> Maybe Text
getLastEventId = fmap (^. stackEvent_eventId) . headMaybe
