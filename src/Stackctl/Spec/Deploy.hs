module Stackctl.Spec.Deploy
  ( DeployOptions(..)
  , DeployConfirmation(..)
  , runDeployOptions
  , runDeploy
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLoggerLn)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Options.Applicative
import Stackctl.Action
import Stackctl.AWS hiding (action)
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.ParameterOption
import Stackctl.Prompt
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.TagOption
import UnliftIO.Directory (createDirectoryIfMissing)

data DeployOptions = DeployOptions
  { sdoParameters :: [Parameter]
  , sdoTags :: [Tag]
  , sdoSaveChangeSets :: Maybe FilePath
  , sdoDeployConfirmation :: DeployConfirmation
  , sdoClean :: Bool
  }

-- brittany-disable-next-binding

runDeployOptions :: Parser DeployOptions
runDeployOptions = DeployOptions
  <$> many parameterOption
  <*> many tagOption
  <*> optional (strOption
    (  long "save-change-sets"
    <> metavar "DIRECTORY"
    <> help "Save executed changesets to DIRECTORY"
    <> action "directory"
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
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsScope env
     , HasAwsEnv env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => DeployOptions
  -> m ()
runDeploy DeployOptions {..} = do
  specs <- discoverSpecs

  for_ specs $ \spec -> do
    withThreadContext ["stackName" .= stackSpecStackName spec] $ do
      handleRollbackComplete sdoDeployConfirmation $ stackSpecStackName spec

      emChangeSet <- createChangeSet spec sdoParameters sdoTags

      case emChangeSet of
        Left err -> do
          logError $ "Error creating ChangeSet" :# ["error" .= err]
          exitFailure
        Right Nothing -> logInfo "Stack is up to date"
        Right (Just changeSet) -> do
          let stackName = stackSpecStackName spec

          for_ sdoSaveChangeSets $ \dir -> do
            let out = dir </> unpack (unStackName stackName) <.> "json"
            logInfo $ "Recording changeset" :# ["path" .= out]
            createDirectoryIfMissing True dir
            writeFileUtf8 out $ changeSetJSON changeSet

          deployChangeSet sdoDeployConfirmation changeSet
          runActions stackName PostDeploy $ stackSpecActions spec
          when sdoClean $ awsCloudFormationDeleteAllChangeSets stackName

data DeployConfirmation
  = DeployWithConfirmation
  | DeployWithoutConfirmation
  deriving stock Eq

handleRollbackComplete
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsEnv env
     )
  => DeployConfirmation
  -> StackName
  -> m ()
handleRollbackComplete confirmation stackName = do
  mStack <- awsCloudFormationDescribeStackMaybe stackName

  when (maybe False stackIsRollbackComplete mStack) $ do
    logWarn
      $ "Stack is in ROLLBACK_COMPLETE state and must be deleted before proceeding"
      :# ["stackName" .= stackName]

    case confirmation of
      DeployWithConfirmation -> promptContinue
      DeployWithoutConfirmation -> do
        logError "Refusing to delete without confirmation"
        exitFailure

    result <- awsCloudFormationDeleteStack stackName

    case result of
      StackDeleteSuccess -> logInfo $ prettyStackDeleteResult result :# []
      StackDeleteFailure{} -> logWarn $ prettyStackDeleteResult result :# []

deployChangeSet
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsEnv env
     )
  => DeployConfirmation
  -> ChangeSet
  -> m ()
deployChangeSet confirmation changeSet = do
  colors <- getColorsLogger

  pushLoggerLn $ formatTTY colors (unStackName stackName) $ Just changeSet

  case confirmation of
    DeployWithConfirmation -> promptContinue
    DeployWithoutConfirmation -> pure ()

  -- It can take a minute to get this batch of events to work out where we're
  -- tailing from, so do that part synchronously
  mLastId <- awsCloudFormationGetMostRecentStackEventId stackName
  asyncTail <- async $ tailStackEventsSince stackName mLastId

  logInfo $ "Executing ChangeSet" :# ["changeSetId" .= changeSetId]
  result <- do
    awsCloudFormationExecuteChangeSet changeSetId
    awsCloudFormationWait stackName

  cancel asyncTail

  let
    onSuccess = logInfo $ prettyStackDeployResult result :# []
    onFailure = do
      logError $ prettyStackDeployResult result :# []
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
  :: ( MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsEnv env
     )
  => StackName
  -> Maybe Text -- ^ StackEventId
  -> m a
tailStackEventsSince stackName mLastId = do
  colors <- getColorsLogger
  events <- awsCloudFormationDescribeStackEvents stackName mLastId
  traverse_ (pushLoggerLn <=< formatStackEvent colors) $ reverse events

  -- Without this small delay before looping, our requests seem to hang
  -- intermittently (without errors) and often we miss events.
  threadDelay $ 1 * 1000000

  -- Tail from the next "last id". If we got no events, be sure to pass along
  -- any last-id we were given
  tailStackEventsSince stackName $ getLastEventId events <|> mLastId

formatStackEvent :: MonadIO m => Colors -> StackEvent -> m Text
formatStackEvent Colors {..} e = do
  timestamp <-
    liftIO $ formatTime defaultTimeLocale "%F %T %Z" <$> utcToLocalZonedTime
      (e ^. stackEvent_timestamp)

  pure $ mconcat
    [ fromString timestamp
    , " | "
    , maybe "" colorStatus $ e ^. stackEvent_resourceStatus
    , maybe "" (magenta . (" " <>)) $ e ^. stackEvent_logicalResourceId
    , maybe "" ((\x -> " (" <> x <> ")") . T.strip)
    $ e
    ^. stackEvent_resourceStatusReason
    ]
 where
  colorStatus = \case
    ResourceStatus' x
      | "ROLLBACK" `T.isInfixOf` x -> red x
      | "COMPLETE" `T.isSuffixOf` x -> green x
      | "FAILED" `T.isSuffixOf` x -> red x
      | "IN_PROGRESS" `T.isSuffixOf` x -> blue x
      | "SKIPPED" `T.isSuffixOf` x -> yellow x
      | otherwise -> x

getLastEventId :: [StackEvent] -> Maybe Text
getLastEventId = fmap (^. stackEvent_eventId) . listToMaybe
