module Stackctl.Spec.Deploy
  ( DeployOptions (..)
  , DeployConfirmation (..)
  , parseDeployOptions
  , runDeploy
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLoggerLn)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime, utcToLocalZonedTime)
import Options.Applicative
import Stackctl.AWS hiding (action)
import Stackctl.AWS.Scope
import Stackctl.Action
import qualified Stackctl.CancelHandler as CancelHandler
import Stackctl.Colors
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.ParameterOption
import Stackctl.Prompt
import Stackctl.RemovedStack
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
  , sdoRemovals :: Bool
  , sdoClean :: Bool
  }

-- brittany-disable-next-binding

parseDeployOptions :: Parser DeployOptions
parseDeployOptions =
  DeployOptions
    <$> many parameterOption
    <*> many tagOption
    <*> optional
      ( strOption
          ( long "save-change-sets"
              <> metavar "DIRECTORY"
              <> help "Save executed changesets to DIRECTORY"
              <> action "directory"
          )
      )
    <*> flag
      DeployWithConfirmation
      DeployWithoutConfirmation
      ( long "no-confirm"
          <> help "Don't confirm changes before executing"
      )
    <*> ( not
            <$> switch
              ( long "no-remove"
                  <> help "Don't delete removed Stacks"
              )
        )
    <*> switch
      ( long "clean"
          <> help "Remove all changesets from Stack after deploy"
      )

runDeploy
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsScope env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => DeployOptions
  -> m ()
runDeploy DeployOptions {..} = do
  when sdoRemovals $ do
    removed <- inferRemovedStacks
    traverse_ (deleteRemovedStack sdoDeployConfirmation) removed

  forEachSpec_ $ \spec -> do
    withThreadContext ["stackName" .= stackSpecStackName spec] $ do
      checkIfStackRequiresDeletion sdoDeployConfirmation
        $ stackSpecStackName spec

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

deleteRemovedStack
  :: ( MonadIO m
     , MonadMask m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => DeployConfirmation
  -> Stack
  -> m ()
deleteRemovedStack confirmation stack = do
  withThreadContext ["stack" .= stackName] $ do
    colors <- getColorsLogger
    pushLoggerLn $ formatRemovedStack colors FormatTTY stack

    case confirmation of
      DeployWithConfirmation -> do
        promptContinue
        logInfo "Deleting Stack"
      DeployWithoutConfirmation -> pure ()

    deleteStack stackName
 where
  stackName = StackName $ stack ^. stack_stackName

data DeployConfirmation
  = DeployWithConfirmation
  | DeployWithoutConfirmation
  deriving stock (Eq)

checkIfStackRequiresDeletion
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => DeployConfirmation
  -> StackName
  -> m ()
checkIfStackRequiresDeletion confirmation stackName = do
  mStack <- awsCloudFormationDescribeStackMaybe stackName

  for_ (stackStatusRequiresDeletion =<< mStack) $ \status -> do
    logWarn $ "Stack must be deleted before proceeding" :# ["status" .= status]
    when (status == StackStatus_ROLLBACK_FAILED)
      $ logWarn
        "Stack is in ROLLBACK_FAILED. This may require elevated permissions for the delete to succeed"

    case confirmation of
      DeployWithConfirmation -> promptContinue
      DeployWithoutConfirmation -> do
        logError "Refusing to delete without confirmation"
        exitFailure

    logInfo "Deleting Stack"
    deleteStack stackName

deleteStack
  :: (MonadIO m, MonadAWS m, MonadLogger m)
  => StackName
  -> m ()
deleteStack stackName = do
  result <- awsCloudFormationDeleteStack stackName

  case result of
    StackDeleteSuccess -> logInfo $ prettyStackDeleteResult result :# []
    StackDeleteFailure {} -> logWarn $ prettyStackDeleteResult result :# []

deployChangeSet
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
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

  let onCancel = do
        logInfo "Canceling stack update, press ^C again to abort"
        case csChangeSetType changeSet of
          ChangeSetType_UPDATE -> do
            awsCloudFormationCancelUpdateStack stackName
            cancel asyncTail
          t -> logWarn $ "Cannot cancel change-set of this type" :# ["type" .= t]

  logInfo $ "Executing ChangeSet" :# ["changeSetId" .= changeSetId]
  result <- CancelHandler.with onCancel $ do
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
    StackCreateFailure {} -> onFailure
    StackUpdateSuccess -> onSuccess
    StackUpdateFailure {} -> onFailure
 where
  stackName = csStackName changeSet
  changeSetId = csChangeSetId changeSet

tailStackEventsSince
  :: ( MonadIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => StackName
  -> Maybe Text
  -- ^ StackEventId
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
    liftIO
      $ formatTime defaultTimeLocale "%F %T %Z"
        <$> utcToLocalZonedTime (e ^. stackEvent_timestamp)

  pure
    $ mconcat
      [ fromString timestamp
      , " | "
      , maybe "" colorStatus $ e ^. stackEvent_resourceStatus
      , maybe "" (magenta . (" " <>)) $ e ^. stackEvent_logicalResourceId
      , maybe "" ((\x -> " (" <> x <> ")") . T.strip)
          $ e ^. stackEvent_resourceStatusReason
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
