module Stackctl.AWS.CloudFormation
  (
  -- * Stacks
    Stack(..)
  , stackIsRollbackComplete
  , StackId(..)
  , StackName(..)
  , StackEvent(..)
  , ResourceStatus(..)
  , stackEvent_eventId
  , stackEvent_logicalResourceId
  , stackEvent_resourceStatus
  , stackEvent_resourceStatusReason
  , stackEvent_timestamp
  , StackTemplate(..)
  , StackDeployResult(..)
  , StackDeleteResult(..)
  , Parameter
  , parameter_parameterKey
  , parameter_parameterValue
  , newParameter
  , makeParameter
  , readParameter
  , Capability(..)
  , Tag
  , newTag
  , tag_key
  , tag_value
  , Output
  , output_outputKey
  , output_outputValue
  , awsCloudFormationDescribeStack
  , awsCloudFormationDescribeStackMaybe
  , awsCloudFormationDescribeStackEvents
  , awsCloudFormationGetMostRecentStackEventId
  , awsCloudFormationDeleteStack
  , awsCloudFormationWait
  , awsCloudFormationGetTemplate

  -- * ChangeSets
  , ChangeSet(..)
  , changeSetJSON
  , ChangeSetId(..)
  , ChangeSetName(..)
  , Change(..)
  , ResourceChange(..)
  , Replacement(..)
  , ChangeAction(..)
  , ResourceAttribute(..)
  , ResourceChangeDetail(..)
  , ChangeSource(..)
  , ResourceTargetDefinition(..)
  , RequiresRecreation(..)
  , awsCloudFormationCreateChangeSet
  , awsCloudFormationExecuteChangeSet
  , awsCloudFormationDeleteAllChangeSets
  ) where

import Stackctl.Prelude

import Amazonka.CloudFormation.CreateChangeSet hiding (id)
import Amazonka.CloudFormation.DeleteChangeSet
import Amazonka.CloudFormation.DeleteStack
import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.DescribeStackEvents
import Amazonka.CloudFormation.DescribeStacks
import Amazonka.CloudFormation.ExecuteChangeSet
import Amazonka.CloudFormation.GetTemplate
import Amazonka.CloudFormation.ListChangeSets
import Amazonka.CloudFormation.Types
import qualified Amazonka.CloudFormation.Types.ChangeSetSummary as Summary
import Amazonka.CloudFormation.Waiters
import Amazonka.Core
  (AsError, ServiceError, _MatchServiceError, hasStatus, serviceMessage)
import Amazonka.Waiter (Accept(..))
import Conduit
import Control.Lens ((?~))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Monoid (First)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)
import RIO.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Stackctl.AWS.Core
import UnliftIO.Exception.Lens (handling_, trying)

newtype StackId = StackId
  { unStackId :: Text
  }
  deriving newtype (Eq, Ord, Show, Display, FromJSON, ToJSON)

newtype StackName = StackName
  { unStackName :: Text
  }
  deriving newtype (Eq, Ord, Show, Display, FromJSON, ToJSON)

newtype StackTemplate = StackTemplate
  { unStackTemplate :: FilePath
  }
  deriving stock (Eq, Show)
  deriving newtype FromJSON

instance Display StackTemplate where
  display = fromString . unStackTemplate

data StackDeployResult
  = StackCreateSuccess
  | StackCreateFailure Bool
  | StackUpdateSuccess
  | StackUpdateFailure Bool
  deriving stock Show

instance Display StackDeployResult where
  display = \case
    StackCreateSuccess -> "Created Stack successfully"
    StackCreateFailure{} -> "Failed to create Stack"
    StackUpdateSuccess -> "Updated Stack successfully"
    StackUpdateFailure{} -> "Failed to update Stack"

stackCreateResult :: Accept -> StackDeployResult
stackCreateResult = \case
  AcceptSuccess -> StackCreateSuccess
  AcceptFailure -> StackCreateFailure False
  AcceptRetry -> StackCreateFailure True

stackUpdateResult :: Accept -> StackDeployResult
stackUpdateResult = \case
  AcceptSuccess -> StackUpdateSuccess
  AcceptFailure -> StackUpdateFailure False
  AcceptRetry -> StackUpdateFailure True

data StackDeleteResult
  = StackDeleteSuccess
  | StackDeleteFailure Bool

instance Display StackDeleteResult where
  display = \case
    StackDeleteSuccess -> "Deleted Stack successfully"
    StackDeleteFailure{} -> "Failed to delete Stack"

stackDeleteResult :: Accept -> StackDeleteResult
stackDeleteResult = \case
  AcceptSuccess -> StackDeleteSuccess
  AcceptFailure -> StackDeleteFailure False
  AcceptRetry -> StackDeleteFailure True

-- | @stackctl-{timestamp}-{uuid}@
newChangeSetName :: MonadIO m => m ChangeSetName
newChangeSetName = do
  timestamp <- formatTime defaultTimeLocale "%Y%m%d%H%M" <$> getCurrentTime
  uuid <- liftIO $ UUID.toString <$> UUID.nextRandom
  let parts = ["stackctl", timestamp, uuid]
  pure $ ChangeSetName $ T.intercalate "-" $ map pack parts

awsCloudFormationDescribeStack
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> m Stack
awsCloudFormationDescribeStack stackName = do
  let
    req = newDescribeStacks & describeStacks_stackName ?~ unStackName stackName

  awsSimple "DescribeStack" req $ \resp -> do
    stacks <- resp ^. describeStacksResponse_stacks
    listToMaybe stacks

awsCloudFormationDescribeStackMaybe
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => StackName
  -> m (Maybe Stack)
awsCloudFormationDescribeStackMaybe stackName =
  -- AWS gives us a 400 if the stackName doesn't exist, rather than simply
  -- returning an empty list, so we need to do this through exceptions
  handling_ _ValidationError (pure Nothing) $ do
    Just <$> awsCloudFormationDescribeStack stackName

awsCloudFormationDescribeStackEvents
  :: (MonadResource m, MonadReader env m, HasAwsEnv env)
  => StackName
  -> Maybe Text -- ^ Last-seen Id
  -> m [StackEvent]
awsCloudFormationDescribeStackEvents stackName mLastId = do
  let
    req =
      newDescribeStackEvents
        & describeStackEvents_stackName
        ?~ unStackName stackName

  runConduit
    $ awsPaginate req
    .| mapC (fromMaybe [] . (^. describeStackEventsResponse_stackEvents))
    .| concatC
    .| takeWhileC (\e -> Just (e ^. stackEvent_eventId) /= mLastId)
    .| sinkList

awsCloudFormationGetMostRecentStackEventId
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> m (Maybe Text)
awsCloudFormationGetMostRecentStackEventId stackName = do
  let
    req =
      newDescribeStackEvents
        & describeStackEvents_stackName
        ?~ unStackName stackName

    -- Events are returned most-recent first, so "last" is "first" here
    getFirstEventId :: [StackEvent] -> Maybe Text
    getFirstEventId = \case
      [] -> Nothing
      (e : _) -> Just $ e ^. stackEvent_eventId

  awsSimple "DescribeStackEvents" req
    $ pure
    . getFirstEventId
    . fromMaybe []
    . (^. describeStackEventsResponse_stackEvents)

awsCloudFormationDeleteStack
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> m StackDeleteResult
awsCloudFormationDeleteStack stackName = do
  let
    deleteReq = newDeleteStack $ unStackName stackName
    describeReq =
      newDescribeStacks & describeStacks_stackName ?~ unStackName stackName

  awsSimple "DeleteStack" deleteReq $ const $ pure ()

  logDebug "Awaiting DeleteStack"
  stackDeleteResult <$> awsAwait newStackDeleteComplete describeReq

awsCloudFormationWait
  :: (MonadUnliftIO m, MonadResource m, MonadReader env m, HasAwsEnv env)
  => StackName
  -> m StackDeployResult
awsCloudFormationWait stackName = do
  either stackCreateResult stackUpdateResult <$> race
    (awsAwait newStackCreateComplete req)
    (awsAwait newStackUpdateComplete req)
 where
  req = newDescribeStacks & describeStacks_stackName ?~ unStackName stackName

awsCloudFormationGetTemplate
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> m Value
awsCloudFormationGetTemplate stackName = do
  let
    req =
      newGetTemplate
        & (getTemplate_stackName ?~ unStackName stackName)
        . (getTemplate_templateStage ?~ TemplateStage_Original)

    -- If decodeStrict fails, assume it's a String of Yaml. See writeStackSpec.
    decodeTemplateBody body =
      fromMaybe (toJSON body) $ decodeStrict $ encodeUtf8 body

  awsSimple "GetTemplate" req $ \resp -> do
    body <- resp ^. getTemplateResponse_templateBody
    pure $ decodeTemplateBody body

makeParameter :: Text -> Maybe Text -> Parameter
makeParameter k v =
  newParameter & (parameter_parameterKey ?~ k) . (parameter_parameterValue .~ v)

readParameter :: String -> Either String Parameter
readParameter x = case T.breakOn "=" $ pack x of
  (key, _) | T.null key -> invalid "empty KEY"
  (_, value) | T.null value -> invalid "empty VALUE"
  (_, "=") -> invalid "empty VALUE" -- Is supporting { k, Nothing } valuable?
  (pName, value) -> Right $ makeParameter pName $ Just $ T.drop 1 value
 where
  invalid msg = Left $ "Invalid format for parameter (KEY=VALUE): " <> msg

newtype ChangeSetId = ChangeSetId
  { unChangeSetId :: Text
  }
  deriving newtype (Display, FromJSON, ToJSON)

newtype ChangeSetName = ChangeSetName
  { unChangeSetName :: Text
  }
  deriving newtype (Display, FromJSON, ToJSON)

data ChangeSet = ChangeSet
  { csCreationTime :: UTCTime
  , csChanges :: Maybe [Change]
  , csChangeSetName :: ChangeSetName
  , csExecutionStatus :: ExecutionStatus
  , csChangeSetId :: ChangeSetId
  , csParameters :: Maybe [Parameter]
  , csStackId :: StackId
  , csCapabilities :: Maybe [Capability]
  , csTags :: Maybe [Tag]
  , csStackName :: StackName
  , csStatus :: ChangeSetStatus
  , csStatusReason :: Maybe Text
  , csResponse :: DescribeChangeSetResponse
  }

changeSetJSON :: ChangeSet -> Text
changeSetJSON = decodeUtf8 . BSL.toStrict . encodePretty . csResponse

changeSetFailed :: ChangeSet -> Bool
changeSetFailed = (== ChangeSetStatus_FAILED) . csStatus

awsCloudFormationCreateChangeSet
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => StackName
  -> StackTemplate
  -> [Parameter]
  -> [Capability]
  -> [Tag]
  -> m (Either Text (Maybe ChangeSet))
awsCloudFormationCreateChangeSet stackName stackTemplate parameters capabilities tags
  = fmap (first $ maybe "Unknown validation error" toText . view serviceMessage)
    $ trying _ValidationError
    $ do
        name <- newChangeSetName
        templateBody <- readFileUtf8 $ unStackTemplate stackTemplate

        mStack <- awsCloudFormationDescribeStackMaybe stackName
        let
          changeSetType = fromMaybe ChangeSetType_CREATE $ do
            stack <- mStack
            pure $ if stackIsAbandonedCreate stack
              then ChangeSetType_CREATE
              else ChangeSetType_UPDATE

        let
          req =
            newCreateChangeSet (unStackName stackName) (unChangeSetName name)
              & (createChangeSet_changeSetType ?~ changeSetType)
              . (createChangeSet_templateBody ?~ templateBody)
              . (createChangeSet_parameters ?~ parameters)
              . (createChangeSet_capabilities ?~ capabilities)
              . (createChangeSet_tags ?~ tags)

        logInfo "Creating changeset..."
        csId <- awsSimple "CreateChangeSet" req (^. createChangeSetResponse_id)

        logDebug "Awaiting CREATE_COMPLETE"
        void $ awsAwait newChangeSetCreateComplete $ newDescribeChangeSet csId

        logInfo "Retrieving changeset..."
        cs <- awsCloudFormationDescribeChangeSet $ ChangeSetId csId
        pure $ cs <$ guard (not $ changeSetFailed cs)

awsCloudFormationDescribeChangeSet
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => ChangeSetId
  -> m ChangeSet
awsCloudFormationDescribeChangeSet changeSetId = do
  let req = newDescribeChangeSet $ unChangeSetId changeSetId
  awsSimple "DescribeChangeSet" req $ \resp ->
    ChangeSet
      <$> (resp ^. describeChangeSetResponse_creationTime)
      <*> pure (resp ^. describeChangeSetResponse_changes)
      <*> (ChangeSetName <$> resp ^. describeChangeSetResponse_changeSetName)
      <*> (resp ^. describeChangeSetResponse_executionStatus)
      <*> (ChangeSetId <$> resp ^. describeChangeSetResponse_changeSetId)
      <*> pure (resp ^. describeChangeSetResponse_parameters)
      <*> (StackId <$> resp ^. describeChangeSetResponse_stackId)
      <*> pure (resp ^. describeChangeSetResponse_capabilities)
      <*> pure (resp ^. describeChangeSetResponse_tags)
      <*> (StackName <$> resp ^. describeChangeSetResponse_stackName)
      <*> pure (resp ^. describeChangeSetResponse_status)
      <*> pure (resp ^. describeChangeSetResponse_statusReason)
      <*> pure resp

awsCloudFormationExecuteChangeSet
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => ChangeSetId -> m ()
awsCloudFormationExecuteChangeSet changeSetId = do
  void $ awsSend $ newExecuteChangeSet $ unChangeSetId changeSetId

awsCloudFormationDeleteAllChangeSets
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => StackName
  -> m ()
awsCloudFormationDeleteAllChangeSets stackName = do
  logInfo $ "Deleting all changesets from " <> display stackName <> "..."
  runConduit
    $ awsPaginate (newListChangeSets $ unStackName stackName)
    .| concatMapC
         (\resp -> fromMaybe [] $ do
           ss <- resp ^. listChangeSetsResponse_summaries
           pure $ mapMaybe Summary.changeSetId ss
         )
    .| mapM_C
         (\csId -> do
           logInfo $ "Enqueing delete of " <> display csId
           void $ awsSend $ newDeleteChangeSet csId
         )

-- | Did we abandoned this Stack's first ever ChangeSet?
--
-- If you create a ChangeSet for a Stack that doesn't exist, but you don't
-- deploy it, then you attempt to create another ChangeSet, you need to use the
-- CREATE ChangeSetType still, or you'll get a "Stack does not exist" error.
--
-- This is despite the Stack very much existing, according to DescribeStacks,
-- with a creationTime, ARN, and everything.
--
-- Our hueristic for finding these is under review but with no previous
-- updates (no lastUpdatedTime), presumably meaning it's still in its /first/
-- review.
--
stackIsAbandonedCreate :: Stack -> Bool
stackIsAbandonedCreate stack =
  stack ^. stack_stackStatus == StackStatus_REVIEW_IN_PROGRESS && isNothing
    (stack ^. stack_lastUpdatedTime)

stackIsRollbackComplete :: Stack -> Bool
stackIsRollbackComplete stack =
  stack ^. stack_stackStatus == StackStatus_ROLLBACK_COMPLETE

_ValidationError :: AsError a => Getting (First ServiceError) a ServiceError
_ValidationError =
  _MatchServiceError defaultService "ValidationError" . hasStatus 400
