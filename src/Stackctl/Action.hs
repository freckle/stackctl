{-# LANGUAGE NamedFieldPuns #-}

-- | Actions that can be performed on certain Stack management events
--
-- For example, to invoke a Lambda whose name is found in the deploying Stack's
-- outputs after it's been deployed:
--
-- @
-- Actions:
--   - on: PostDeploy
--     run:
--       InvokeLambdaByStackOutput: OnDeployFunction
-- @
--
module Stackctl.Action
  ( Action
  , newAction
  , ActionOn(..)
  , ActionRun(..)
  , runActions
  ) where

import Stackctl.Prelude hiding (on)

import Data.Aeson
import Data.List (find)
import Stackctl.AWS
import Stackctl.AWS.Lambda

data Action = Action
  { on :: ActionOn
  , run :: ActionRun
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)

newAction :: ActionOn -> ActionRun -> Action
newAction = Action

data ActionOn = PostDeploy
  deriving stock (Eq, Generic)

instance FromJSON ActionOn where
  parseJSON = withText "ActionOn" $ \case
    "PostDeploy" -> pure PostDeploy
    x ->
      fail $ "Invalid ActionOn: " <> show x <> ", must be one of [PostDeploy]"

instance ToJSON ActionOn where
  toJSON = \case
    PostDeploy -> toJSON @Text "PostDeploy"
  toEncoding = \case
    PostDeploy -> toEncoding @Text "PostDeploy"

data ActionRun
  = InvokeLambdaByStackOutput Text
  | InvokeLambdaByName Text

instance FromJSON ActionRun where
  parseJSON = withObject "ActionRun" $ \o -> asum
    [ InvokeLambdaByStackOutput <$> o .: "InvokeLambdaByStackOutput"
    , InvokeLambdaByStackOutput <$> o .: "InvokeLambdaByName"
    ]

instance ToJSON ActionRun where
  toJSON = object . \case
    InvokeLambdaByStackOutput name -> ["InvokeLambdaByStackOutput" .= name]
    InvokeLambdaByName name -> ["InvokeLambdaByName" .= name]
  toEncoding = pairs . \case
    InvokeLambdaByStackOutput name -> "InvokeLambdaByStackOutput" .= name
    InvokeLambdaByName name -> "InvokeLambdaByName" .= name

data ActionFailure
  = NoSuchOutput
  | InvokeLambdaFailure
  deriving stock Show
  deriving anyclass Exception

runActions
  :: (MonadResource m, MonadLogger m, MonadReader env m, HasAwsEnv env)
  => StackName
  -> ActionOn
  -> [Action]
  -> m ()
runActions stackName on =
  traverse_ (runAction stackName) . filter (`shouldRunOn` on)

shouldRunOn :: Action -> ActionOn -> Bool
shouldRunOn Action { on } on' = on == on'

runAction
  :: (MonadResource m, MonadLogger m, MonadReader env m, HasAwsEnv env)
  => StackName
  -> Action
  -> m ()
runAction stackName Action { on, run } = do
  logInfo $ "Running action" :# ["on" .= on, "run" .= run]

  case run of
    InvokeLambdaByStackOutput outputName -> do
      outputs <- awsCloudFormationDescribeStackOutputs stackName
      case findOutputValue outputName outputs of
        Nothing -> do
          logError
            $ "Output not found"
            :# [ "stackName" .= stackName
               , "desiredOutput" .= outputName
               , "availableOutputs" .= map (^. output_outputKey) outputs
               ]
          throwIO NoSuchOutput
        Just name -> invoke name
    InvokeLambdaByName name -> invoke name
 where
  invoke name = do
    result <- awsLambdaInvoke name payload
    logLambdaInvocationResult result
    unless (isLambdaInvocationSuccess result) $ throwIO InvokeLambdaFailure

  payload = object ["stack" .= stackName, "event" .= on]

findOutputValue :: Text -> [Output] -> Maybe Text
findOutputValue name =
  view output_outputValue <=< find ((== Just name) . view output_outputKey)
