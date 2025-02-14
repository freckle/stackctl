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
module Stackctl.Action
  ( Action
  , newAction
  , ActionOn (..)
  , ActionRun (..)
  , runActions
  ) where

import Stackctl.Prelude hiding (on)

import Blammo.Logging.Logger (flushLogger)
import Data.Aeson
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Stackctl.AWS
import Stackctl.AWS.Lambda
import Stackctl.OneOrListOf
import qualified Stackctl.OneOrListOf as OneOrListOf
import System.Process.Typed

data Action = Action
  { on :: ActionOn
  , run :: OneOrListOf ActionRun
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

newAction :: ActionOn -> [ActionRun] -> Action
newAction on runs = Action {on, run = OneOrListOf.fromList runs}

data ActionOn = PostDeploy
  deriving stock (Eq, Show, Generic)

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
  | Exec (NonEmpty String)
  | Shell String
  deriving stock (Eq, Show)

instance FromJSON ActionRun where
  parseJSON = withObject "ActionRun" $ \o ->
    (InvokeLambdaByStackOutput <$> o .: "InvokeLambdaByStackOutput")
      <|> (InvokeLambdaByName <$> o .: "InvokeLambdaByName")
      <|> (Exec <$> o .: "Exec")
      <|> (Shell <$> o .: "Shell")

instance ToJSON ActionRun where
  toJSON =
    object . \case
      InvokeLambdaByStackOutput name -> ["InvokeLambdaByStackOutput" .= name]
      InvokeLambdaByName name -> ["InvokeLambdaByName" .= name]
      Exec args -> ["Exec" .= args]
      Shell arg -> ["Shell" .= arg]
  toEncoding =
    pairs . \case
      InvokeLambdaByStackOutput name -> "InvokeLambdaByStackOutput" .= name
      InvokeLambdaByName name -> "InvokeLambdaByName" .= name
      Exec args -> "Exec" .= args
      Shell arg -> "Shell" .= arg

data ActionFailure
  = NoSuchOutput
  | InvokeLambdaFailure
  | ExecFailure ExitCode
  deriving stock (Show)
  deriving anyclass (Exception)

runActions
  :: ( MonadIO m
     , MonadLogger m
     , MonadAWS m
     , MonadReader env m
     , HasLogger env
     )
  => StackName
  -> ActionOn
  -> [Action]
  -> m ()
runActions stackName on =
  traverse_ (runAction stackName) . filter (`shouldRunOn` on)

shouldRunOn :: Action -> ActionOn -> Bool
shouldRunOn Action {on} on' = on == on'

runAction
  :: ( MonadIO m
     , MonadLogger m
     , MonadAWS m
     , MonadReader env m
     , HasLogger env
     )
  => StackName
  -> Action
  -> m ()
runAction stackName Action {on, run} = do
  logInfo $ "Running action" :# ["on" .= on, "run" .= run]

  for_ run $ \case
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
    Exec args -> execProcessAction (NE.head args) (NE.tail args)
    Shell arg -> execProcessAction "sh" ["-c", arg]
 where
  invoke name = do
    result <- awsLambdaInvoke name payload
    logLambdaInvocationResult result
    unless (isLambdaInvocationSuccess result) $ throwIO InvokeLambdaFailure

  payload = object ["stack" .= stackName, "event" .= on]

findOutputValue :: Text -> [Output] -> Maybe Text
findOutputValue name =
  view output_outputValue <=< find ((== Just name) . view output_outputKey)

execProcessAction
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => String
  -> [String]
  -> m ()
execProcessAction cmd args = do
  logDebug $ "runProcess" :# ["command" .= (cmd : args)]
  flushLogger

  ec <- runProcess $ proc cmd args
  unless (ec == ExitSuccess) $ throwIO $ ExecFailure ec
