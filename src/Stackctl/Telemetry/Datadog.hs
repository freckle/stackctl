{-# LANGUAGE UndecidableInstances #-}

module Stackctl.Telemetry.Datadog
  ( DatadogCreds (..)
  , HasDatadogCreds (..)
  , DatadogEvents (..)
  )
where

import Stackctl.Prelude

import qualified Data.ByteString.Char8 as BS8
import Network.Datadog
import Network.Datadog.Types
import Stackctl.AWS.CloudFormation (StackName (..))
import Stackctl.Telemetry
import Stackctl.Telemetry.Tags (HasTelemetryTags (..), TelemetryTags)
import qualified Stackctl.Telemetry.Tags as TelemetryTags

data DatadogCreds
  = DatadogCredsNone
  | DatadogCredsWriteOnly Write
  | DatadogCredsReadWrite ReadWrite

class HasDatadogCreds env where
  datadogCredsL :: Lens' env DatadogCreds

instance HasDatadogCreds DatadogCreds where
  datadogCredsL = id

newtype DatadogEvents m a = DatadogEvents
  { unActualDatadog :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadReader env
    )

instance
  ( MonadIO m
  , MonadReader env m
  , MonadLogger m
  , HasTelemetryTags env
  , HasDatadogCreds env
  )
  => MonadTelemetry (DatadogEvents m)
  where
  recordDeployment deployment = do
    result <- withEnvironment $ \env -> do
      tTags <- view telemetryTagsL

      liftIO
        $ tryAny
        $ createEvent env
        $ deploymentToEventSpec tTags deployment

    case result of
      Nothing -> pure ()
      Just (Left ex) ->
        logWarn
          $ "Unable to record deployment"
          :# ["error" .= displayException ex]
      Just (Right {}) -> pure ()

withEnvironment
  :: (MonadIO m, MonadReader env m, HasDatadogCreds env)
  => (Environment -> m a)
  -> m (Maybe a)
withEnvironment f = traverse f =<< credsToEnvironment =<< view datadogCredsL

credsToEnvironment :: MonadIO m => DatadogCreds -> m (Maybe Environment)
credsToEnvironment = \case
  DatadogCredsNone -> pure Nothing
  DatadogCredsWriteOnly _r -> pure Nothing -- can't create events with this
  DatadogCredsReadWrite rw ->
    fmap Just $ liftIO $ createEnvironment $ readWriteToKeys rw

readWriteToKeys :: ReadWrite -> Keys
readWriteToKeys ReadWrite {..} =
  Keys
    { apiKey = BS8.unpack readWriteApiKey
    , appKey = BS8.unpack readWriteApplicationKey
    }

deploymentToEventSpec :: TelemetryTags -> Deployment -> EventSpec
deploymentToEventSpec tTags Deployment {..} =
  EventSpec
    { eventSpecTitle = "Stackctl Deployment of " <> stackName
    , eventSpecText =
        case deploymentResult of
          DeploymentNoChange -> "Deployment skipped due to no changes."
          DeploymentSucceeded finishedAt ->
            "Deployment succeeded."
              <> "Succeeded at: "
              <> pack (show finishedAt)
          DeploymentFailed finishedAt err ->
            "Deployment failed."
              <> "Failed at: "
              <> pack (show finishedAt)
              <> "Error: "
              <> pack err
    , eventSpecDateHappened = deploymentStartedAt
    , eventSpecPriority =
        case deploymentResult of
          DeploymentNoChange -> LowPriority
          DeploymentSucceeded {} -> NormalPriority
          DeploymentFailed {} -> NormalPriority
    , eventSpecHost = Nothing
    , eventSpecTags = TelemetryTags.toDatadog $ tTags <> eventTags
    , eventSpecAlertType =
        case deploymentResult of
          DeploymentNoChange -> Info
          DeploymentSucceeded {} -> Success
          DeploymentFailed {} -> Error
    , eventSpecSourceType = Just User
    }
 where
  stackName = unStackName deploymentStack
  eventTags =
    TelemetryTags.fromList
      [ ("stack", stackName)
      ,
        ( "conclusion"
        , case deploymentResult of
            DeploymentNoChange -> "no_change"
            DeploymentSucceeded {} -> "succeeded"
            DeploymentFailed {} -> "failed"
        )
      ]
