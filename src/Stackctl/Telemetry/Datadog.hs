{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Stackctl.Telemetry.Datadog
  ( DatadogCreds (..)
  , HasDatadogCreds (..)
  , DatadogTags
  , datadogTagsFromList
  , HasDatadogTags (..)
  , DatadogEvents (..)
  )
where

import Stackctl.Prelude

import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Network.Datadog
import Network.Datadog.Types
import Stackctl.Telemetry
import System.Process.Typed
import UnliftIO.Environment (lookupEnv)

data DatadogCreds
  = DatadogCredsNone
  | DatadogCredsWriteOnly Write
  | DatadogCredsReadWrite ReadWrite

class HasDatadogCreds env where
  datadogCredsL :: Lens' env DatadogCreds

instance HasDatadogCreds DatadogCreds where
  datadogCredsL = id

newtype DatadogTags = DatadogTags
  { unDatadogTags :: [Tag]
  }
  deriving newtype (Semigroup, Monoid)

datadogTagsFromList :: [(Text, Text)] -> DatadogTags
datadogTagsFromList = DatadogTags . map (uncurry KeyValueTag)

class HasDatadogTags env where
  datadogTagsL :: Lens' env DatadogTags

instance HasDatadogTags DatadogTags where
  datadogTagsL = id

newtype DatadogEvents m a = DatadogEvents
  { unActualDatadog :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader env
    )

instance
  ( MonadIO m
  , MonadReader env m
  , HasDatadogCreds env
  , HasDatadogTags env
  )
  => MonadTelemetry (DatadogEvents m)
  where
  recordDeployment deployment = do
    ddTags <-
      (<>)
        <$> view datadogTagsL
        <*> getSystemTags

    withEnvironment $ \env -> do
      liftIO $ createEvent env $ deploymentToEventSpec ddTags deployment

withEnvironment
  :: (MonadIO m, MonadReader env m, HasDatadogCreds env)
  => (Environment -> m a)
  -> m ()
withEnvironment f = traverse_ f =<< credsToEnvironment =<< view datadogCredsL

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

deploymentToEventSpec :: DatadogTags -> Deployment -> EventSpec
deploymentToEventSpec ddTags Deployment {..} =
  EventSpec
    { eventSpecTitle = "Stackctl Deployment"
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
    , eventSpecTags = unDatadogTags $ ddTags <> eventTags
    , eventSpecAlertType =
        case deploymentResult of
          DeploymentNoChange -> Info
          DeploymentSucceeded {} -> Success
          DeploymentFailed {} -> Error
    , eventSpecSourceType = Just User
    }
 where
  eventTags =
    datadogTagsFromList
      [
        ( "conclusion"
        , case deploymentResult of
            DeploymentNoChange -> "no_change"
            DeploymentSucceeded {} -> "succeeded"
            DeploymentFailed {} -> "failed"
        )
      ]

getSystemTags :: MonadIO m => m DatadogTags
getSystemTags =
  datadogTagsFromList
    . catMaybes
    <$> sequence
      [ fmap (("user",) . pack) <$> lookupEnv "USER"
      , fmap (("current-directory",) . pack) <$> lookupEnv "PWD"
      , fmap ("hostname",) <$> chompProcessText "hostname" []
      ]

chompProcessText :: MonadIO m => String -> [String] -> m (Maybe Text)
chompProcessText cmd args = do
  (_ec, out, _err) <- readProcess $ proc cmd args
  pure $ Just $ T.dropWhileEnd (== '\n') $ decodeUtf8 $ BSL.toStrict out
