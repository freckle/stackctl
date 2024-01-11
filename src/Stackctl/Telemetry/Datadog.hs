{-# LANGUAGE UndecidableInstances #-}

module Stackctl.Telemetry.Datadog
  ( DatadogCreds (..)
  , HasDatadogCreds (..)
  , ViaDatadog (..)
  )
where

import Stackctl.Prelude

import Data.ByteString.Char8 as BS8
import Network.Datadog
import Network.Datadog.Types
import Stackctl.Telemetry

data DatadogCreds
  = DatadogCredsNone
  | DatadogCredsWrite Write
  | DatadogCredsReadWrite ReadWrite

class HasDatadogCreds env where
  datadogCredsL :: Lens' env DatadogCreds

instance HasDatadogCreds DatadogCreds where
  datadogCredsL = id

newtype ViaDatadog m a = ViaDatadog
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
  (MonadIO m, MonadReader env m, HasDatadogCreds env)
  => MonadTelemetry (ViaDatadog m)
  where
  recordDeployment deployment = withEnvironment $ \env -> do
    void $ liftIO $ createEvent env $ deploymentToEventSpec deployment

withEnvironment
  :: (MonadIO m, MonadReader env m, HasDatadogCreds env)
  => (Environment -> m a)
  -> m ()
withEnvironment f = traverse_ f =<< credsToEnvironment =<< view datadogCredsL

credsToEnvironment :: MonadIO m => DatadogCreds -> m (Maybe Environment)
credsToEnvironment = \case
  DatadogCredsNone -> pure Nothing
  DatadogCredsWrite _r -> pure Nothing -- unsupported
  DatadogCredsReadWrite rw ->
    fmap Just $ liftIO $ createEnvironment $ readWriteToKeys rw

readWriteToKeys :: ReadWrite -> Keys
readWriteToKeys ReadWrite {..} =
  Keys
    { apiKey = BS8.unpack readWriteApiKey
    , appKey = BS8.unpack readWriteApplicationKey
    }

deploymentToEventSpec :: Deployment -> EventSpec
deploymentToEventSpec = undefined
