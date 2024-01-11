module Stackctl.CLI
  ( App
  , optionsL
  , AppT
  , runAppT
  ) where

import Stackctl.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Monad.AWS as AWS
import Control.Monad.AWS.ViaReader as AWS
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import Datadog (Datadog, HasDatadog (..), mkWithDatadog)
import qualified Datadog as DD
import qualified Stackctl.AWS.Core as AWS
import Stackctl.AWS.SSM
import Stackctl.AWS.Scope
import Stackctl.AutoSSO
import Stackctl.ColorOption
import Stackctl.Config
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.Telemetry
import Stackctl.TelemetryOption
import Stackctl.VerboseOption

data App options = App
  { appLogger :: Logger
  , appConfig :: Config
  , appOptions :: options
  , appAwsScope :: AwsScope
  , appAwsEnv :: AWS.Env
  , appDatadog :: Datadog
  }

optionsL :: Lens' (App options) options
optionsL = lens appOptions $ \x y -> x {appOptions = y}

instance HasLogger (App options) where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasConfig (App options) where
  configL = lens appConfig $ \x y -> x {appConfig = y}

instance HasAwsScope (App options) where
  awsScopeL = lens appAwsScope $ \x y -> x {appAwsScope = y}

instance AWS.HasEnv (App options) where
  envL = lens appAwsEnv $ \x y -> x {appAwsEnv = y}

instance HasDatadog (App options) where
  datadogL = lens appDatadog $ \x y -> x {appDatadog = y}

instance HasDirectoryOption options => HasDirectoryOption (App options) where
  directoryOptionL = optionsL . directoryOptionL

instance HasFilterOption options => HasFilterOption (App options) where
  filterOptionL = optionsL . filterOptionL

instance HasColorOption options => HasColorOption (App options) where
  colorOptionL = optionsL . colorOptionL

instance HasVerboseOption options => HasVerboseOption (App options) where
  verboseOptionL = optionsL . verboseOptionL

instance HasAutoSSOOption options => HasAutoSSOOption (App options) where
  autoSSOOptionL = optionsL . autoSSOOptionL

instance HasTelemetryOption options => HasTelemetryOption (App options) where
  telemetryOptionL = optionsL . telemetryOptionL

newtype AppT app m a = AppT
  { unAppT :: ReaderT app (LoggingT (ResourceT m)) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadResource
    , MonadReader app
    , MonadLogger
    , MonadThrow
    , MonadCatch
    , MonadMask
    )
  deriving (MonadAWS) via (ReaderAWS (AppT app m))

instance MonadIO m => MonadTelemetry (AppT (App options) m) where
  recordDeployment _ = pure () -- TODO

runAppT
  :: ( MonadMask m
     , MonadUnliftIO m
     , HasColorOption options
     , HasVerboseOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => options
  -> AppT (App options) m a
  -> m a
runAppT options f = do
  envLogSettings <-
    liftIO
      . LoggingEnv.parseWith
      . setLogSettingsConcurrency (Just 1)
      $ defaultLogSettings

  logger <-
    newLogger
      $ adjustLogSettings
        (options ^. colorOptionL)
        (options ^. verboseOptionL)
        envLogSettings

  runResourceT $ runLoggerLoggingT logger $ do
    aws <- runReaderT (handleAutoSSO options AWS.discover) logger

    withDatadog <- case options ^. telemetryOptionL of
      TelemetryDisabled -> pure $ mkWithDatadog Nothing
      TelemetryEnabled -> do
        mcreds <- AWS.runEnvT fetchDatadogCredentials aws
        pure $ mkWithDatadog mcreds

    withDatadog $ \dd -> do
      app <-
        App logger
          <$> loadConfigOrExit
          <*> pure options
          <*> AWS.runEnvT fetchAwsScope aws
          <*> pure aws
          <*> pure dd

      let
        AwsScope {..} = appAwsScope app

        context =
          [ "region" .= awsRegion
          , "accountId" .= awsAccountId
          , "accountName" .= awsAccountName
          ]

      flip runReaderT app
        $ withThreadContext context
        $ unAppT f

adjustLogSettings
  :: Maybe ColorOption -> Verbosity -> LogSettings -> LogSettings
adjustLogSettings mco v =
  maybe id (setLogSettingsColor . unColorOption) mco . verbositySetLogLevels v

fetchDatadogCredentials
  :: (MonadUnliftIO m, MonadAWS m) => m (Maybe DD.Credentials)
fetchDatadogCredentials = do
  mApiKey <- awsGetParameterValue "/datadog-api-key"
  mAppKey <- awsGetParameterValue "/datadog-app-key"

  pure $ case (mApiKey, mAppKey) of
    (Nothing, _) -> Nothing
    (Just apiKey, Nothing) -> Just $ DD.CredentialsWrite apiKey
    (Just apiKey, Just appKey) -> Just $ DD.CredentialsReadWrite apiKey appKey
