module Stackctl.CLI
  ( App
  , optionsL
  , AppT
  , runAppT
  ) where

import Stackctl.Prelude

import Blammo.Logging.LogSettings
import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Monad.AWS as AWS
import Control.Monad.AWS.ViaReader as AWS
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, runResourceT)
import qualified Stackctl.AWS.Core as AWS
import Stackctl.AWS.Scope
import Stackctl.AutoSSO
import Stackctl.ColorOption
import Stackctl.Config
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.VerboseOption

data App options = App
  { appLogger :: Logger
  , appConfig :: Config
  , appOptions :: options
  , appAwsScope :: AwsScope
  , appAwsEnv :: AWS.Env
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

newtype AppT app m a = AppT
  { unAppT :: ReaderT app (ResourceT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadResource
    , MonadReader app
    , MonadThrow
    , MonadCatch
    , MonadMask
    )
  deriving (MonadAWS) via (ReaderAWS (AppT app m))
  deriving (MonadLogger) via (WithLogger app (ResourceT m))
  deriving (MonadLoggerIO) via (WithLogger app (ResourceT m))

runAppT
  :: ( MonadMask m
     , MonadUnliftIO m
     , HasColorOption options
     , HasVerboseOption options
     , HasAutoSSOOption options
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

  let logSettings =
        adjustLogSettings
          (options ^. colorOptionL)
          (options ^. verboseOptionL)
          envLogSettings

  withLogger logSettings $ \appLogger -> do
    appAwsEnv <- runWithLogger appLogger $ handleAutoSSO options $ do
      logDebug "Discovering AWS credentials"
      AWS.discover
    appConfig <- runWithLogger appLogger loadConfigOrExit
    appAwsScope <- AWS.runEnvT fetchAwsScope appAwsEnv

    let
      AwsScope {..} = appAwsScope

      context =
        [ "region" .= awsRegion
        , "accountId" .= awsAccountId
        , "accountName" .= awsAccountName
        ]

      appOptions = options
      app = App {..}

    runResourceT
      $ flip runReaderT app
      $ withThreadContext context
      $ unAppT f

adjustLogSettings
  :: Maybe ColorOption -> Verbosity -> LogSettings -> LogSettings
adjustLogSettings mco v =
  maybe id (setLogSettingsColor . unColorOption) mco . verbositySetLogLevels v
