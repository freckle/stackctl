module Stackctl.CLI
  ( App
  , optionsL
  , AppT
  , runAppT
  ) where

import Stackctl.Prelude

import qualified Blammo.Logging.LogSettings.Env as LoggingEnv
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Stackctl.AWS
import Stackctl.AWS.Scope
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
  , appAwsEnv :: AwsEnv
  }

optionsL :: Lens' (App options) options
optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasLogger (App options) where
  loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasConfig (App options) where
  configL = lens appConfig $ \x y -> x { appConfig = y }

instance HasAwsScope (App options) where
  awsScopeL = lens appAwsScope $ \x y -> x { appAwsScope = y }

instance HasAwsEnv (App options) where
  awsEnvL = lens appAwsEnv $ \x y -> x { appAwsEnv = y }

instance HasDirectoryOption options => HasDirectoryOption (App options) where
  directoryOptionL = optionsL . directoryOptionL

instance HasFilterOption options => HasFilterOption (App options) where
  filterOptionL = optionsL . filterOptionL

instance HasColorOption options => HasColorOption (App options) where
  colorOptionL = optionsL . colorOptionL

instance HasVerboseOption options => HasVerboseOption (App options) where
  verboseOptionL = optionsL . verboseOptionL

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

runAppT
  :: ( MonadMask m
     , MonadUnliftIO m
     , HasColorOption options
     , HasVerboseOption options
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

  logger <- newLogger $ adjustLogSettings
    (options ^. colorOptionL . to unColorOption)
    (options ^. verboseOptionL)
    envLogSettings

  app <- runResourceT $ runLoggerLoggingT logger $ do
    aws <- awsEnvDiscover

    App logger
      <$> loadConfigOrExit
      <*> pure options
      <*> runReaderT fetchAwsScope aws
      <*> pure aws

  let
    AwsScope {..} = appAwsScope app

    context =
      [ "region" .= awsRegion
      , "accountId" .= awsAccountId
      , "accountName" .= awsAccountName
      ]

  runResourceT
    $ runLoggerLoggingT app
    $ flip runReaderT app
    $ withThreadContext context
    $ unAppT f

adjustLogSettings :: LogColor -> Verbosity -> LogSettings -> LogSettings
adjustLogSettings lc v = setLogSettingsColor lc . verbositySetLogLevels v
