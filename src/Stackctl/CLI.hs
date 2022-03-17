module Stackctl.CLI
  ( App
  , runApp
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.Options

data App = App
  { appLogFunc :: LogFunc
  , appProcessContext :: ProcessContext
  , appResourceMap :: ResourceMap
  , appAwsEnv :: AwsEnv
  , appOptions :: Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
  processContextL =
    lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasResourceMap App where
  resourceMapL = lens appResourceMap $ \x y -> x { appResourceMap = y }

instance HasAwsEnv App where
  awsEnvL = lens appAwsEnv $ \x y -> x { appAwsEnv = y }

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x { appOptions = y }

runApp :: MonadUnliftIO m => Options -> RIO App a -> m a
runApp options f = withApp options $ \app -> runRIO app f

withApp :: MonadUnliftIO m => Options -> (App -> m a) -> m a
withApp options@Options {..} f = do
  color <- colorHandle stderr oColor
  logOptions <-
    setLogUseLoc False
    . setLogUseColor color
    <$> logOptionsHandle stderr oVerbose

  withLogFunc logOptions $ \lf -> withResourceMap $ \rm -> do
    app <-
      App lf
      <$> mkDefaultProcessContext
      <*> pure rm
      <*> awsEnvDiscover
      <*> pure options
    f app
