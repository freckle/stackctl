module Stackctl.CLI
  ( App
  , optionsL
  , runApp
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.VerboseOption

data App options = App
  { appLogFunc :: LogFunc
  , appProcessContext :: ProcessContext
  , appResourceMap :: ResourceMap
  , appAwsEnv :: AwsEnv
  , appOptions :: options
  }

optionsL :: Lens' (App options) options
optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasLogFunc (App options) where
  logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext (App options) where
  processContextL =
    lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasResourceMap (App options) where
  resourceMapL = lens appResourceMap $ \x y -> x { appResourceMap = y }

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

runApp
  :: (MonadUnliftIO m, HasColorOption options, HasVerboseOption options)
  => options
  -> RIO (App options) a
  -> m a
runApp options f = withApp options $ \app -> runRIO app f

withApp
  :: (MonadUnliftIO m, HasColorOption options, HasVerboseOption options)
  => options
  -> (App options -> m a)
  -> m a
withApp options f = do
  color <- colorHandle stderr $ options ^. colorOptionL
  logOptions <- setLogUseLoc False . setLogUseColor color <$> logOptionsHandle
    stderr
    (options ^. verboseOptionL)

  withLogFunc logOptions $ \lf -> withResourceMap $ \rm -> do
    app <-
      App lf
      <$> mkDefaultProcessContext
      <*> pure rm
      <*> awsEnvDiscover
      <*> pure options
    f app
