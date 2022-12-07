module Stackctl.Logger
  ( flushLogger
  , pushLogger
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (flushLogger, pushLogStrLn)
import System.Log.FastLogger (toLogStr)

pushLogger :: (MonadIO m, MonadReader env m, HasLogger env) => Text -> m ()
pushLogger msg = do
  logger <- view loggerL
  pushLogStrLn logger $ toLogStr msg
