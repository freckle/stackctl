-- | Facilities for colorizing output
module Stackctl.Colors
  ( Colors (..)
  , getColorsStdout
  , getColorsLogger
  , noColors
  ) where

import Stackctl.Prelude

import Blammo.Logging.Colors
import Blammo.Logging.LogSettings (shouldColorHandle)
import Blammo.Logging.Logger

-- | Return 'Colors' based on options and 'stdout'
getColorsStdout :: (MonadIO m, MonadReader env m, HasLogger env) => m Colors
getColorsStdout = getColorsHandle stdout

-- | Return 'Colors' based on options given 'Handle'
getColorsHandle
  :: (MonadIO m, MonadReader env m, HasLogger env) => Handle -> m Colors
getColorsHandle h = do
  ls <- view $ loggerL . to getLoggerLogSettings
  getColors <$> shouldColorHandle ls h

-- | Return 'Colors' consistent with the ambient 'Logger'
getColorsLogger :: (MonadReader env m, HasLogger env) => m Colors
getColorsLogger = view $ loggerL . to (getColors . getLoggerShouldColor)

noColors :: Colors
noColors = getColors False
