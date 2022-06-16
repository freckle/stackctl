-- | Facilities for colorizing output
module Stackctl.Colors
  ( Colors(..)
  , HasColorOption
  , getColorsStdout
  , noColors
  ) where

import Stackctl.Prelude

import Blammo.Logging.Colors
import Stackctl.ColorOption (HasColorOption(..), colorHandle)

-- | Return 'Colors' based on options and 'stdout'
getColorsStdout
  :: (MonadIO m, MonadReader env m, HasColorOption env) => m Colors
getColorsStdout = getColorsHandle stdout

-- | Return 'Colors' based on options given 'Handle'
getColorsHandle
  :: (MonadIO m, MonadReader env m, HasColorOption env) => Handle -> m Colors
getColorsHandle h = do
  colorOption <- view colorOptionL
  c <- colorHandle h colorOption
  pure $ getColors c
