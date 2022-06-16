-- | Facilities for colorizing output
module Stackctl.Colors
  ( Colors(..)
  , HasColorOption
  , getColorsStdout
  , getColorsStderr
  , getColorsHandle
  , noColors
  ) where

import Stackctl.Prelude2

import Stackctl.ColorOption (HasColorOption(..), colorHandle)

data Colors = Colors
  { cyan :: Utf8Builder -> Utf8Builder
  , magenta :: Utf8Builder -> Utf8Builder
  , blue :: Utf8Builder -> Utf8Builder
  , yellow :: Utf8Builder -> Utf8Builder
  , green :: Utf8Builder -> Utf8Builder
  , red :: Utf8Builder -> Utf8Builder
  }

colors :: Colors
colors = Colors
  { cyan = esc "36"
  , magenta = esc "35"
  , blue = esc "34"
  , yellow = esc "33"
  , green = esc "32"
  , red = esc "31"
  }
  where esc code x = "\ESC[0;" <> code <> "m" <> x <> "\ESC[0m"

noColors :: Colors
noColors = Colors
  { cyan = id
  , magenta = id
  , blue = id
  , yellow = id
  , green = id
  , red = id
  }

-- | Return 'Colors' based on if the current 'LogFunc' is colorizing
--
-- Use this if colorizing in a log message. This should be equivalent
-- @'getColorsHandle' 'stderr'@, but doing it from the 'LogFunc' guarantees
-- consistency.
--

-- | Return 'Colors' based on options and 'stdout'
getColorsStdout
  :: (MonadIO m, MonadReader env m, HasColorOption env) => m Colors
getColorsStdout = getColorsHandle stdout

getColorsStderr
  :: (MonadIO m, MonadReader env m, HasColorOption env) => m Colors
getColorsStderr = getColorsHandle stderr

-- | Return 'Colors' based on options given 'Handle'
getColorsHandle
  :: (MonadIO m, MonadReader env m, HasColorOption env) => Handle -> m Colors
getColorsHandle h = do
  colorOption <- view colorOptionL
  c <- colorHandle h colorOption
  pure $ if c then colors else noColors
