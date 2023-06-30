module Stackctl.ColorOption
  ( ColorOption (..)
  , HasColorOption (..)
  , colorOption
  ) where

import Stackctl.Prelude

import Blammo.Logging.LogSettings
import Data.Semigroup (Last (..))
import Options.Applicative

newtype ColorOption = ColorOption
  { unColorOption :: LogColor
  }
  deriving (Semigroup) via Last ColorOption

class HasColorOption env where
  colorOptionL :: Lens' env (Maybe ColorOption)

colorOption :: Parser ColorOption
colorOption =
  option (eitherReader $ fmap ColorOption . readLogColor)
    $ mconcat
      [long "color", help "When to colorize output", metavar "auto|always|never"]
