module Stackctl.ColorOption
  ( ColorOption(..)
  , defaultColorOption
  , HasColorOption(..)
  , colorOption
  , colorHandle
  ) where

import Stackctl.Prelude

import Blammo.Logging.LogSettings
import Data.Semigroup (Last(..))
import Options.Applicative

newtype ColorOption = ColorOption
  { unColorOption :: LogColor
  }
  deriving Semigroup via Last ColorOption

defaultColorOption :: ColorOption
defaultColorOption = ColorOption LogColorAuto

class HasColorOption env where
  colorOptionL :: Lens' env ColorOption

instance HasColorOption ColorOption where
  colorOptionL = id

colorOption :: Parser ColorOption
colorOption = option (eitherReader $ fmap ColorOption . readLogColor) $ mconcat
  [ long "color"
  , help "When to colorize output"
  , metavar "auto|always|never"
  , value defaultColorOption
  , showDefaultWith showColorOption
  ]

showColorOption :: ColorOption -> String
showColorOption co = case unColorOption co of
  LogColorAuto -> "auto"
  LogColorAlways -> "always"
  LogColorNever -> "never"

colorHandle :: MonadIO m => Handle -> ColorOption -> m Bool
colorHandle h co = shouldColorHandle settings h
  where settings = setLogSettingsColor (unColorOption co) defaultLogSettings
