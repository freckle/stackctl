module Stackctl.ColorOption
  ( LogColor(..)
  , HasColorOption(..)
  , colorOption
  , colorHandle
  ) where

import Stackctl.Prelude2

import Blammo.Logging.LogSettings
import Options.Applicative

class HasColorOption env where
  colorOptionL :: Lens' env LogColor

instance HasColorOption LogColor where
  colorOptionL = id

colorOption :: Parser LogColor
colorOption = option (eitherReader readLogColor) $ mconcat
  [ long "color"
  , help "When to colorize output"
  , metavar "auto|always|never"
  , value LogColorAuto
  , showDefaultWith showLogColor
  ]

showLogColor :: LogColor -> String
showLogColor = \case
  LogColorAuto -> "auto"
  LogColorAlways -> "always"
  LogColorNever -> "never"

colorHandle :: MonadIO m => Handle -> LogColor -> m Bool
colorHandle h lc = shouldColorHandle settings h
  where settings = setLogSettingsColor lc defaultLogSettings
