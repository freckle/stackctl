module Stackctl.ColorOption
  ( ColorOption
  , HasColorOption(..)
  , colorOption
  , colorHandle
  ) where

import Stackctl.Prelude

import Options.Applicative

data ColorOption
  = ColorAuto
  | ColorAlways
  | ColorNever

class HasColorOption env where
  colorOptionL :: Lens' env ColorOption

instance HasColorOption ColorOption where
  colorOptionL = id

colorOption :: Parser ColorOption
colorOption = option (eitherReader readColorOption) $ mconcat
  [ long "color"
  , help "When to colorize output"
  , metavar "auto|always|never"
  , value ColorAuto
  , showDefaultWith showColorOption
  ]

readColorOption :: String -> Either String ColorOption
readColorOption = \case
  "auto" -> Right ColorAuto
  "always" -> Right ColorAlways
  "never" -> Right ColorNever
  x -> Left $ "Invalid color option: " <> x

showColorOption :: ColorOption -> String
showColorOption = \case
  ColorAuto -> "auto"
  ColorAlways -> "always"
  ColorNever -> "never"

colorHandle :: MonadIO m => Handle -> ColorOption -> m Bool
colorHandle h = \case
  ColorAuto -> hIsTerminalDevice h
  ColorAlways -> pure True
  ColorNever -> pure False
