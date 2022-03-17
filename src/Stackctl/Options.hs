module Stackctl.Options
  ( Options(..)
  , HasOptions(..)
  , ColorOption
  , colorHandle
  , optionsParser
  ) where

import Stackctl.Prelude

import Options.Applicative
import Stackctl.FilterOption

data Options = Options
  { oColor :: ColorOption
  , oVerbose :: Bool
  , oDirectory :: FilePath
  , oFilterOption :: Maybe FilterOption
  }

class HasOptions env where
  optionsL :: Lens' env Options

instance HasOptions Options where
  optionsL = id

data ColorOption
  = ColorAuto
  | ColorAlways
  | ColorNever

showColorOption :: ColorOption -> String
showColorOption = \case
  ColorAuto -> "auto"
  ColorAlways -> "always"
  ColorNever -> "never"

readColorOption :: String -> Either String ColorOption
readColorOption = \case
  "auto" -> Right ColorAuto
  "always" -> Right ColorAlways
  "never" -> Right ColorNever
  x -> Left $ "Invalid color option: " <> x

colorHandle :: MonadIO m => Handle -> ColorOption -> m Bool
colorHandle h = \case
  ColorAuto -> hIsTerminalDevice h
  ColorAlways -> pure True
  ColorNever -> pure False

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
    <$> option (eitherReader readColorOption)
      (  long "color"
      <> help "When to colorize output"
      <> metavar "auto|always|never"
      <> value ColorAuto
      <> showDefaultWith showColorOption
      )
    <*> switch
      (  short 'v'
      <> long "verbose"
      <> help "Log verbosely"
      )
    <*> option str
      (  short 'd'
      <> long "directory"
      <> metavar "PATH"
      <> help "Operate on specifications in PATH"
      <> value "."
      <> showDefault
      )
    <*> optional (filterOption "specifications")
