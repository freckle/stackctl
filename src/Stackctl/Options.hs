module Stackctl.Options
  ( Options(..)
  , HasOptions(..)
  , ColorOption
  , colorHandle
  , optionsParser
  ) where

import Stackctl.Prelude

import Stackctl.Environment
import qualified Stackctl.Paths as Paths
import Options.Applicative

data Options = Options
  { oColor :: ColorOption
  , oVerbose :: Bool
  , oEnvironment :: Maybe Environment
  , oResource :: Maybe Text
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
    <*> optional (Environment <$> strOption
      (  short 'e'
      <> long "environment"
      <> metavar "ENVIRONMENT"
      <> help "Set Environment Parameter, for namespacing purposes"
      ))
    <*> optional (strOption
      (  short 'r'
      <> long "resource"
      <> metavar "NAME"
      <> help ("Only process " <> Paths.platformYaml "NAME")
      ))
