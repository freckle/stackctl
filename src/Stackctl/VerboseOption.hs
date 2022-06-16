module Stackctl.VerboseOption
  ( Verbosity
  , verbositySetLogLevels
  , HasVerboseOption(..)
  , verboseOption
  ) where

import Stackctl.Prelude2

import Blammo.Logging.LogSettings.LogLevels
import Options.Applicative

newtype Verbosity = Verbosity Bool

verbositySetLogLevels :: Verbosity -> (LogSettings -> LogSettings)
verbositySetLogLevels (Verbosity b) =
  if b then setLogSettingsLevels (newLogLevels LevelDebug []) else id

class HasVerboseOption env where
  verboseOptionL :: Lens' env Verbosity

instance HasVerboseOption Verbosity where
  verboseOptionL = id

verboseOption :: Parser Verbosity
verboseOption = fmap Verbosity $ switch $ mconcat
  [short 'v', long "verbose", help "Log verbosely"]
