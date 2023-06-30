module Stackctl.VerboseOption
  ( Verbosity
  , verbositySetLogLevels
  , HasVerboseOption (..)
  , verboseOption
  ) where

import Stackctl.Prelude

import Blammo.Logging.LogSettings.LogLevels
import Options.Applicative

newtype Verbosity = Verbosity [()]
  deriving newtype (Semigroup, Monoid)

verbositySetLogLevels :: Verbosity -> (LogSettings -> LogSettings)
verbositySetLogLevels (Verbosity bs) = case bs of
  [] -> id
  [_] -> setLogSettingsLevels v
  [_, _] -> setLogSettingsLevels vv
  _ -> setLogSettingsLevels vvv
 where
  v = newLogLevels LevelDebug [("Amazonka", LevelInfo)]
  vv = newLogLevels LevelDebug []
  vvv = newLogLevels (LevelOther "trace") []

class HasVerboseOption env where
  verboseOptionL :: Lens' env Verbosity

instance HasVerboseOption Verbosity where
  verboseOptionL = id

verboseOption :: Parser Verbosity
verboseOption =
  fmap Verbosity
    $ many
    $ flag' ()
    $ mconcat
      [ short 'v'
      , long "verbose"
      , help "Increase verbosity (can be passed multiple times)"
      ]
