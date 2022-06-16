module Stackctl.VerboseOption
  ( HasVerboseOption(..)
  , verboseOption
  ) where

import Stackctl.Prelude2

import Options.Applicative

class HasVerboseOption env where
  verboseOptionL :: Lens' env Bool

instance HasVerboseOption Bool where
  verboseOptionL = id

verboseOption :: Parser Bool
verboseOption =
  switch $ mconcat [short 'v', long "verbose", help "Log verbosely"]
