module Stackctl.TelemetryOption
  ( TelemetryOption (..)
  , HasTelemetryOption (..)
  , envTelemetryOption
  , telemetryOption
  ) where

import Stackctl.Prelude

import Data.Semigroup (Last (..))
import qualified Env
import Options.Applicative

data TelemetryOption
  = TelemetryDisabled
  | TelemetryEnabled
  deriving (Semigroup) via Last TelemetryOption

class HasTelemetryOption env where
  telemetryOptionL :: Lens' env TelemetryOption

envTelemetryOption :: Env.Parser Env.Error TelemetryOption
envTelemetryOption = pure TelemetryEnabled -- TODO

telemetryOption :: Parser TelemetryOption
telemetryOption = pure TelemetryEnabled -- TODO
