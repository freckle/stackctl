module Stackctl.TelemetryOption
  ( TelemetryOption (..)
  , HasTelemetryOption (..)
  ) where

import Stackctl.Prelude

data TelemetryOption
  = TelemetryDisabled
  | TelemetryEnabled

class HasTelemetryOption env where
  telemetryOptionL :: Lens' env TelemetryOption
