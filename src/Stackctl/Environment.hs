module Stackctl.Environment
  ( Environment(..)
  ) where

import Stackctl.Prelude

import Data.Aeson

newtype Environment = Environment
    { unEnvironment :: Text
    }
    deriving newtype (Eq, Hashable, Show, Display, FromJSON, FromJSONKey)
