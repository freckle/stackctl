module Stackctl.Version
  ( logVersion
  ) where

import Stackctl.Prelude

import Data.Version
import qualified Paths_stackctl as Pkg

logVersion :: MonadLogger m => m ()
logVersion = logInfo $ fromString $ showVersion Pkg.version
