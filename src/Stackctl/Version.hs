module Stackctl.Version
  ( logVersion
  ) where

import Stackctl.Prelude

import Data.Version
import qualified Paths_stackctl as Pkg

logVersion :: (MonadIO m, MonadReader env m, HasLogFunc env) => m ()
logVersion = logInfo $ fromString $ showVersion Pkg.version
