module Stackctl.Version
  ( logVersion
  ) where

import Stackctl.Prelude
import Prelude (putStrLn)

import Data.Version
import qualified Paths_stackctl as Pkg

logVersion :: MonadIO m => m ()
logVersion = liftIO $ putStrLn $ ("Stackctl v" <>) $ showVersion Pkg.version
