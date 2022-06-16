module Stackctl.Version
  ( logVersion
  ) where

import Stackctl.Prelude

import Data.Version
import qualified Paths_stackctl as Pkg
import Prelude (putStrLn)

logVersion :: MonadIO m => m ()
logVersion = liftIO $ putStrLn $ ("Stackctl v" <>) $ showVersion Pkg.version
