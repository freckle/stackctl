module Stackctl.Paths
  ( platform
  , platformYaml
  , platformSpecs
  ) where

import Stackctl.Prelude

platform :: FilePath
platform = ".platform"

platformYaml :: String -> FilePath
platformYaml name = platform </> name <.> "yaml"

platformSpecs :: FilePath
platformSpecs = platform </> "specs"
