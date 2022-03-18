module Stackctl.DirectoryOption
  ( HasDirectoryOption(..)
  , directoryOption
  ) where

import Stackctl.Prelude

import Options.Applicative

class HasDirectoryOption env where
  directoryOptionL :: Lens' env FilePath

instance HasDirectoryOption FilePath where
  directoryOptionL = id

directoryOption :: Parser FilePath
directoryOption = option str $ mconcat
  [ short 'd'
  , long "directory"
  , metavar "PATH"
  , help "Operate on specifications in PATH"
  , value "."
  , showDefault
  ]
