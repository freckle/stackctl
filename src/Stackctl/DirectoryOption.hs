module Stackctl.DirectoryOption
  ( DirectoryOption (..)
  , defaultDirectoryOption
  , HasDirectoryOption (..)
  , envDirectoryOption
  , directoryOption
  ) where

import Stackctl.Prelude

import Data.Semigroup (Last (..))
import qualified Env
import Options.Applicative

newtype DirectoryOption = DirectoryOption
  { unDirectoryOption :: FilePath
  }
  deriving newtype (IsString)
  deriving (Semigroup) via Last DirectoryOption

defaultDirectoryOption :: DirectoryOption
defaultDirectoryOption = "."

class HasDirectoryOption env where
  directoryOptionL :: Lens' env DirectoryOption

instance HasDirectoryOption DirectoryOption where
  directoryOptionL = id

envDirectoryOption :: Env.Parser Env.Error DirectoryOption
envDirectoryOption =
  Env.var (Env.str <=< Env.nonempty) "DIRECTORY"
    $ Env.help "Operate on specifications in this directory"

directoryOption :: Parser DirectoryOption
directoryOption =
  option str
    $ mconcat
      [ short 'd'
      , long "directory"
      , metavar "PATH"
      , help "Operate on specifications in PATH"
      , action "directory"
      ]
