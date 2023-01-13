module Stackctl.Options
  ( Options
  , envParser
  , optionsParser
  ) where

import Stackctl.Prelude

import Data.Semigroup.Generic
import qualified Env
import Options.Applicative
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.VerboseOption

data Options = Options
  { oDirectory :: Maybe DirectoryOption
  , oFilter :: Maybe FilterOption
  , oColor :: Maybe ColorOption
  , oVerbose :: Verbosity
  }
  deriving stock Generic
  deriving Semigroup via GenericSemigroupMonoid Options

directoryL :: Lens' Options (Maybe DirectoryOption)
directoryL = lens oDirectory $ \x y -> x { oDirectory = y }

filterL :: Lens' Options (Maybe FilterOption)
filterL = lens oFilter $ \x y -> x { oFilter = y }

instance HasDirectoryOption Options where
  directoryOptionL = directoryL . maybeLens defaultDirectoryOption

instance HasFilterOption Options where
  filterOptionL = filterL . maybeLens defaultFilterOption

instance HasColorOption Options where
  colorOptionL = lens oColor $ \x y -> x { oColor = y }

instance HasVerboseOption Options where
  verboseOptionL = lens oVerbose $ \x y -> x { oVerbose = y }

-- brittany-disable-next-binding

envParser :: Env.Parser Env.Error Options
envParser = Env.prefixed "STACKCTL_" $ Options
  <$> optional envDirectoryOption
  <*> optional (envFilterOption "specifications")
  <*> pure mempty -- use LOG_COLOR
  <*> pure mempty -- use LOG_LEVEL

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
  <$> optional directoryOption
  <*> optional (filterOption "specifications")
  <*> optional colorOption
  <*> verboseOption
