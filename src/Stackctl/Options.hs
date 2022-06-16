module Stackctl.Options
  ( Options(..)
  , optionsParser
  ) where

import Stackctl.Prelude

import Options.Applicative
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.VerboseOption

data Options = Options
  { oDirectory :: FilePath
  , oFilterOption :: FilterOption
  , oColor :: LogColor
  , oVerbose :: Verbosity
  }

instance HasDirectoryOption Options where
  directoryOptionL = lens oDirectory $ \x y -> x { oDirectory = y }

instance HasColorOption Options where
  colorOptionL = lens oColor $ \x y -> x { oColor = y }

instance HasFilterOption Options where
  filterOptionL = lens oFilterOption $ \x y -> x { oFilterOption = y }

instance HasVerboseOption Options where
  verboseOptionL = lens oVerbose $ \x y -> x { oVerbose = y }

-- brittany-disable-next-binding

optionsParser :: Parser Options
optionsParser = Options
  <$> directoryOption
  <*> filterOption "specifications"
  <*> colorOption
  <*> verboseOption
