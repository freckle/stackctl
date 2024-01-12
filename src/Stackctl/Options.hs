module Stackctl.Options
  ( Options
  , envParser
  , optionsParser
  ) where

import Stackctl.Prelude

import Data.Semigroup.Generic
import Data.Version
import qualified Env
import Options.Applicative
import qualified Paths_stackctl as Pkg
import Stackctl.AutoSSO
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.Telemetry.Tags (HasTelemetryTags (..), TelemetryTags)
import qualified Stackctl.Telemetry.Tags as TelemetryTags
import Stackctl.TelemetryOption
import Stackctl.VerboseOption

data Options = Options
  { oDirectory :: Maybe DirectoryOption
  , oFilter :: Maybe FilterOption
  , oColor :: Maybe ColorOption
  , oVerbose :: Verbosity
  , oAutoSSO :: Maybe AutoSSOOption
  , oTelemetry :: TelemetryOption
  , oTelemetryTags :: TelemetryTags
  }
  deriving stock (Generic)
  deriving (Semigroup) via GenericSemigroupMonoid Options

directoryL :: Lens' Options (Maybe DirectoryOption)
directoryL = lens oDirectory $ \x y -> x {oDirectory = y}

filterL :: Lens' Options (Maybe FilterOption)
filterL = lens oFilter $ \x y -> x {oFilter = y}

autoSSOL :: Lens' Options (Maybe AutoSSOOption)
autoSSOL = lens oAutoSSO $ \x y -> x {oAutoSSO = y}

instance HasDirectoryOption Options where
  directoryOptionL = directoryL . maybeLens defaultDirectoryOption

instance HasFilterOption Options where
  filterOptionL = filterL . maybeLens defaultFilterOption

instance HasColorOption Options where
  colorOptionL = lens oColor $ \x y -> x {oColor = y}

instance HasVerboseOption Options where
  verboseOptionL = lens oVerbose $ \x y -> x {oVerbose = y}

instance HasAutoSSOOption Options where
  autoSSOOptionL = autoSSOL . maybeLens defaultAutoSSOOption

instance HasTelemetryOption Options where
  telemetryOptionL = lens oTelemetry $ \x y -> x {oTelemetry = y}

instance HasTelemetryTags Options where
  telemetryTagsL = lens oTelemetryTags $ \x y -> x {oTelemetryTags = y}

envParser :: Env.Parser Env.Error Options
envParser =
  Env.prefixed "STACKCTL_"
    $ Options
    <$> optional envDirectoryOption
    <*> optional (envFilterOption "specifications")
    <*> pure mempty -- use LOG_COLOR
    <*> pure mempty -- use LOG_LEVEL
    <*> optional envAutoSSOOption
    <*> envTelemetryOption
    <*> pure defaultTelemetryTags

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional directoryOption
    <*> optional (filterOption "specifications")
    <*> optional colorOption
    <*> verboseOption
    <*> optional autoSSOOption
    <*> telemetryOption
    <*> pure defaultTelemetryTags

defaultTelemetryTags :: TelemetryTags
defaultTelemetryTags =
  TelemetryTags.fromList
    [ ("tool-name", "Stackctl")
    , ("tool-version", pack $ showVersion Pkg.version)
    ]
