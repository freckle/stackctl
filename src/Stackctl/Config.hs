module Stackctl.Config
  ( Config (..)
  , configParameters
  , configTags
  , emptyConfig
  , HasConfig (..)
  , ConfigError (..)
  , loadConfigOrExit
  , loadConfigFromBytes
  , applyConfig
  ) where

import Stackctl.Prelude

import Control.Monad.Except
import Data.Aeson
import Data.Version
import qualified Data.Yaml as Yaml
import Paths_stackctl as Paths
import Stackctl.Config.RequiredVersion
import Stackctl.StackSpecYaml
import UnliftIO.Directory (doesFileExist)

data Config = Config
  { required_version :: Maybe RequiredVersion
  , defaults :: Maybe Defaults
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

configParameters :: Config -> Maybe ParametersYaml
configParameters = parameters <=< defaults

configTags :: Config -> Maybe TagsYaml
configTags = tags <=< defaults

emptyConfig :: Config
emptyConfig = Config Nothing Nothing

data Defaults = Defaults
  { parameters :: Maybe ParametersYaml
  , tags :: Maybe TagsYaml
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)

class HasConfig env where
  configL :: Lens' env Config

instance HasConfig Config where
  configL = id

data ConfigError
  = ConfigInvalidYaml Yaml.ParseException
  | ConfigInvalid (NonEmpty Text)
  | ConfigVersionNotSatisfied RequiredVersion Version
  deriving stock (Show)

configErrorMessage :: ConfigError -> Message
configErrorMessage = \case
  ConfigInvalidYaml ex ->
    "Configuration is not valid Yaml"
      :# ["error" .= Yaml.prettyPrintParseException ex]
  ConfigInvalid errs -> "Invalid configuration" :# ["errors" .= errs]
  ConfigVersionNotSatisfied rv v ->
    "Incompatible Stackctl version" :# ["current" .= v, "required" .= show rv]

loadConfigOrExit :: (MonadIO m, MonadLogger m) => m Config
loadConfigOrExit = either die pure =<< loadConfig
 where
  die e = do
    logError $ configErrorMessage e
    exitFailure

loadConfig :: MonadIO m => m (Either ConfigError Config)
loadConfig =
  runExceptT $ getConfigFile >>= \case
    Nothing -> pure emptyConfig
    Just cf -> loadConfigFrom cf

loadConfigFrom :: (MonadIO m, MonadError ConfigError m) => FilePath -> m Config
loadConfigFrom path = loadConfigFromBytes =<< liftIO (readFileBinary path)

loadConfigFromBytes :: MonadError ConfigError m => ByteString -> m Config
loadConfigFromBytes bs = do
  config <- either (throwError . ConfigInvalidYaml) pure $ Yaml.decodeEither' bs
  config <$ traverse_ checkRequiredVersion (required_version config)
 where
  checkRequiredVersion rv =
    unless (isRequiredVersionSatisfied rv Paths.version)
      $ throwError
      $ ConfigVersionNotSatisfied rv Paths.version

applyConfig :: Config -> StackSpecYaml -> StackSpecYaml
applyConfig config ss@StackSpecYaml {..} =
  ss
    { ssyParameters = configParameters config <> ssyParameters
    , ssyTags = configTags config <> ssyTags
    }

getConfigFile :: MonadIO m => m (Maybe FilePath)
getConfigFile =
  listToMaybe
    <$> filterM
      doesFileExist
      [ ".stackctl" </> "config" <.> "yaml"
      , ".stackctl" </> "config" <.> "yml"
      , ".stackctl" <.> "yaml"
      , ".stackctl" <.> "yml"
      ]
