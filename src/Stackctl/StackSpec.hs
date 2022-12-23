module Stackctl.StackSpec
  ( StackSpec
  , stackSpecSpecPath
  , stackSpecSpecBody
  , stackSpecStackName
  , stackSpecStackDescription
  , stackSpecActions
  , stackSpecParameters
  , stackSpecCapabilities
  , stackSpecStackFile
  , stackSpecTemplateFile
  , stackSpecTags
  , buildStackSpec
  , TemplateBody
  , templateBodyFromValue
  , writeStackSpec
  , readStackSpec
  , createChangeSet
  , sortStackSpecs
  ) where

import Stackctl.Prelude

import qualified CfnFlip
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.List.Extra (nubOrdOn)
import qualified Data.Yaml as Yaml
import Stackctl.Action
import Stackctl.AWS
import Stackctl.Config (HasConfig(..), applyConfig)
import Stackctl.Sort
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml
import qualified System.FilePath as FilePath
import System.FilePath (takeExtension)
import UnliftIO.Directory (createDirectoryIfMissing)

data StackSpec = StackSpec
  { ssSpecRoot :: FilePath
  , ssSpecPath :: StackSpecPath
  , ssSpecBody :: StackSpecYaml
  }

stackSpecSpecRoot :: StackSpec -> FilePath
stackSpecSpecRoot = ssSpecRoot

stackSpecSpecPath :: StackSpec -> StackSpecPath
stackSpecSpecPath = ssSpecPath

stackSpecSpecBody :: StackSpec -> StackSpecYaml
stackSpecSpecBody = ssSpecBody

stackSpecStackName :: StackSpec -> StackName
stackSpecStackName = stackSpecPathStackName . ssSpecPath

stackSpecStackDescription :: StackSpec -> Maybe StackDescription
stackSpecStackDescription = ssyDescription . ssSpecBody

stackSpecDepends :: StackSpec -> [StackName]
stackSpecDepends = fromMaybe [] . ssyDepends . ssSpecBody

stackSpecActions :: StackSpec -> [Action]
stackSpecActions = fromMaybe [] . ssyActions . ssSpecBody

-- | Relative path @stacks/...@
stackSpecStackFile :: StackSpec -> FilePath
stackSpecStackFile = stackSpecPathFilePath . ssSpecPath

-- | Relative path @templates/...@
stackSpecTemplateFile :: StackSpec -> FilePath
stackSpecTemplateFile = ("templates" </>) . ssyTemplate . ssSpecBody

stackSpecTemplate :: StackSpec -> StackTemplate
stackSpecTemplate spec =
  StackTemplate
    $ FilePath.normalise
    $ ssSpecRoot spec
    </> stackSpecTemplateFile spec

stackSpecParameters :: StackSpec -> [Parameter]
stackSpecParameters =
  maybe [] (map unParameterYaml . unParametersYaml) . ssyParameters . ssSpecBody

stackSpecCapabilities :: StackSpec -> [Capability]
stackSpecCapabilities = fromMaybe [] . ssyCapabilities . ssSpecBody

stackSpecTags :: StackSpec -> [Tag]
stackSpecTags = maybe [] (map unTagYaml . unTagsYaml) . ssyTags . ssSpecBody

buildStackSpec
  :: (MonadReader env m, HasConfig env)
  => FilePath
  -> StackSpecPath
  -> StackSpecYaml
  -> m StackSpec
buildStackSpec dir specPath specBody = do
  config <- view configL
  pure StackSpec
    { ssSpecRoot = dir
    , ssSpecPath = specPath
    , ssSpecBody = applyConfig config specBody
    }

data TemplateBody
  = TemplateText Text
  | TemplateJson Value

newtype UnexpectedTemplateJson = UnexpectedTemplateJson
  { _unexpectedTemplateJsonExtension :: String
  }
  deriving stock Show

instance Exception UnexpectedTemplateJson where
  displayException (UnexpectedTemplateJson ext) =
    "TemplateJson must be written to .yaml or .json, encountered "
      <> ext
      <> ". To write to an arbitrary path, use TemplateText."

templateBodyFromValue :: Value -> TemplateBody
templateBodyFromValue = \case
  String x -> TemplateText x
  v -> TemplateJson v

writeTemplateBody :: MonadUnliftIO m => FilePath -> TemplateBody -> m ()
writeTemplateBody path body = do
  createDirectoryIfMissing True dir

  case (body, ext) of
    (TemplateText t, _) -> writeFileUtf8 path t
    (TemplateJson v, ".yaml") -> CfnFlip.jsonToYamlFile path v
    (TemplateJson v, ".json") -> writeFileBinary path $ BSL.toStrict $ encode v
    (TemplateJson _, _) -> throwIO $ UnexpectedTemplateJson ext
 where
  dir = takeDirectory path
  ext = takeExtension path

writeStackSpec :: MonadUnliftIO m => StackSpec -> TemplateBody -> m ()
writeStackSpec stackSpec templateBody = do
  writeTemplateBody templatePath templateBody
  createDirectoryIfMissing True $ takeDirectory specPath
  liftIO $ Yaml.encodeFile specPath $ stackSpecSpecBody stackSpec
 where
  templatePath = unStackTemplate $ stackSpecTemplate stackSpec
  specPath =
    FilePath.normalise
      $ stackSpecSpecRoot stackSpec
      </> stackSpecStackFile stackSpec

readStackSpec
  :: (MonadIO m, MonadReader env m, HasConfig env)
  => FilePath
  -> StackSpecPath
  -> m StackSpec
readStackSpec dir specPath = do
  specBody <- liftIO $ either err pure =<< Yaml.decodeFileEither path
  buildStackSpec dir specPath specBody
 where
  path = dir </> stackSpecPathFilePath specPath
  err e =
    throwString $ path <> " is invalid: " <> Yaml.prettyPrintParseException e

-- | Create a Change Set between a Stack Specification and deployed state
createChangeSet
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsEnv env
     )
  => StackSpec
  -> [Parameter]
  -> m (Either Text (Maybe ChangeSet))
createChangeSet spec parameters = awsCloudFormationCreateChangeSet
  (stackSpecStackName spec)
  (stackSpecStackDescription spec)
  (stackSpecTemplate spec)
  (nubOrdOn (^. parameter_parameterKey) $ parameters <> stackSpecParameters spec
  )
  (stackSpecCapabilities spec)
  (stackSpecTags spec)

sortStackSpecs :: [StackSpec] -> [StackSpec]
sortStackSpecs = sortByDependencies stackSpecStackName stackSpecDepends
