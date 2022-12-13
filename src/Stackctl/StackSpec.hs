module Stackctl.StackSpec
  ( StackSpec
  , stackSpecSpecPath
  , stackSpecSpecBody
  , stackSpecStackName
  , stackSpecStackDescription
  , stackSpecActions
  , stackSpecParameters
  , stackSpecCapabilities
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
import Stackctl.Sort
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml
import System.FilePath (takeExtension)
import UnliftIO.Directory (createDirectoryIfMissing)

data StackSpec = StackSpec
  { ssSpecRoot :: FilePath
  , ssSpecPath :: StackSpecPath
  , ssSpecBody :: StackSpecYaml
  }

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

stackSpecTemplateFile :: StackSpec -> StackTemplate
stackSpecTemplateFile StackSpec {..} =
  StackTemplate $ ssSpecRoot </> "templates" </> ssyTemplate ssSpecBody

stackSpecParameters :: StackSpec -> [Parameter]
stackSpecParameters =
  maybe [] (map unParameterYaml) . ssyParameters . ssSpecBody

stackSpecCapabilities :: StackSpec -> [Capability]
stackSpecCapabilities = fromMaybe [] . ssyCapabilities . ssSpecBody

stackSpecTags :: StackSpec -> [Tag]
stackSpecTags = maybe [] (map unTagYaml) . ssyTags . ssSpecBody

buildStackSpec :: FilePath -> StackSpecPath -> StackSpecYaml -> StackSpec
buildStackSpec = StackSpec

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

writeStackSpec
  :: MonadUnliftIO m
  => FilePath -- ^ Parent directory
  -> StackSpec
  -> TemplateBody
  -> m ()
writeStackSpec parent stackSpec@StackSpec {..} templateBody = do
  writeTemplateBody templatePath templateBody
  createDirectoryIfMissing True $ takeDirectory specPath
  liftIO $ Yaml.encodeFile specPath ssSpecBody
 where
  templatePath = unStackTemplate $ stackSpecTemplateFile stackSpec
  specPath = parent </> stackSpecPathFilePath ssSpecPath

readStackSpec :: MonadIO m => FilePath -> StackSpecPath -> m StackSpec
readStackSpec dir specPath = do
  specBody <- liftIO $ either err pure =<< Yaml.decodeFileEither path

  pure StackSpec
    { ssSpecRoot = dir
    , ssSpecPath = specPath
    , ssSpecBody = specBody
    }
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
  (stackSpecTemplateFile spec)
  (nubOrdOn (^. parameter_parameterKey) $ parameters <> stackSpecParameters spec
  )
  (stackSpecCapabilities spec)
  (stackSpecTags spec)

sortStackSpecs :: [StackSpec] -> [StackSpec]
sortStackSpecs = sortByDependencies stackSpecStackName stackSpecDepends
