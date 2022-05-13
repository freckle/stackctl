module Stackctl.StackSpec
  ( StackSpec
  , stackSpecSpecPath
  , stackSpecSpecBody
  , stackSpecStackName
  , stackSpecParameters
  , stackSpecCapabilities
  , stackSpecTags
  , buildStackSpec
  , logStackSpec
  , writeStackSpec
  , readStackSpec
  , createChangeSet
  , sortStackSpecs
  ) where

import Stackctl.Prelude

import qualified CfnFlip
import Data.Aeson
import Data.Graph (graphFromEdges, topSort)
import qualified Data.Yaml as Yaml
import RIO.Directory (createDirectoryIfMissing)
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

data StackSpec = StackSpec
  { ssSpecRoot :: FilePath
  , ssSpecPath :: StackSpecPath
  , ssSpecBody :: StackSpecYaml
  }

instance Display StackSpec where
  display StackSpec {..} = display ssSpecPath

stackSpecSpecPath :: StackSpec -> StackSpecPath
stackSpecSpecPath = ssSpecPath

stackSpecSpecBody :: StackSpec -> StackSpecYaml
stackSpecSpecBody = ssSpecBody

stackSpecStackName :: StackSpec -> StackName
stackSpecStackName = stackSpecPathStackName . ssSpecPath

stackSpecDepends :: StackSpec -> [StackName]
stackSpecDepends = fromMaybe [] . ssyDepends . ssSpecBody

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

logStackSpec
  :: (MonadIO m, MonadReader env m, HasLogFunc env) => StackSpec -> m ()
logStackSpec ss@StackSpec {..} = do
  Colors {..} <- getColorsLogFunc

  let
    account =
      cyan (display $ stackSpecPathAccountId ssSpecPath)
        <> " ("
        <> display (stackSpecPathAccountName ssSpecPath)
        <> ")"

  traverse_
    logInfo
    [ magenta $ fromString (stackSpecPathFilePath ssSpecPath)
    , "  Stack:    " <> cyan (display $ stackSpecStackName ss)
    , "  Template: " <> cyan (fromString $ ssyTemplate ssSpecBody)
    , "  Account:  " <> account
    , "  Region:   " <> cyan (display $ stackSpecPathRegion ssSpecPath)
    ]

writeStackSpec
  :: MonadUnliftIO m
  => FilePath -- ^ Parent directory
  -> StackSpec
  -> Value -- ^ Template body
  -> m ()
writeStackSpec parent stackSpec@StackSpec {..} templateBody = do
  createDirectoryIfMissing True $ takeDirectory templatePath

  case templateBody of
    -- Already Yaml
    String x -> writeFileUtf8 templatePath x
    _ -> CfnFlip.jsonToYamlFile templatePath templateBody

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
--
-- We use @aws cloudformation deploy --no-execute-changeset@ because it handles
-- the create-or-update logic for us.
--
createChangeSet
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => StackSpec
  -> m (Either Text (Maybe ChangeSet))
createChangeSet spec = awsCloudFormationCreateChangeSet
  (stackSpecStackName spec)
  (stackSpecTemplateFile spec)
  (stackSpecParameters spec)
  (stackSpecCapabilities spec)
  (stackSpecTags spec)

sortStackSpecs :: [StackSpec] -> [StackSpec]
sortStackSpecs specs = map nodeFromVertex $ reverse $ topSort graph
 where
  (graph, tripleFromVertex, _) = graphFromEdges $ map tripleFromNode specs

  nodeFromVertex = nodeFromTriple . tripleFromVertex

  tripleFromNode n = (n, stackSpecStackName n, stackSpecDepends n)

  nodeFromTriple (n, _, _) = n
