module Stackctl.Spec.Generate
  ( Generate(..)
  , generate
  , TemplateFormat(..)
  ) where

import Stackctl.Prelude

import Stackctl.Action
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Spec.Discover (buildSpecPath)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

data Generate = Generate
  { gOutputDirectory :: FilePath
  , gTemplatePath :: Maybe FilePath
  -- ^ If not given, will use @{stack-name}.(yaml|json)@
  , gTemplateFormat :: TemplateFormat
  -- ^ Ignored if 'gTemplatePath' is given
  , gStackPath :: Maybe FilePath
  -- ^ If not given, will use @{stack-name}.yaml@
  , gStackName :: StackName
  , gDescription :: Maybe StackDescription
  , gDepends :: Maybe [StackName]
  , gActions :: Maybe [Action]
  , gParameters :: Maybe [Parameter]
  , gCapabilities :: Maybe [Capability]
  , gTags :: Maybe [Tag]
  , gTemplateBody :: TemplateBody
  }

data TemplateFormat
  = TemplateFormatYaml
  | TemplateFormatJson

generate
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     )
  => Generate
  -> m FilePath
generate Generate {..} = do
  let
    defaultStackPath = unpack (unStackName gStackName) <.> "yaml"
    defaultTemplatePath =
      unpack (unStackName gStackName) <.> case gTemplateFormat of
        TemplateFormatYaml -> "yaml"
        TemplateFormatJson -> "json"

    stackPath = fromMaybe defaultStackPath gStackPath

  specPath <- buildSpecPath gStackName stackPath

  let
    templatePath = fromMaybe defaultTemplatePath gTemplatePath
    specYaml = StackSpecYaml
      { ssyDescription = gDescription
      , ssyTemplate = templatePath
      , ssyDepends = gDepends
      , ssyActions = gActions
      , ssyParameters = parametersYaml . mapMaybe parameterYaml <$> gParameters
      , ssyCapabilities = gCapabilities
      , ssyTags = map TagYaml <$> gTags
      }

    stackSpec = buildStackSpec gOutputDirectory specPath specYaml

  withThreadContext ["stackName" .= stackSpecStackName stackSpec] $ do
    logInfo "Generating specification"
    writeStackSpec gOutputDirectory stackSpec gTemplateBody
    pure $ stackSpecPathFilePath specPath
