module Stackctl.Spec.Generate
  ( Generate (..)
  , GenerateSpec (..)
  , GenerateTemplate (..)
  , generate
  , TemplateFormat (..)
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Action
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption
import Stackctl.Spec.Discover (buildSpecPath)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

data Generate = Generate
  { gDescription :: Maybe StackDescription
  , gDepends :: Maybe [StackName]
  , gActions :: Maybe [Action]
  , gParameters :: Maybe ParametersYaml
  , gCapabilities :: Maybe [Capability]
  , gTags :: Maybe [Tag]
  , gSpec :: GenerateSpec
  , gTemplate :: GenerateTemplate
  , gOverwrite :: Bool
  }

data GenerateSpec
  = -- | Generate at an inferred name
    GenerateSpec StackName
  | -- | Generate to a given path
    GenerateSpecTo StackName FilePath

data GenerateTemplate
  = -- | Generate at an inferred name
    GenerateTemplate TemplateBody TemplateFormat
  | -- | Generate to the given path
    GenerateTemplateTo TemplateBody FilePath
  | -- | Assume template exists
    UseExistingTemplate FilePath

data TemplateFormat
  = TemplateFormatYaml
  | TemplateFormatJson

generate
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasConfig env
     , HasAwsScope env
     , HasDirectoryOption env
     )
  => Generate
  -> m FilePath
generate Generate {..} = do
  let
    (stackName, stackPath) = case gSpec of
      GenerateSpec name -> (name, unpack (unStackName name) <> ".yaml")
      GenerateSpecTo name path -> (name, path)

    (mTemplateBody, templatePath) = case gTemplate of
      GenerateTemplate body format ->
        ( Just body
        , case format of
            TemplateFormatYaml -> unpack (unStackName stackName) <> ".yaml"
            TemplateFormatJson -> unpack (unStackName stackName) <> ".json"
        )
      GenerateTemplateTo body path -> (Just body, path)
      UseExistingTemplate path -> (Nothing, path)

    specYaml =
      StackSpecYaml
        { ssyDescription = gDescription
        , ssyTemplate = templatePath
        , ssyDepends = gDepends
        , ssyActions = gActions
        , ssyParameters = gParameters
        , ssyCapabilities = gCapabilities
        , ssyTags = tagsYaml . map TagYaml <$> gTags
        }

  dir <- view $ directoryOptionL . to unDirectoryOption
  specPath <- buildSpecPath stackName stackPath
  stackSpec <- buildStackSpec dir specPath specYaml

  withThreadContext ["stackName" .= stackSpecStackName stackSpec] $ do
    writeStackSpec gOverwrite stackSpec mTemplateBody
    pure $ stackSpecPathFilePath specPath
