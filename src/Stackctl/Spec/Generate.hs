module Stackctl.Spec.Generate
  ( GenerateSpec (..)
  , GenerateTemplate (..)
  , generate
  , TemplateFormat (..)
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption
import Stackctl.Spec.Discover (buildSpecPath)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

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
  => Bool
  -> GenerateSpec
  -> GenerateTemplate
  -> (FilePath -> StackSpecYaml)
  -> m FilePath
generate overwrite spec template toStackSpecYaml = do
  let
    (stackName, stackPath) = case spec of
      GenerateSpec name -> (name, unpack (unStackName name) <> ".yaml")
      GenerateSpecTo name path -> (name, path)

    (mTemplateBody, templatePath) = case template of
      GenerateTemplate body format ->
        ( Just body
        , case format of
            TemplateFormatYaml -> unpack (unStackName stackName) <> ".yaml"
            TemplateFormatJson -> unpack (unStackName stackName) <> ".json"
        )
      GenerateTemplateTo body path -> (Just body, path)
      UseExistingTemplate path -> (Nothing, path)

    specYaml = toStackSpecYaml templatePath

  dir <- view $ directoryOptionL . to unDirectoryOption
  specPath <- buildSpecPath stackName stackPath
  stackSpec <- buildStackSpec dir specPath specYaml

  withThreadContext ["stackName" .= stackSpecStackName stackSpec] $ do
    writeStackSpec overwrite stackSpec mTemplateBody
    pure $ stackSpecPathFilePath specPath
