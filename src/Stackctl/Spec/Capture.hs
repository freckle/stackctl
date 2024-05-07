module Stackctl.Spec.Capture
  ( CaptureOptions (..)
  , parseCaptureOptions
  , runCapture
  ) where

import Stackctl.Prelude

import Options.Applicative
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.Spec.Generate
import Stackctl.StackSpec
import Stackctl.StackSpecYaml
  ( StackSpecYaml (..)
  , TagYaml (..)
  , parameterYaml
  , parametersYaml
  , tagsYaml
  )
import System.FilePath.Glob

data CaptureOptions = CaptureOptions
  { scoAccountName :: Maybe Text
  , scoTemplatePath :: Maybe FilePath
  , scoStackPath :: Maybe FilePath
  , scoDepends :: Maybe [StackName]
  , scoTemplateFormat :: TemplateFormat
  , scoStackName :: Pattern
  }

-- brittany-disable-next-binding

parseCaptureOptions :: Parser CaptureOptions
parseCaptureOptions =
  CaptureOptions
    <$> optional
      ( strOption
          ( short 'n'
              <> long "account-name"
              <> metavar "NAME"
              <> help "Account name to use in generated files"
          )
      )
    <*> optional
      ( strOption
          ( short 't'
              <> long "template-path"
              <> metavar "PATH"
              <> help "Write Template to PATH. Default is based on STACK"
          )
      )
    <*> optional
      ( strOption
          ( short 'p'
              <> long "path"
              <> metavar "PATH"
              <> help "Write specification to PATH. Default is based on STACK"
          )
      )
    <*> optional
      ( some
          ( StackName
              <$> strOption
                ( long "depend"
                    <> metavar "STACK"
                    <> help "Add a dependency on STACK"
                )
          )
      )
    <*> flag
      TemplateFormatYaml
      TemplateFormatJson
      ( long "no-flip"
          <> help "Don't flip JSON templates to Yaml"
      )
    <*> strArgument
      ( metavar "STACK"
          <> help "Name of deployed Stack to capture"
      )

runCapture
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     , HasConfig env
     , HasDirectoryOption env
     )
  => CaptureOptions
  -> m ()
runCapture CaptureOptions {..} = do
  let
    setScopeName scope =
      maybe scope (\name -> scope {awsAccountName = name}) scoAccountName

    generate' stack template mPath mTemplatePath = do
      let
        stackName = StackName $ stack ^. stack_stackName
        templateBody = templateBodyFromValue template

      void
        $ local (awsScopeL %~ setScopeName)
        $ generate
          False
          ( case mPath of
              Nothing -> GenerateSpec stackName
              Just sp -> GenerateSpecTo stackName sp
          )
          ( case mTemplatePath of
              Nothing -> GenerateTemplate templateBody scoTemplateFormat
              Just tp -> GenerateTemplateTo templateBody tp
          )
          ( \templatePath ->
              StackSpecYaml
                { ssyDescription = stackDescription stack
                , ssyTemplate = templatePath
                , ssyDepends = scoDepends
                , ssyActions = Nothing
                , ssyParameters = parametersYaml . mapMaybe parameterYaml <$> parameters stack
                , ssyCapabilities = capabilities stack
                , ssyTags = tagsYaml . map TagYaml <$> tags stack
                }
          )

  results <- awsCloudFormationGetStackNamesMatching scoStackName

  case results of
    [] -> do
      logError
        $ "No Active Stacks match "
        <> pack (decompile scoStackName)
        :# []
      exitFailure
    [stackName] -> do
      stack <- awsCloudFormationDescribeStack stackName
      template <- awsCloudFormationGetTemplate stackName
      generate' stack template scoStackPath scoTemplatePath
    stackNames -> do
      logInfo "Capturing multiple matching Stacks"
      for_ scoStackPath $ \_ -> logWarn "--path option ignored"
      for_ scoTemplatePath $ \_ -> logWarn "--template-path option ignored"
      for_ stackNames $ \stackName -> do
        stack <- awsCloudFormationDescribeStack stackName
        template <- awsCloudFormationGetTemplate stackName
        generate' stack template Nothing Nothing
