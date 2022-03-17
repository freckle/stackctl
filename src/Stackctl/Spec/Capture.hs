module Stackctl.Spec.Capture
  ( CaptureOptions(..)
  , runCaptureOptions
  , runCapture
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import qualified Stackctl.Paths as Paths
import Stackctl.Spec.Generate
import Options.Applicative

data CaptureOptions = CaptureOptions
  { scoOutputDirectory :: FilePath
  , scoAccountName :: Maybe Text
  , scoTemplatePath :: Maybe FilePath
  , scoStackPath :: Maybe FilePath
  , scoDepends :: Maybe [StackName]
  , scoStackName :: StackName
  }

-- brittany-disable-next-binding

runCaptureOptions :: Parser CaptureOptions
runCaptureOptions = CaptureOptions
    <$> strOption
      (  short 'd'
      <> long "output-directory"
      <> metavar "PATH"
      <> help "Directory within which to generate"
      <> value Paths.platformSpecs
      <> showDefault
      )
    <*> optional (strOption
      (  short 'n'
      <> long "account-name"
      <> metavar "NAME"
      <> help "Account name to use in generated files"
      ))
    <*> optional (strOption
      (  short 't'
      <> long "template-path"
      <> metavar "PATH"
      <> help "Write Template to PATH. Default is based on STACK"
      ))
    <*> optional (strOption
      (  short 'p'
      <> long "path"
      <> metavar "PATH"
      <> help "Write specification to PATH. Default is based on STACK"
      ))
    <*> optional (some (StackName <$> strOption
      (  long "depend"
      <> metavar "STACK"
      <> help "Add a dependency on STACK"
      )))
    <*> (StackName <$> argument str
      (  metavar "STACK"
      <> help "Name of deployed Stack to capture"
      ))

runCapture
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     )
  => CaptureOptions
  -> m ()
runCapture CaptureOptions {..} = do
  stack <- awsCloudFormationDescribeStack scoStackName
  template <- awsCloudFormationGetTemplate scoStackName
  void $ generate Generate
    { gOutputDirectory = scoOutputDirectory
    , gAccountName = scoAccountName
    , gTemplatePath = scoTemplatePath
    , gStackPath = scoStackPath
    , gStackName = scoStackName
    , gDepends = scoDepends
    , gParameters = parameters stack
    , gCapabilities = capabilities stack
    , gTags = tags stack
    , gTemplate = template
    }
