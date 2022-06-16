module Stackctl.Spec.Generate
  ( Generate(..)
  , generate
  ) where

import Stackctl.Prelude

import Data.Aeson
import Stackctl.AWS
import Stackctl.Spec.Discover (buildSpecPath)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml
import System.Environment (lookupEnv)

data Generate = Generate
  { gOutputDirectory :: FilePath
  , gAccountName :: Maybe Text
  -- ^ If not given, will use @${AWS_PROFILE:-unknown}@
  , gTemplatePath :: Maybe FilePath
  -- ^ If not given will use @{stack-name}.yaml@
  , gStackPath :: Maybe FilePath
  -- ^ If not given will use @{stack-name}.yaml@
  , gStackName :: StackName
  , gDepends :: Maybe [StackName]
  , gParameters :: Maybe [Parameter]
  , gCapabilities :: Maybe [Capability]
  , gTags :: Maybe [Tag]
  , gTemplate :: Value
  }

generate
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsEnv env
     )
  => Generate
  -> m FilePath
generate Generate {..} = do
  let
    path = unpack (unStackName gStackName) <.> "yaml"
    stackPath = fromMaybe path gStackPath

  profile <- liftIO $ maybe "unknown" pack <$> lookupEnv "AWS_PROFILE"
  specPath <- buildSpecPath
    (fromMaybe profile gAccountName)
    gStackName
    stackPath

  let
    templatePath = fromMaybe path gTemplatePath
    specYaml = StackSpecYaml
      { ssyTemplate = templatePath
      , ssyDepends = gDepends
      , ssyParameters = map ParameterYaml <$> gParameters
      , ssyCapabilities = gCapabilities
      , ssyTags = map TagYaml <$> gTags
      }

    stackSpec = buildStackSpec gOutputDirectory specPath specYaml

  logInfo "Generating specification"
  logStackSpec stackSpec

  writeStackSpec gOutputDirectory stackSpec gTemplate
  pure $ stackSpecPathFilePath specPath
