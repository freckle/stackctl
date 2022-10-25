module Stackctl.Spec.Generate
  ( Generate(..)
  , generate
  ) where

import Stackctl.Prelude

import Data.Aeson
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Action
import Stackctl.Spec.Discover (buildSpecPath)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml

data Generate = Generate
  { gOutputDirectory :: FilePath
  , gTemplatePath :: Maybe FilePath
  -- ^ If not given will use @{stack-name}.yaml@
  , gStackPath :: Maybe FilePath
  -- ^ If not given will use @{stack-name}.yaml@
  , gStackName :: StackName
  , gDepends :: Maybe [StackName]
  , gActions :: Maybe [Action]
  , gParameters :: Maybe [Parameter]
  , gCapabilities :: Maybe [Capability]
  , gTags :: Maybe [Tag]
  , gTemplate :: Value
  }

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
    path = unpack (unStackName gStackName) <.> "yaml"
    stackPath = fromMaybe path gStackPath

  specPath <- buildSpecPath gStackName stackPath

  let
    templatePath = fromMaybe path gTemplatePath
    specYaml = StackSpecYaml
      { ssyTemplate = templatePath
      , ssyDepends = gDepends
      , ssyActions = gActions
      , ssyParameters = map ParameterYaml <$> gParameters
      , ssyCapabilities = gCapabilities
      , ssyTags = map TagYaml <$> gTags
      }

    stackSpec = buildStackSpec gOutputDirectory specPath specYaml

  withThreadContext ["stackName" .= stackSpecStackName stackSpec] $ do
    logInfo "Generating specification"
    writeStackSpec gOutputDirectory stackSpec gTemplate
    pure $ stackSpecPathFilePath specPath
