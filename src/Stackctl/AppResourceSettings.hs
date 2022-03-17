module Stackctl.AppResourceSettings
  ( AppResourceSettings(..)
  , AppResourceSettingsYaml
  , appResourceSettingsFromYaml
  ) where

import Stackctl.Prelude

import Data.Aeson
import Stackctl.AWS
  ( BucketName(..)
  , BucketPathPrefix
  , EcrRepository
  , EcrRepositoryName(..)
  , StackName(..)
  , StackTemplate(..)
  , bucketPathPrefix
  , prodEcrRepository
  )
import Stackctl.Docker
import Stackctl.Environment

data AppResourceSettings = AppResourceSettings
  { arsStackName :: StackName
  , arsStackTemplate :: StackTemplate
  , arsStackParameters :: FilePath
  , arsResourceName :: Text
  , arsQualifiedResourceName :: Text
  , arsEcrRepository :: EcrRepository
  , arsDockerBuildContext :: BuildContext
  , arsDockerfile :: Maybe FilePath
  , arsAssetsDist :: FilePath
  , arsS3AssetBucket :: BucketName
  , arsS3AssetPathPrefix :: BucketPathPrefix
  }

data AppResourceSettingsYaml = AppResourceSettingsYaml
  { stackName :: Maybe StackName
  , stackTemplate :: Maybe StackTemplate
  , stackParameters :: Maybe FilePath
  , name :: Maybe Text
  , docker :: Maybe DockerSettingsYaml
  , assets :: Maybe AssetsSettingsYaml
  }
  deriving stock Generic
  deriving anyclass FromJSON

data DockerSettingsYaml = DockerSettingsYaml
  { repository :: Maybe EcrRepositoryName
  , context :: Maybe BuildContext
  , dockerfile :: Maybe FilePath
  }
  deriving stock Generic
  deriving anyclass FromJSON

data AssetsSettingsYaml = AssetsSettingsYaml
  { bucket :: Maybe BucketName
  , prefix :: Maybe BucketPathPrefix
  , dist :: Maybe FilePath
  }
  deriving stock Generic
  deriving anyclass FromJSON

-- brittany-next-binding --columns=90

appResourceSettingsFromYaml
  :: Maybe Environment
  -> Text
  -> Text
  -> Maybe AppResourceSettingsYaml
  -> AppResourceSettings
appResourceSettingsFromYaml mEnvironment appName appResourceName mSettings =
  AppResourceSettings
    { arsStackName = defaulted arsStackName stackName
    , arsStackTemplate = defaulted arsStackTemplate stackTemplate
    , arsStackParameters = defaulted arsStackParameters stackParameters
    , arsResourceName = arsResourceName defaults
    , arsQualifiedResourceName = arsQualifiedResourceName defaults
    , arsEcrRepository =
      defaulted arsEcrRepository $ (fmap prodEcrRepository . repository) <=< docker
    , arsDockerBuildContext = defaulted arsDockerBuildContext $ context <=< docker
    , arsDockerfile = dockerfile =<< docker =<< mSettings
    , arsAssetsDist = defaulted arsAssetsDist $ dist <=< assets
    , arsS3AssetBucket = defaulted arsS3AssetBucket $ bucket <=< assets
    , arsS3AssetPathPrefix = defaulted arsS3AssetPathPrefix $ prefix <=< assets
    }
 where
  defaulted :: (AppResourceSettings -> a) -> (AppResourceSettingsYaml -> Maybe a) -> a
  defaulted f g = fromMaybe (f defaults) $ g =<< mSettings

  defaults :: AppResourceSettings
  defaults =
    defaultAppResourceSettings mEnvironment appName
      $ fromMaybe appResourceName
      $ name
      =<< mSettings

defaultAppResourceSettings
  :: Maybe Environment -> Text -> Text -> AppResourceSettings
defaultAppResourceSettings mEnvironment appName appResourceName =
  AppResourceSettings
    { arsStackName = StackName $ envPrefix <> qualifiedName
    , arsStackTemplate =
      StackTemplate
      $ "platform-stack-"
      <> unpack (envPrefix <> qualifiedName)
      <.> "yaml"
    , arsStackParameters =
      "platform-stack-"
      <> unpack (envPrefix <> qualifiedName)
      <> "-parameters"
      <.> "json"
    , arsResourceName = appResourceName
    , arsQualifiedResourceName = qualifiedName
    , arsEcrRepository =
      prodEcrRepository $ EcrRepositoryName $ "frontrow/" <> qualifiedName
    , arsDockerBuildContext = BuildContext $ unpack $ if singletonResource
      then "."
      else appResourceName
    , arsDockerfile = Nothing
    , arsAssetsDist = unpack
      $ if singletonResource then "." else appResourceName
    , arsS3AssetBucket = BucketName "freckle-platform-assets"
    , arsS3AssetPathPrefix = bucketPathPrefix qualifiedName
    }
 where
  envPrefix = maybe "" ((<> "-") . unEnvironment) mEnvironment

  qualifiedName = if singletonResource
    then appResourceName
    else appName <> "-" <> appResourceName

  singletonResource = appResourceName == appName
