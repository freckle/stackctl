module Stackctl.Docker
  ( DockerRegistry(..)
  , DockerImageName(..)
  , DockerTag(..)
  , DockerImage
  , dockerImage
  , unDockerImage

  -- * @docker login@
  , LoginUsername(..)
  , LoginPassword(..)
  , dockerLogin

  -- * @docker build@
  , BuildContext(..)
  , dockerBuild

  -- * @docker push@
  , dockerPush
  ) where

import Stackctl.Prelude

import Data.Aeson
import qualified RIO.ByteString.Lazy as BSL

newtype DockerRegistry = DockerRegistry
  { unDockerRegistry :: Text
  }
  deriving newtype Display

newtype DockerImageName = DockerImageName
  { unDockerImageName :: Text
  }
  deriving newtype Display

newtype DockerTag = DockerTag
  { unDockerTag :: Text
  }
  deriving newtype Display

newtype DockerImage = DockerImage
  { unDockerImage :: Text
  }
  deriving newtype Display

dockerImage
  :: Maybe DockerRegistry -> DockerImageName -> DockerTag -> DockerImage
dockerImage registry imageName tag =
  DockerImage
    $ maybe "" ((<> "/") . unDockerRegistry) registry
    <> unDockerImageName imageName
    <> ":"
    <> unDockerTag tag

newtype LoginUsername = LoginUsername
  { unLoginUsername :: Text
  }

newtype LoginPassword = LoginPassword
  { unLoginPassword :: Text
  }

dockerLogin
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => DockerRegistry
  -> LoginUsername
  -> LoginPassword
  -> m ()
dockerLogin registry username password =
  proc
      "docker"
      (concat
        [ ["login"]
        , ["--username", unpack $ unLoginUsername username]
        , ["--password-stdin"]
        , [unpack $ unDockerRegistry registry]
        ]
      )
    $ runProcess_
    . setStdin (byteStringInput bsl)
  where bsl = BSL.fromStrict $ encodeUtf8 $ unLoginPassword password

newtype BuildContext = BuildContext
  { unBuildContext :: FilePath
  }
  deriving newtype FromJSON

instance Display BuildContext where
  display = fromString . unBuildContext

dockerBuild
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => DockerImage
  -> Maybe FilePath
  -> BuildContext
  -> m ()
dockerBuild image mDockerfile context = proc
  "docker"
  (concat
    [ ["build"]
    , ["--tag", unpack $ unDockerImage image]
    , maybe [] (\f -> ["--file", f]) mDockerfile
    , [unBuildContext context]
    ]
  )
  runProcess_

dockerPush
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => DockerImage
  -> m ()
dockerPush image =
  proc "docker" ["push", unpack $ unDockerImage image] runProcess_
