module Stackctl.Datadog
  ( apiKey
  , agentImage
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.Docker

apiKey :: Text
apiKey = "0cbaebac7ac435ddd4d8f6fcebf52a68"

agentImage :: DockerImage
agentImage = ecrRepositoryDockerImage
  (prodEcrRepository $ EcrRepositoryName "frontrow/datadog-agent")
  (DockerTag "v7.19.0-0")
