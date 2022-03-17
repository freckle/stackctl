module Stackctl.AWS.ECR
  ( EcrRepository
  , unEcrRepository
  , ecrRepositoryName
  , ecrRepositoryDockerImage
  , EcrRepositoryName(..)
  , EcrRepositoryPolicy(..)
  , awsEcrGetLogin
  , awsEcrCreateRepository
  , awsEcrRepositoryExists
  , prodEcrDockerRegistry
  , prodEcrRepository
  ) where

import Stackctl.Prelude

import Amazonka.ECR.CreateRepository
import Amazonka.ECR.DescribeRepositories
import Amazonka.ECR.GetAuthorizationToken
import Amazonka.ECR.SetRepositoryPolicy
import Amazonka.ECR.Types
import qualified Amazonka.ECR.Types.Repository as Repository
import Control.Lens ((?~))
import Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import Stackctl.AWS.Core
import Stackctl.Docker
import qualified RIO.ByteString.Lazy as BSL
import RIO.List (headMaybe)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)

data EcrRepository = EcrRepository
  { erDockerRegistry :: DockerRegistry
  , erName :: EcrRepositoryName
  }

instance Display EcrRepository where
  display = display . unEcrRepository

unEcrRepository :: EcrRepository -> Text
unEcrRepository EcrRepository {..} =
  unDockerRegistry erDockerRegistry <> "/" <> unEcrRepositoryName erName

ecrRepositoryName :: EcrRepository -> EcrRepositoryName
ecrRepositoryName = erName

ecrRepositoryDockerImage :: EcrRepository -> DockerTag -> DockerImage
ecrRepositoryDockerImage EcrRepository {..} tag = dockerImage
  (Just erDockerRegistry)
  (DockerImageName $ unEcrRepositoryName erName)
  tag

newtype EcrRepositoryName = EcrRepositoryName
  { unEcrRepositoryName :: Text
  }
  deriving newtype (Eq, Show, Display, FromJSON)

newtype EcrRepositoryPolicy = EcrRepositoryPolicy
  { unEcrRepositoryPolicy :: Value
  }
  deriving newtype (Eq, Show, ToJSON)

ecrRepositoryPolicyText :: EcrRepositoryPolicy -> Text
ecrRepositoryPolicyText =
  decodeUtf8 . BSL.toStrict . encode . unEcrRepositoryPolicy

awsEcrGetLogin
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => AccountId
  -> Region
  -> m (LoginUsername, LoginPassword)
awsEcrGetLogin accountId region = do
  let
    req =
      newGetAuthorizationToken
        & (getAuthorizationToken_registryIds ?~ pure (unAccountId accountId))

  awsSimpleWithin region "GetAuthorizationToken" req $ \resp -> do
    bs <-
      hush
      . Base64.decode
      . encodeUtf8
      =<< (^. authorizationData_authorizationToken)
      =<< headMaybe
      =<< (resp ^. getAuthorizationTokenResponse_authorizationData)

    pure
      $ bimap LoginUsername (LoginPassword . T.drop 1)
      $ T.breakOn ":"
      $ decodeUtf8 bs

awsEcrCreateRepository
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => AccountId
  -> Region
  -> EcrRepositoryName
  -> EcrRepositoryPolicy
  -> m ()
awsEcrCreateRepository accountId region (EcrRepositoryName name) policy = do
  let
    req =
      newCreateRepository name
        & (createRepository_registryId ?~ unAccountId accountId)

  _repo <- awsSimpleWithin region "CreateRepository" req $ \resp -> do
    resp ^. createRepositoryResponse_repository

  void
    $ awsSendWithin region
    $ newSetRepositoryPolicy name (ecrRepositoryPolicyText policy)
    & (setRepositoryPolicy_registryId ?~ unAccountId accountId)

-- | Describe repository names in Prod/@us-east-1@ ECR
awsEcrRepositoryExists
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => AccountId
  -> Region
  -> EcrRepositoryName
  -> m Bool
awsEcrRepositoryExists accountId region (EcrRepositoryName name) = do
  let
    req =
      newDescribeRepositories
        & (describeRepositories_registryId ?~ unAccountId accountId)
        . (describeRepositories_repositoryNames ?~ pure name)

  awsSimpleWithin region "DescribeRepositories" req $ \resp -> do
    repos <- resp ^. describeRepositoriesResponse_repositories
    pure $ any ((== Just name) . Repository.repositoryName) repos

prodEcrDockerRegistry :: DockerRegistry
prodEcrDockerRegistry =
  DockerRegistry
    $ unAccountId prodAccountId
    <> ".dkr.ecr."
    <> fromRegion usEast1
    <> ".amazonaws.com"

prodEcrRepository :: EcrRepositoryName -> EcrRepository
prodEcrRepository name =
  EcrRepository { erDockerRegistry = prodEcrDockerRegistry, erName = name }
