module Stackctl.AWS.Scope
  ( AwsScope(..)
  , HasAwsScope(..)
  , fetchAwsScope
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import System.Environment (lookupEnv)

data AwsScope = AwsScope
  { awsAccountId :: AccountId
  , awsAccountName :: Text
  , awsRegion :: Region
  }
  deriving stock Generic
  deriving anyclass ToJSON

class HasAwsScope env where
  awsScopeL :: Lens' env AwsScope

instance HasAwsScope AwsScope where
  awsScopeL = id

fetchAwsScope
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => m AwsScope
fetchAwsScope =
  AwsScope
    <$> awsGetCallerIdentityAccount
    <*> liftIO (maybe "unknown" pack <$> lookupEnv "AWS_PROFILE")
    <*> awsEc2DescribeFirstAvailabilityZoneRegionName
