module Stackctl.AWS.Scope
  ( AwsScope (..)
  , awsScopeSpecPatterns
  , awsScopeSpecStackName
  , HasAwsScope (..)
  , fetchAwsScope
  ) where

import Stackctl.Prelude

import qualified Data.Text as T
import Stackctl.AWS
import System.Environment (lookupEnv)
import System.FilePath (joinPath, splitPath)
import System.FilePath.Glob (Pattern, compile, match)

data AwsScope = AwsScope
  { awsAccountId :: AccountId
  , awsAccountName :: Text
  , awsRegion :: Region
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

awsScopeSpecPatterns :: AwsScope -> [Pattern]
awsScopeSpecPatterns AwsScope {..} =
  [ compile
      $ "stacks"
      </> unpack (unAccountId awsAccountId)
      <> ".*"
      </> unpack (fromRegion awsRegion)
      <> "**"
      </> "*"
      <.> "yaml"
  , compile
      $ "stacks"
      </> "*."
      <> unpack (unAccountId awsAccountId)
      </> unpack (fromRegion awsRegion)
      <> "**"
      </> "*"
      <.> "yaml"
  ]

awsScopeSpecStackName :: AwsScope -> FilePath -> Maybe StackName
awsScopeSpecStackName scope path = do
  guard $ any (`match` path) $ awsScopeSpecPatterns scope

  -- once we've guarded that the path matches our scope patterns, we can play it
  -- pretty fast and loose with the "parsing" step
  pure
    $ path -- stacks/account/region/x/y.yaml
    & splitPath -- [stacks/, account/, region/, x/, y.yaml]
    & drop 3 -- [x, y.yaml]
    & joinPath -- x/y.yaml
    & dropExtension -- x/y
    & pack
    & T.replace "/" "-" -- x-y
    & StackName

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
