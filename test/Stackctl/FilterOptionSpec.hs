module Stackctl.FilterOptionSpec
  ( spec
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.FilterOption
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml
import Test.Hspec

spec :: Spec
spec = do
  describe "filterStackSpecs" $ do
    it "filters specs matching any of the given patterns" $ do
      let
        option =
          filterOptionFromPaths $ "**/some-path" :| ["**/prefix/*", "**/suffix"]
        specs =
          [ toSpec "some-path" "some-path"
          , toSpec "some-other-path" "some-path-other"
          , toSpec "other-some-path" "other-some-path"
          , toSpec "prefix-foo" "prefix/foo"
          , toSpec "prefix-foo-bar" "prefix/foo-bar"
          , toSpec "prefix-foo-bar-prefix" "prefix/foo-bar/prefix"
          , toSpec "foo-suffix" "foo/suffix"
          , toSpec "foo-bar-suffix" "foo/bar/suffix"
          , toSpec "foo-suffix-bar" "foo/suffix/bar"
          ]

      map specName (filterStackSpecs option specs)
        `shouldMatchList` [ "some-path"
                          , "prefix-foo"
                          , "prefix-foo-bar"
                          , "foo-suffix"
                          , "foo-bar-suffix"
                          ]

toSpec :: Text -> FilePath -> StackSpec
toSpec name path = buildStackSpec "." specPath specBody
 where
  stackName = StackName name
  specPath = stackSpecPath scope stackName path
  specBody = StackSpecYaml
    { ssyDescription = Nothing
    , ssyDepends = Nothing
    , ssyActions = Nothing
    , ssyTemplate = ""
    , ssyParameters = Nothing
    , ssyCapabilities = Nothing
    , ssyTags = Nothing
    }

  scope = AwsScope
    { awsAccountId = AccountId "1234567890"
    , awsAccountName = "test-account"
    , awsRegion = Region' "us-east-1"
    }

specName :: StackSpec -> Text
specName = unStackName . stackSpecStackName
