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
          [ toSpec "some-path" "some-path" Nothing
          , toSpec "some-other-path" "some-path-other" Nothing
          , toSpec "other-some-path" "other-some-path" Nothing
          , toSpec "prefix-foo" "prefix/foo" Nothing
          , toSpec "prefix-foo-bar" "prefix/foo-bar" Nothing
          , toSpec "prefix-foo-bar-prefix" "prefix/foo-bar/prefix" Nothing
          , toSpec "foo-suffix" "foo/suffix" Nothing
          , toSpec "foo-bar-suffix" "foo/bar/suffix" Nothing
          , toSpec "foo-suffix-bar" "foo/suffix/bar" Nothing
          ]

      map specName (filterStackSpecs option specs)
        `shouldMatchList` [ "some-path"
                          , "prefix-foo"
                          , "prefix-foo-bar"
                          , "foo-suffix"
                          , "foo-bar-suffix"
                          ]

    it "filters specs by template too" $ do
      let
        option = filterOptionFromPaths $ "templates/x" :| ["**/y/*"]
        specs =
          [ toSpec "some-path" "some-path" Nothing
          , toSpec "some-other-path" "some-path-other" $ Just "x"
          , toSpec "prefix-foo" "prefix/foo" Nothing
          , toSpec "other-some-path" "other-some-path" $ Just "z/y/t"
          , toSpec "prefix-foo-bar" "prefix/foo-bar" Nothing
          ]

      map specName (filterStackSpecs option specs)
        `shouldMatchList` ["some-other-path", "other-some-path"]

    it "filters specs by name too" $ do
      let
        option =
          filterOptionFromPaths $ "some-name" :| ["**/prefix/*", "templates/x"]
        specs =
          [ toSpec "some-name" "some-path" Nothing
          , toSpec "some-path" "some-path-other" $ Just "x"
          , toSpec "prefix-foo" "prefix/foo" Nothing
          , toSpec "other-some-path" "other-some-path" $ Just "z/y/t"
          , toSpec "prefix-foo-bar" "prefix/foo-bar" Nothing
          ]

      map specName (filterStackSpecs option specs)
        `shouldMatchList` [ "some-name"
                          , "some-path"
                          , "prefix-foo"
                          , "prefix-foo-bar"
                          ]

toSpec :: Text -> FilePath -> Maybe FilePath -> StackSpec
toSpec name path mTemplate = buildStackSpec "." specPath specBody
 where
  stackName = StackName name
  specPath = stackSpecPath scope stackName path
  specBody = StackSpecYaml
    { ssyDescription = Nothing
    , ssyDepends = Nothing
    , ssyActions = Nothing
    , ssyTemplate = fromMaybe path mTemplate
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
