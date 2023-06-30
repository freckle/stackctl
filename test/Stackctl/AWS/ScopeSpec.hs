module Stackctl.AWS.ScopeSpec
  ( spec
  ) where

import Stackctl.Prelude

import Stackctl.AWS.CloudFormation
import Stackctl.AWS.Core
import Stackctl.AWS.Scope
import Test.Hspec

spec :: Spec
spec = do
  describe "awsScopeSpecStackName" $ do
    let scope =
          AwsScope
            { awsAccountId = AccountId "123"
            , awsAccountName = "testing"
            , awsRegion = "us-east-1"
            }

    it "parses full paths to stacks in the current scope" $ do
      awsScopeSpecStackName scope "stacks/123.testing/us-east-1/foo.yaml"
        `shouldBe` Just (StackName "foo")

    it "parses name.account style too" $ do
      awsScopeSpecStackName scope "stacks/testing.123/us-east-1/foo.yaml"
        `shouldBe` Just (StackName "foo")

    it "handles sub-directories" $ do
      awsScopeSpecStackName scope "stacks/123.testing/us-east-1/foo/bar.yaml"
        `shouldBe` Just (StackName "foo-bar")

    it "handles mismatched name" $ do
      awsScopeSpecStackName scope "stacks/123.x/us-east-1/foo.yaml"
        `shouldBe` Just (StackName "foo")

    it "handles mismatched name in name.account style" $ do
      awsScopeSpecStackName scope "stacks/x.123/us-east-1/foo.yaml"
        `shouldBe` Just (StackName "foo")

    it "avoids wrong region" $ do
      awsScopeSpecStackName scope "stacks/123.testing/us-east-2/foo.yaml"
        `shouldBe` Nothing

    it "avoids arong account id" $ do
      awsScopeSpecStackName scope "stacks/124.testing/us-east-1/foo.yaml"
        `shouldBe` Nothing

    it "requires a stacks/ prefix" $ do
      awsScopeSpecStackName scope "123.testing/us-east-1/foo.yaml"
        `shouldBe` Nothing

    it "requires a .yaml suffix" $ do
      awsScopeSpecStackName scope "stacks/123.testing/us-east-1/foo.yml"
        `shouldBe` Nothing
