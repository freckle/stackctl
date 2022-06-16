module Stackctl.AWS.CloudFormationSpec
  ( spec
  ) where

import Stackctl.Prelude2

import Data.List (isSuffixOf)
import Stackctl.AWS.CloudFormation
import Test.Hspec

spec :: Spec
spec = do
  describe "readParameter" $ do
    it "refuses empty key" $ do
      readParameter "=Value"
        `shouldSatisfy` either ("empty KEY" `isSuffixOf`) (const False)

    it "refuses empty value" $ do
      readParameter "Key"
        `shouldSatisfy` either ("empty VALUE" `isSuffixOf`) (const False)

    it "refuses empty value (with =)" $ do
      readParameter "Key="
        `shouldSatisfy` either ("empty VALUE" `isSuffixOf`) (const False)

    it "creates a parameter when valid" $ do
      readParameter "Key=Value=More"
        `shouldBe` Right (makeParameter "Key" $ Just "Value=More")
