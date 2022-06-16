module Stackctl.StackSpecSpec
  ( spec
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.StackSpecYaml
import Test.Hspec

spec :: Spec
spec = do
  describe "sortStackSpecs" $ do
    it "orders dependencies before dependents" $ do
      let
        specs =
          [ toSpec "app" ["roles", "iam", "networking"]
          , toSpec "roles" ["iam"]
          , toSpec "iam" []
          , toSpec "networking" []
          ]

      map specName (sortStackSpecs specs)
        `shouldBe` ["iam", "roles", "networking", "app"]

toSpec :: Text -> [Text] -> StackSpec
toSpec name depends = buildStackSpec "." specPath specBody
 where
  stackName = StackName name
  specPath = stackSpecPath scope stackName "a/b.yaml"
  specBody = StackSpecYaml
    { ssyDepends = Just $ map StackName depends
    , ssyTemplate = ""
    , ssyParameters = Nothing
    , ssyCapabilities = Nothing
    , ssyTags = Nothing
    }

  scope = AwsScope
    { awsAccountId = AccountId ""
    , awsAccountName = ""
    , awsRegion = Region' ""
    }

specName :: StackSpec -> Text
specName = unStackName . stackSpecStackName
