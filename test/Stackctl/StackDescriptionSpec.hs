module Stackctl.StackDescriptionSpec
  ( spec
  ) where

import Stackctl.Prelude

import Stackctl.StackDescription
import Test.Hspec

spec :: Spec
spec = do
  describe "addStackDescription" $ do
    let aDescription = Just $ StackDescription "A \"cool\" description"

    it "does nothing with nothing" $ do
      addStackDescription Nothing "hi there" `shouldBe` "hi there"

    it "does nothing invalid inputs" $ do
      for_ ["", "hi there", "[a list]", "{\"invalid\":", "true", "42"]
        $ \input -> addStackDescription aDescription input `shouldBe` input

    context "Yaml" $ do
      it "adds a Description" $ do
        addStackDescription aDescription "Resources: []\n"
          `shouldBe` "Description: \"A \\\"cool\\\" description\"\nResources: []\n"

      it "does not clobber or duplicate an existing Description" $ do
        addStackDescription
          aDescription
          "Resources: []\nDescription: Existing description\n"
          `shouldBe` "Resources: []\nDescription: Existing description\n"

    context "JSON" $ do
      it "adds a Description" $ do
        addStackDescription aDescription "{\"Resources\":[]}"
          `shouldBe` "{\"Description\":\"A \\\"cool\\\" description\",\"Resources\":[]}"

      it "does not clobber or duplicate an existing Description" $ do
        addStackDescription
          aDescription
          "{\"Resources\":[],\"Description\":\"Existing description\"}"
          `shouldBe` "{\"Resources\":[],\"Description\":\"Existing description\"}"
