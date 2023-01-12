module Stackctl.Config.RequiredVersionSpec
  ( spec
  ) where

import Stackctl.Prelude

import Data.Aeson (decode, encode)
import Data.Version
import Stackctl.Config.RequiredVersion
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "JSON" $ do
    it "round-trips" $ property $ \rv -> do
      decode (encode @RequiredVersion rv) `shouldBe` Just rv

  describe "requiredVersionFromText" $ do
    it "parses with or without operator" $ do
      requiredVersionFromText "1.2.3-rc1" `shouldSatisfy` isRight
      requiredVersionFromText "= 1.2.3-rc1" `shouldSatisfy` isRight

    it "rejects unknown operators" $ do
      requiredVersionFromText "!! 1.2.3" `shouldSatisfy` isLeft

    it "rejects invalid versions" $ do
      requiredVersionFromText "= wowOMG-2/2" `shouldSatisfy` isLeft

  describe "parsing operators" $ do
    let prop cmp = property . uncurry . compareAsRequiredVersion cmp

    it "compares exactly" $ prop (==) Nothing
    it "compares with = " $ prop (==) $ Just "="
    it "compares with < " $ prop (<) $ Just "<"
    it "compares with <=" $ prop (<=) $ Just "<="
    it "compares with > " $ prop (>) $ Just ">"
    it "compares with >=" $ prop (>=) $ Just ">="
    it "compares with =~" $ prop (=~) $ Just "=~"


  describe "=~" $ do
    it "treats equal versions as satisfying" $ do
      makeVersion [1, 2, 3] =~ makeVersion [1, 2, 3] `shouldBe` True

    it "treats older versions as non-satisfying" $ do
      makeVersion [1, 2, 2] =~ makeVersion [1, 2, 3] `shouldBe` False

    it "treats newer versions of the same branch as satisfying" $ do
      makeVersion [1, 2, 3, 1] =~ makeVersion [1, 2, 3] `shouldBe` True

    it "treats newer versions as non-satisfying" $ do
      makeVersion [1, 2, 4] =~ makeVersion [1, 2, 3] `shouldBe` False

    it "respects the number of components specified" $ do
      makeVersion [1, 2] =~ makeVersion [1, 2] `shouldBe` True
      makeVersion [1, 2, 3] =~ makeVersion [1, 2] `shouldBe` True
      makeVersion [1, 1] =~ makeVersion [1, 2] `shouldBe` False
      makeVersion [1, 3] =~ makeVersion [1, 2, 3] `shouldBe` False

compareAsRequiredVersion
  :: (Version -> Version -> Bool)
  -- ^ Reference compare
  -> Maybe Text
  -- ^ Operator
  -> Version
  -- ^ Hypothetical required version
  -> Version
  -- ^ Hypotehtical current version
  -> Bool
compareAsRequiredVersion cmp mOperator required current =
  runRequiredVersion mOperator required current
    == Right (current `cmp` required)

runRequiredVersion
  :: Maybe Text
  -- ^ Operator
  -> Version
  -- ^ Hypothetical required version
  -> Version
  -- ^ Hypothetical current version
  -> Either String Bool
runRequiredVersion mOperator required current =
  (`isRequiredVersionSatisfied` current) <$> requiredVersionFromText rvText
  where rvText = maybe "" (<> " ") mOperator <> pack (showVersion required)
