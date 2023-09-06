module Stackctl.OneOrListOfSpec
  ( spec
  ) where

import Stackctl.Prelude

import Data.Aeson
import qualified Data.Yaml as Yaml
import Stackctl.OneOrListOf
import Test.Hspec

data ExampleObject = ExampleObject
  { oneOf :: OneOrListOf Text
  , listOf :: OneOrListOf Text
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- N.B. the sorting and indentation must match what encode will do in order for
-- the round-trip spec to pass.
exampleBS :: ByteString
exampleBS =
  mconcat
    [ "listOf:\n"
    , "- one\n"
    , "- two\n"
    , "oneOf: one\n"
    ]

spec :: Spec
spec = do
  it "Foldable" $ do
    ExampleObject {..} <- Yaml.decodeThrow exampleBS

    toList oneOf `shouldBe` ["one"]

    toList listOf `shouldBe` ["one", "two"]

  it "Semigroup" $ do
    ExampleObject {..} <- Yaml.decodeThrow exampleBS

    toList (oneOf <> listOf) `shouldBe` ["one", "one", "two"]

  it "From/ToJSON" $ do
    decoded <- Yaml.decodeThrow @_ @ExampleObject exampleBS

    Yaml.encode decoded `shouldBe` exampleBS
