{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Stackctl.ConfigSpec
  ( spec
  ) where

import Stackctl.Prelude

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS8
import Data.Version (showVersion)
import Paths_stackctl as Paths
import Stackctl.AWS (makeParameter, newTag)
import Stackctl.Config
import Stackctl.StackSpecYaml
import Test.Hspec

spec :: Spec
spec = do
  describe "loadConfigFromBytes" $ do
    it "loads a valid config" $ do
      let result =
            loadConfigFromLines
              [ "required_version: " <> BS8.pack (showVersion Paths.version)
              , "defaults:"
              , "  parameters:"
              , "    Some: Parameter"
              , "  tags:"
              , "    Some: Tag"
              ]

      case result of
        Left err -> do
          expectationFailure
            $ "Expected to load a Config, got error: " <> show err
        Right config -> do
          configParameters config
            `shouldBe` Just (toParametersYaml [("Some", Just "Parameter")])
          configTags config `shouldBe` Just (toTagsYaml [("Some", "Tag")])

  describe "applyConfig" $ do
    it "defaults missing Tags" $ do
      let
        specYaml =
          StackSpecYaml
            { ssyDescription = Nothing
            , ssyTemplate = ""
            , ssyDepends = Nothing
            , ssyActions = Nothing
            , ssyParameters = Nothing
            , ssyCapabilities = Nothing
            , ssyTags = Just $ toTagsYaml [("Hi", "There"), ("Keep", "Me")]
            }

        Right config =
          loadConfigFromBytes
            $ "defaults:"
              <> "\n  tags:"
              <> "\n    From: Defaults"
              <> "\n    Keep: \"You?\""

        Just tags = ssyTags (applyConfig config specYaml)

      tags
        `shouldBe` toTagsYaml
          [("From", "Defaults"), ("Hi", "There"), ("Keep", "Me")]

loadConfigFromLines :: MonadError ConfigError m => [ByteString] -> m Config
loadConfigFromLines = loadConfigFromBytes . mconcat . map (<> "\n")

toParametersYaml :: [(Text, Maybe Text)] -> ParametersYaml
toParametersYaml =
  parametersYaml . mapMaybe (parameterYaml . uncurry makeParameter)

toTagsYaml :: [(Text, Text)] -> TagsYaml
toTagsYaml = tagsYaml . map (TagYaml . uncurry newTag)
