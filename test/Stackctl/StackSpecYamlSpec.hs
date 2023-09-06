{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Stackctl.StackSpecYamlSpec
  ( spec
  ) where

import Stackctl.Prelude

import Data.Aeson
import qualified Data.Yaml as Yaml
import Stackctl.AWS
import Stackctl.Action
import Stackctl.StackSpecYaml
import Test.Hspec

spec :: Spec
spec = do
  describe "From/ToJSON" $ do
    it "round trips" $ do
      let yaml =
            StackSpecYaml
              { ssyDescription = Just $ StackDescription "Testing Stack"
              , ssyTemplate = "path/to/template.yaml"
              , ssyDepends = Just [StackName "a-stack", StackName "another-stack"]
              , ssyActions =
                  Just
                    [newAction PostDeploy [InvokeLambdaByName "a-lambda"]]
              , ssyParameters =
                  Just
                    $ parametersYaml
                    $ mapMaybe
                      parameterYaml
                      [makeParameter "PKey" $ Just "PValue"]
              , ssyCapabilities = Just [Capability_CAPABILITY_IAM]
              , ssyTags = Just $ tagsYaml [TagYaml $ newTag "TKey" "TValue"]
              }

      eitherDecode (encode yaml) `shouldBe` Right yaml

  describe "decoding Yaml" $ do
    it "reads String parameters" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            [ "Template: foo.yaml\n"
            , "Parameters:\n"
            , "  - ParameterKey: Foo\n"
            , "    ParameterValue: Bar\n"
            ]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"

    it "reads Number parameters without decimals" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            [ "Template: foo.yaml\n"
            , "Parameters:\n"
            , "  - ParameterKey: Port\n"
            , "    ParameterValue: 80\n"
            ]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Port"
      param ^. parameter_parameterValue `shouldBe` Just "80"

    it "reads Number parameters with decimals" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            [ "Template: foo.yaml\n"
            , "Parameters:\n"
            , "  - ParameterKey: Pie\n"
            , "    ParameterValue: 3.14\n"
            ]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Pie"
      param ^. parameter_parameterValue `shouldBe` Just "3.14"

    it "has informative errors" $ do
      let Left ex =
            Yaml.decodeEither' @StackSpecYaml
              $ mconcat
                [ "Template: foo.yaml\n"
                , "Parameters:\n"
                , "  - ParameterKey: Norway\n"
                , "    ParameterValue: no\n"
                ]

      show ex
        `shouldBe` "AesonException \"Error in $.Parameters[0].ParameterValue: Expected String or Number, got: Bool False\""

    it "has informative errors in Object form" $ do
      let Left ex =
            Yaml.decodeEither' @StackSpecYaml
              $ mconcat
                ["Template: foo.yaml\n", "Parameters:\n", "  Norway: no\n"]

      show ex
        `shouldBe` "AesonException \"Error in $.Parameters.Norway: Expected String or Number, got: Bool False\""

    it "handles null Value" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            [ "Template: foo.yaml\n"
            , "Parameters:\n"
            , "  - ParameterKey: Foo\n"
            , "    ParameterValue: null\n"
            ]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Nothing

    it "handles missing Value" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            ["Template: foo.yaml\n", "Parameters:\n", "  - ParameterKey: Foo\n"]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Nothing

    it "also accepts CloudGenesis formatted values" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat
            [ "Template: foo.yaml\n"
            , "Parameters:\n"
            , "  - Name: Foo\n"
            , "    Value: Bar\n"
            ]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"

    it "also accepts objects" $ do
      StackSpecYaml {..} <-
        Yaml.decodeThrow
          $ mconcat ["Template: foo.yaml\n", "Parameters:\n", "  Foo: Bar\n"]

      let Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"

  describe "ParametersYaml" $ do
    it "has overriding Semigroup semantics" $ do
      let
        a = parametersYaml []
        b =
          parametersYaml
            $ catMaybes [parameterYaml $ makeParameter "Key" (Just "B")]
        c =
          parametersYaml
            $ catMaybes [parameterYaml $ makeParameter "Key" (Just "C")]
        d =
          parametersYaml
            $ catMaybes [parameterYaml $ makeParameter "Key" Nothing]

      a <> b `shouldBe` b -- keeps keys in B
      b <> c `shouldBe` c -- C overrides B (Last)
      c <> d `shouldBe` c -- C overrides D (Just)
      d <> c `shouldBe` c -- C overrides D (Just)
  describe "TagsYaml" $ do
    it "has overriding Semigroup semantics" $ do
      let
        a = tagsYaml []
        b = tagsYaml [TagYaml $ newTag "Key" "B"]
        c = tagsYaml [TagYaml $ newTag "Key" "C"]

      a <> b `shouldBe` b -- keeps keys in B
      b <> c `shouldBe` c -- C overrides B (Last)
