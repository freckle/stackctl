{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Stackctl.StackSpecYamlSpec
  ( spec
  ) where

import Stackctl.Prelude

import qualified Data.Yaml as Yaml
import Stackctl.AWS
import Stackctl.StackSpecYaml
import Test.Hspec

spec :: Spec
spec = do
  describe "decoding Yaml" $ do
    it "reads String parameters" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        [ "Template: foo.yaml\n"
        , "Parameters:\n"
        , "  - ParameterKey: Foo\n"
        , "    ParameterValue: Bar\n"
        ]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"

    it "reads Number parameters without decimals" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        [ "Template: foo.yaml\n"
        , "Parameters:\n"
        , "  - ParameterKey: Port\n"
        , "    ParameterValue: 80\n"
        ]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Port"
      param ^. parameter_parameterValue `shouldBe` Just "80"

    it "reads Number parameters with decimals" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        [ "Template: foo.yaml\n"
        , "Parameters:\n"
        , "  - ParameterKey: Pie\n"
        , "    ParameterValue: 3.14\n"
        ]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Pie"
      param ^. parameter_parameterValue `shouldBe` Just "3.14"

    it "has informative errors" $ do
      let
        Left ex = Yaml.decodeEither' @StackSpecYaml $ mconcat
          [ "Template: foo.yaml\n"
          , "Parameters:\n"
          , "  - ParameterKey: Norway\n"
          , "    ParameterValue: no\n"
          ]

      show ex
        `shouldBe` "AesonException \"Error in $.Parameters[0].ParameterValue: Expected String or Number, got: Bool False\""

    it "has informative errors in Object form" $ do
      let
        Left ex = Yaml.decodeEither' @StackSpecYaml $ mconcat
          ["Template: foo.yaml\n", "Parameters:\n", "  Norway: no\n"]

      show ex
        `shouldBe` "AesonException \"Error in $.Parameters.Norway: Expected String or Number, got: Bool False\""

    it "handles null Value" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        [ "Template: foo.yaml\n"
        , "Parameters:\n"
        , "  - ParameterKey: Foo\n"
        , "    ParameterValue: null\n"
        ]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Nothing

    it "handles missing Value" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        ["Template: foo.yaml\n", "Parameters:\n", "  - ParameterKey: Foo\n"]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Nothing

    it "also accepts CloudGenesis formatted values" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow $ mconcat
        [ "Template: foo.yaml\n"
        , "Parameters:\n"
        , "  - Name: Foo\n"
        , "    Value: Bar\n"
        ]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"

    it "also accepts objects" $ do
      StackSpecYaml {..} <- Yaml.decodeThrow
        $ mconcat ["Template: foo.yaml\n", "Parameters:\n", "  Foo: Bar\n"]

      let
        Just [param] = map unParameterYaml . unParametersYaml <$> ssyParameters
      param ^. parameter_parameterKey `shouldBe` Just "Foo"
      param ^. parameter_parameterValue `shouldBe` Just "Bar"
