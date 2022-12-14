-- | Definition of our @stacks/<account>/<region>/<stack-name>.yaml@ format
--
-- @
-- Template: <path>
--
-- Depends:
-- - <string>
--
-- Parameters:
-- - ParameterKey: <string>
--   ParameterValue: <string>
--
-- Capabilities:
-- - <capability>
--
-- Tags:
-- - Key: <string>
--   Value: <string>
-- @
--
module Stackctl.StackSpecYaml
  ( StackSpecYaml(..)
  , ParametersYaml
  , parametersYaml
  , unParametersYaml
  , ParameterYaml
  , parameterYaml
  , unParameterYaml
  , TagsYaml
  , tagsYaml
  , unTagsYaml
  , TagYaml(..)
  ) where

import Stackctl.Prelude

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import Stackctl.Action
import Stackctl.AWS

data StackSpecYaml = StackSpecYaml
  { ssyDescription :: Maybe StackDescription
  , ssyTemplate :: FilePath
  , ssyDepends :: Maybe [StackName]
  , ssyActions :: Maybe [Action]
  , ssyParameters :: Maybe ParametersYaml
  , ssyCapabilities :: Maybe [Capability]
  , ssyTags :: Maybe TagsYaml
  }
  deriving stock Generic

instance FromJSON StackSpecYaml where
  parseJSON = genericParseJSON $ aesonPrefix id

instance ToJSON StackSpecYaml where
  toJSON = genericToJSON $ aesonPrefix id
  toEncoding = genericToEncoding $ aesonPrefix id

newtype ParametersYaml = ParametersYaml
  { unParametersYaml :: [ParameterYaml]
  }
  deriving newtype ToJSON

instance FromJSON ParametersYaml where
  parseJSON = \case
    Object o -> do
      -- NB. There are simpler ways to do this, but making sure we construct
      -- things such that we use (.:) to read the value from each key means that
      -- error messages will include "Parameters.{k}". See specs for an example.
      let parseKey k = ParameterYaml k <$> o .: k
      ParametersYaml <$> traverse parseKey (KeyMap.keys o)
    v@Array{} -> ParametersYaml <$> parseJSON v
    v -> typeMismatch err v
   where
    err =
      "Object"
        <> ", list of {ParameterKey, ParameterValue} Objects"
        <> ", or list of {Key, Value} Objects"

parametersYaml :: [ParameterYaml] -> ParametersYaml
parametersYaml = ParametersYaml

data ParameterYaml = ParameterYaml
  { _pyKey :: Key
  , _pyValue :: Maybe ParameterValue
  }

parameterYaml :: Parameter -> Maybe ParameterYaml
parameterYaml p = do
  k <- p ^. parameter_parameterKey
  pure
    $ ParameterYaml (Key.fromText k)
    $ ParameterValue
    <$> p
    ^. parameter_parameterKey

unParameterYaml :: ParameterYaml -> Parameter
unParameterYaml (ParameterYaml k v) =
  makeParameter (Key.toText k) $ unParameterValue <$> v

instance FromJSON ParameterYaml where
  parseJSON = withObject "Parameter" $ \o ->
    (ParameterYaml <$> o .: "Name" <*> o .:? "Value")
      <|> (ParameterYaml <$> o .: "ParameterKey" <*> o .:? "ParameterValue")

newtype ParameterValue = ParameterValue
  { unParameterValue :: Text
  }
  deriving newtype ToJSON

instance FromJSON ParameterValue where
  parseJSON = \case
    String x -> pure $ ParameterValue x
    Number x -> pure $ ParameterValue $ dropSuffix ".0" $ pack $ show x
    x -> fail $ "Expected String or Number, got: " <> show x

instance ToJSON ParameterYaml where
  toJSON = object . parameterPairs
  toEncoding = pairs . mconcat . parameterPairs

parameterPairs :: KeyValue a => ParameterYaml -> [a]
parameterPairs (ParameterYaml k v) = [k .= v]

newtype TagsYaml = TagsYaml
  { unTagsYaml :: [TagYaml]
  }
  deriving newtype ToJSON

instance FromJSON TagsYaml where
  parseJSON = \case
    Object o -> do
      let
        parseKey k = do
          t <- newTag (Key.toText k) <$> o .: k
          pure $ TagYaml t
      TagsYaml <$> traverse parseKey (KeyMap.keys o)
    v@Array{} -> TagsYaml <$> parseJSON v
    v -> typeMismatch err v
    where err = "Object or list of {Key, Value} Objects"

tagsYaml :: [TagYaml] -> TagsYaml
tagsYaml = TagsYaml

newtype TagYaml = TagYaml
  { unTagYaml :: Tag
  }

instance FromJSON TagYaml where
  parseJSON = withObject "Tag" $ \o -> do
    t <- newTag <$> o .: "Key" <*> o .: "Value"
    pure $ TagYaml t

instance ToJSON TagYaml where
  toJSON = object . tagPairs
  toEncoding = pairs . mconcat . tagPairs

tagPairs :: KeyValue a => TagYaml -> [a]
tagPairs (TagYaml t) = ["Key" .= (t ^. tag_key), "Value" .= (t ^. tag_value)]

dropSuffix :: Text -> Text -> Text
dropSuffix suffix t = fromMaybe t $ T.stripSuffix suffix t
