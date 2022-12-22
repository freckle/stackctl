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
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid (Last(..))
import qualified Data.Text as T
import Stackctl.AWS
import Stackctl.Action

data StackSpecYaml = StackSpecYaml
  { ssyDescription :: Maybe StackDescription
  , ssyTemplate :: FilePath
  , ssyDepends :: Maybe [StackName]
  , ssyActions :: Maybe [Action]
  , ssyParameters :: Maybe ParametersYaml
  , ssyCapabilities :: Maybe [Capability]
  , ssyTags :: Maybe TagsYaml
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON StackSpecYaml where
  parseJSON = genericParseJSON $ aesonPrefix id

instance ToJSON StackSpecYaml where
  toJSON = genericToJSON $ aesonPrefix id
  toEncoding = genericToEncoding $ aesonPrefix id

newtype ParametersYaml = ParametersYaml
  { unParametersYaml :: [ParameterYaml]
  }
  deriving stock (Eq, Show)

instance Semigroup ParametersYaml where
  ParametersYaml as <> ParametersYaml bs =
    ParametersYaml
      $ map (uncurry ParameterYaml)
      $ KeyMap.toList
      $ KeyMap.fromListWith (<>)
      $ map (pyKey &&& pyValue)
      $ bs -- flipped to make sure Last-wins
      <> as

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

instance ToJSON ParametersYaml where
  toJSON = object . parametersYamlPairs
  toEncoding = pairs . mconcat . parametersYamlPairs

parametersYamlPairs :: KeyValue kv => ParametersYaml -> [kv]
parametersYamlPairs = map parameterYamlPair . unParametersYaml

parametersYaml :: [ParameterYaml] -> ParametersYaml
parametersYaml = ParametersYaml

data ParameterYaml = ParameterYaml
  { pyKey :: Key
  , pyValue :: Last ParameterValue
  }
  deriving stock (Eq, Show)

instance FromJSON ParameterYaml where
  parseJSON = withObject "Parameter" $ \o ->
    (mkParameterYaml <$> o .: "Name" <*> o .:? "Value")
      <|> (mkParameterYaml <$> o .: "ParameterKey" <*> o .:? "ParameterValue")

parameterYamlPair :: KeyValue kv => ParameterYaml -> kv
parameterYamlPair ParameterYaml {..} = pyKey .= pyValue

mkParameterYaml :: Text -> Maybe ParameterValue -> ParameterYaml
mkParameterYaml k = ParameterYaml (Key.fromText k) . Last

parameterYaml :: Parameter -> Maybe ParameterYaml
parameterYaml p = do
  k <- p ^. parameter_parameterKey
  let mv = p ^. parameter_parameterValue
  pure $ mkParameterYaml k $ ParameterValue <$> mv

unParameterYaml :: ParameterYaml -> Parameter
unParameterYaml (ParameterYaml k v) =
  makeParameter (Key.toText k) $ unParameterValue <$> getLast v

newtype ParameterValue = ParameterValue
  { unParameterValue :: Text
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, ToJSON)

instance FromJSON ParameterValue where
  parseJSON = \case
    String x -> pure $ ParameterValue x
    Number x -> pure $ ParameterValue $ dropSuffix ".0" $ pack $ show x
    x -> fail $ "Expected String or Number, got: " <> show x

newtype TagsYaml = TagsYaml
  { unTagsYaml :: [TagYaml]
  }
  deriving stock (Eq, Show)

instance Semigroup TagsYaml where
  TagsYaml as <> TagsYaml bs =
    TagsYaml
      $ map (TagYaml . uncurry newTag)
      $ HashMap.toList
      $ HashMap.fromList
      $ map (toPair . unTagYaml)
      $ as
      <> bs
   where
    toPair :: Tag -> (Text, Text)
    toPair = (^. tag_key) &&& (^. tag_value)

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

instance ToJSON TagsYaml where
  toJSON = object . tagsYamlPairs
  toEncoding = pairs . mconcat . tagsYamlPairs

tagsYamlPairs :: KeyValue kv => TagsYaml -> [kv]
tagsYamlPairs = map tagYamlPair . unTagsYaml

tagsYaml :: [TagYaml] -> TagsYaml
tagsYaml = TagsYaml

newtype TagYaml = TagYaml
  { unTagYaml :: Tag
  }
  deriving newtype (Eq, Show)

instance FromJSON TagYaml where
  parseJSON = withObject "Tag" $ \o -> do
    t <- newTag <$> o .: "Key" <*> o .: "Value"
    pure $ TagYaml t

tagYamlPair :: KeyValue kv => TagYaml -> kv
tagYamlPair (TagYaml t) = Key.fromText (t ^. tag_key) .= (t ^. tag_value)

dropSuffix :: Text -> Text -> Text
dropSuffix suffix t = fromMaybe t $ T.stripSuffix suffix t
