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
  , ParameterYaml
  , parameterYaml
  , unParameterYaml
  , TagYaml(..)
  ) where

import Stackctl.Prelude

import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Text as T
import Stackctl.Action
import Stackctl.AWS

data StackSpecYaml = StackSpecYaml
  { ssyDescription :: Maybe StackDescription
  , ssyTemplate :: FilePath
  , ssyDepends :: Maybe [StackName]
  , ssyActions :: Maybe [Action]
  , ssyParameters :: Maybe [ParameterYaml]
  , ssyCapabilities :: Maybe [Capability]
  , ssyTags :: Maybe [TagYaml]
  }
  deriving stock Generic

instance FromJSON StackSpecYaml where
  parseJSON = genericParseJSON $ aesonPrefix id

instance ToJSON StackSpecYaml where
  toJSON = genericToJSON $ aesonPrefix id
  toEncoding = genericToEncoding $ aesonPrefix id

data ParameterYaml = ParameterYaml
  { _pyKey :: Text
  , _pyValue :: Maybe Text
  }

parameterYaml :: Parameter -> Maybe ParameterYaml
parameterYaml p = do
  k <- p ^. parameter_parameterKey
  pure $ ParameterYaml k $ p ^. parameter_parameterKey

unParameterYaml :: ParameterYaml -> Parameter
unParameterYaml (ParameterYaml k v) = makeParameter k v

instance FromJSON ParameterYaml where
  parseJSON = withObject "Parameter" $ \o ->
    (build <$> o .: "Name" <*> o .:? "Value")
      <|> (build <$> o .: "ParameterKey" <*> o .:? "ParameterValue")
    where build k v = ParameterYaml k $ unParameterValue <$> v

newtype ParameterValue = ParameterValue
  { unParameterValue :: Text
  }

instance FromJSON ParameterValue where
  parseJSON = \case
    String x -> pure $ ParameterValue x
    Number x -> pure $ ParameterValue $ dropSuffix ".0" $ pack $ show x
    x -> fail $ "Expected String or Number, got: " <> show x

instance ToJSON ParameterYaml where
  toJSON = object . parameterPairs
  toEncoding = pairs . mconcat . parameterPairs

parameterPairs :: KeyValue a => ParameterYaml -> [a]
parameterPairs (ParameterYaml k v) =
  ["ParameterKey" .= k, "ParameterValue" .= v]

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
