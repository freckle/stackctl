module Stackctl.Telemetry.Tags
  ( TelemetryTags
  , singleton
  , fromList
  , toDatadog
  , HasTelemetryTags (..)
  , getTelemetryTags
  ) where

import Stackctl.Prelude

import Control.Lens (views)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.Datadog.Types as Datadog
import Stackctl.AWS.Scope
import System.Process.Typed
import UnliftIO.Environment (lookupEnv)

newtype TelemetryTags = TelemetryTags
  { unTelemetryTags :: [(Text, Text)]
  }
  deriving newtype (Semigroup, Monoid)

singleton :: Text -> Text -> TelemetryTags
singleton k v = TelemetryTags [(k, v)]

fromList :: [(Text, Text)] -> TelemetryTags
fromList = foldMap $ uncurry singleton

toDatadog :: TelemetryTags -> [Datadog.Tag]
toDatadog = map (uncurry Datadog.KeyValueTag) . unTelemetryTags

class HasTelemetryTags env where
  telemetryTagsL :: Lens' env TelemetryTags

instance HasTelemetryTags TelemetryTags where
  telemetryTagsL = id

getTelemetryTags
  :: ( MonadIO m
     , MonadReader env m
     , HasAwsScope env
     , HasTelemetryTags env
     )
  => m TelemetryTags
getTelemetryTags =
  mconcat
    <$> sequence
      [ view telemetryTagsL
      , views awsScopeL fromAwsScope
      , maybe mempty (singleton "user" . pack) <$> lookupEnv "USER"
      , maybe mempty (singleton "path" . pack) <$> lookupEnv "PWD"
      , maybe mempty (singleton "host") <$> chompProcessText "hostname" []
      ]

fromAwsScope :: AwsScope -> TelemetryTags
fromAwsScope = undefined

chompProcessText :: MonadIO m => String -> [String] -> m (Maybe Text)
chompProcessText cmd args = do
  (_ec, out, _err) <- readProcess $ proc cmd args
  pure $ Just $ T.dropWhileEnd (== '\n') $ decodeUtf8 $ BSL.toStrict out
