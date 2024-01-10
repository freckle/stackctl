module Datadog
  ( Datadog (..)
  , HasDatadog (..)
  , withDummyDatadog
  , Keys (..)
  , withDatadog
  , increment
  , Duration
  , Seconds
  , newDuration
  , duration
  )
where

import Prelude

import Control.Lens (Lens', view, (&), (.~))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Network.Datadog (Environment, Keys (..))
import Network.StatsD.Datadog
  ( MetricName (..)
  , MetricType (..)
  , StatsClient
  , ToMetricValue (..)
  , metric
  , send
  , tag
  , tags
  )

data Datadog = DummyDatadog | Datadog StatsClient Environment

withDummyDatadog :: (Datadog -> m a) -> m a
withDummyDatadog f = f DummyDatadog

withDatadog :: Keys -> (Datadog -> m a) -> m a
withDatadog = undefined

class HasDatadog env where
  datadogL :: Lens' env Datadog

instance HasDatadog Datadog where
  datadogL = id

increment
  :: ( MonadIO m
     , MonadReader env m
     , HasDatadog env
     )
  => Text
  -> [(Text, Text)]
  -> m ()
increment = sendMetric Counter (1 :: Int)

newtype Duration t = Duration
  { unDuration :: NominalDiffTime
  }

newDuration :: NominalDiffTime -> Duration t
newDuration = Duration

durationToSeconds :: Duration Seconds -> Int
durationToSeconds = round . nominalDiffTimeToSeconds . unDuration

data Seconds

instance ToMetricValue (Duration Seconds) where
  encodeValue = encodeValue . durationToSeconds

duration
  :: ( MonadIO m
     , MonadReader env m
     , HasDatadog env
     , ToMetricValue (Duration t)
     )
  => Duration t
  -> Text
  -> [(Text, Text)]
  -> m ()
duration = sendMetric Histogram

sendMetric
  :: ( MonadIO m
     , MonadReader env m
     , HasDatadog env
     , ToMetricValue v
     )
  => MetricType
  -> v
  -> Text
  -> [(Text, Text)]
  -> m ()
sendMetric mtype mval mname mtags = do
  dd <- view datadogL
  case dd of
    DummyDatadog -> pure ()
    Datadog sc _ ->
      send sc $
        metric (MetricName mname) mtype mval
          & tags .~ map (uncurry tag) mtags
