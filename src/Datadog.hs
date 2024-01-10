module Datadog
  ( Credentials (..)
  , Datadog (..)
  , HasDatadog (..)
  , mkWithDatadog
  )
where

import Prelude

import Control.Lens (Lens')
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Data.Text (Text)
import Network.Datadog
import Network.Datadog.Internal (DatadogCredentials)
import Network.Datadog.Types

data Credentials
  = -- | API Key only
    CredentialsWrite Text
  | -- | API and App Keys
    CredentialsReadWrite Text Text

data Datadog
  = NullDatadog
  | DatadogWrite (DatadogClient Write)
  | DatadogReadWrite (DatadogClient ReadWrite)

class HasDatadog env where
  datadogL :: Lens' env Datadog

instance HasDatadog Datadog where
  datadogL = id

mkWithDatadog
  :: MonadUnliftIO m => Maybe Credentials -> ((Datadog -> m a) -> m a)
mkWithDatadog = \case
  Nothing -> \f -> f NullDatadog
  Just (CredentialsWrite apiKey) -> \f -> do
    withDatadog' (writeCredentials apiKey) $ f . DatadogWrite
  Just (CredentialsReadWrite apiKey appKey) -> \f -> do
    withDatadog' (readWriteCredentials apiKey appKey) $ f . DatadogReadWrite

withDatadog'
  :: (MonadUnliftIO m, DatadogCredentials k)
  => k
  -> (DatadogClient k -> m a)
  -> m a
withDatadog' creds f =
  withRunInIO $ \runInIO -> withDatadog creds $ runInIO . f
