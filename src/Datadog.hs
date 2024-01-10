module Datadog
  ( Credentials (..)
  , mkWithDatadog
  , Datadog (..)
  , HasDatadog (..)
  )
where

import Prelude

import Control.Lens (Lens')
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Network.Datadog.Types

data Credentials
  = -- | API Key only
    CredentialsWrite Text
  | -- | API and App Keys
    CredentialsReadWrite Text Text

mkWithDatadog :: MonadIO m => Maybe Credentials -> m ((Datadog -> m a) -> m a)
mkWithDatadog = \case
  Nothing -> pure $ \f -> f NullDatadog
  _ -> undefined

data Datadog
  = NullDatadog
  | DatadogWrite (DatadogClient Write)
  | DatadogReadWrite (DatadogClient ReadWrite)

class HasDatadog env where
  datadogL :: Lens' env Datadog

instance HasDatadog Datadog where
  datadogL = id
