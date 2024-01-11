module Datadog
  ( Credentials (..)
  , HasCredentials (..)
  , ActualDatadog (..)
  )
where

import Prelude

import Control.Lens (Lens', view)
import Control.Monad (void)
import Control.Monad.IO.Unlift (MonadUnliftIO (..))
import Control.Monad.Reader (MonadReader)
import Data.Text (Text)
import Network.Datadog
import Network.Datadog.Internal (DatadogCredentials)
import Network.Datadog.Types

data Credentials
  = CredentialsNone
  | CredentialsWrite Write
  | CredentialsReadWrite ReadWrite

class HasCredentials env where
  credentialsL :: Lens' env Credentials

instance HasCredentials Credentials where
  credentialsL = id

newtype ActualDatadog m a = ActualDatadog
  { unActualDatadog :: m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader
    )

instance MonadTelemetry (ActualDatadog m a) where
  recordDeployment _ = do

withDatadog'
  :: (MonadUnliftIO m, MonadReader env m, HasCredentials env)
  => (forall k. DatadogClient k -> m a)
  -> m ()
withDatadog' f = do
  creds <- view credentialsL

  case creds of
    CredentialsNone -> pure ()
    CredentialsWrite w -> withRunInIO $ \runInIO -> do
      withDatadog w $ runInIO . void . f
    CredentialsReadWrite rw -> withRunInIO $ \runInIO -> do
      withDatadog rw $ runInIO . void . f
