module Stackctl.AWS.Core
  (
  -- * AWS via 'MonadReader'
    AwsEnv
  , HasAwsEnv(..)
  , awsEnvDiscover
  , awsSimple
  , awsSend
  , awsPaginate
  , awsAwait

  -- * Modifiers on 'AwsEnv'
  , awsWithin

  -- * 'Amazonka' extensions
  , AccountId(..)

  -- * 'Amazonka'/'ResourceT' re-exports
  , Region(..)
  , FromText(..)
  , ToText(..)
  , MonadResource
  , HasResourceMap(..)
  , ResourceMap
  , withResourceMap
  ) where

import Stackctl.Prelude2

import Amazonka
import Conduit (ConduitM)
import Control.Monad.Trans.Resource (MonadResource)
import RIO.Orphans as X (HasResourceMap(..), ResourceMap, withResourceMap)
import Stackctl.AWS.Orphans ()

newtype AwsEnv = AwsEnv
  { unAwsEnv :: Env
  }

unL :: Lens' AwsEnv Env
unL = lens unAwsEnv $ \x y -> x { unAwsEnv = y }

awsEnvDiscover :: MonadIO m => m AwsEnv
awsEnvDiscover = liftIO $ AwsEnv <$> newEnv discover

class HasAwsEnv env where
  awsEnvL :: Lens' env AwsEnv

awsSimple
  :: (MonadResource m, MonadReader env m, HasAwsEnv env, AWSRequest a)
  => Text
  -> a
  -> (AWSResponse a -> Maybe b)
  -> m b
awsSimple name req post = do
  resp <- awsSend req
  maybe (throwString err) pure $ post resp
  where err = unpack name <> " successful, but processing the response failed"

awsSend
  :: (MonadResource m, MonadReader env m, HasAwsEnv env, AWSRequest a)
  => a
  -> m (AWSResponse a)
awsSend req = do
  AwsEnv env <- view awsEnvL
  send env req

awsPaginate
  :: (MonadResource m, MonadReader env m, HasAwsEnv env, AWSPager a)
  => a
  -> ConduitM () (AWSResponse a) m ()
awsPaginate req = do
  AwsEnv env <- view awsEnvL
  paginateEither env req >>= hoistEither

hoistEither :: MonadIO m => Either Error a -> m a
hoistEither = either (liftIO . throwIO) pure

awsAwait
  :: (MonadResource m, MonadReader env m, HasAwsEnv env, AWSRequest a)
  => Wait a
  -> a
  -> m Accept
awsAwait w req = do
  AwsEnv env <- view awsEnvL
  await env w req

awsWithin :: (MonadReader env m, HasAwsEnv env) => Region -> m a -> m a
awsWithin r = local $ over (awsEnvL . unL) (within r)

newtype AccountId = AccountId
  { unAccountId :: Text
  }
  deriving newtype (Eq, Ord, ToJSON)
