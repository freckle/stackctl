module Stackctl.AWS.Core
  ( AwsEnv
  , HasAwsEnv (..)
  , awsEnvDiscover
  , awsSimple
  , awsSend
  , awsPaginate
  , awsAwait
  , awsAssumeRole

    -- * Modifiers on 'AwsEnv'
  , awsWithin
  , awsTimeout

    -- * 'Amazonka' extensions
  , AccountId (..)

    -- * Error-handling
  , handlingServiceError
  , formatServiceError

    -- * 'Amazonka'/'ResourceT' re-exports
  , Region (..)
  , FromText (..)
  , ToText (..)
  , MonadResource
  ) where

import Stackctl.Prelude hiding (timeout)

import Amazonka hiding (LogLevel (..))
import qualified Amazonka as AWS
import Amazonka.Auth.Keys (fromSession)
import Amazonka.Data.Text (FromText (..), ToText (..))
import Amazonka.Env (env_logger, env_region)
import Amazonka.STS.AssumeRole
import Conduit (ConduitM)
import Control.Monad.Logger (defaultLoc, toLogStr)
import Control.Monad.Trans.Resource (MonadResource)
import Stackctl.AWS.Orphans ()

newtype AwsEnv = AwsEnv
  { unAwsEnv :: Env
  }

unL :: Lens' AwsEnv Env
unL = lens unAwsEnv $ \x y -> x {unAwsEnv = y}

awsEnvDiscover :: MonadLoggerIO m => m AwsEnv
awsEnvDiscover = do
  env <- liftIO $ newEnv discover
  AwsEnv <$> configureLogging env

configureLogging :: MonadLoggerIO m => Env -> m Env
configureLogging env = do
  loggerIO <- askLoggerIO

  let logger level = do
        loggerIO
          defaultLoc -- TODO: there may be a way to get a CallStack/Loc
          "Amazonka"
          ( case level of
              AWS.Info -> LevelInfo
              AWS.Error -> LevelError
              AWS.Debug -> LevelDebug
              AWS.Trace -> LevelOther "trace"
          )
          . toLogStr
  pure $ env & env_logger .~ logger

class HasAwsEnv env where
  awsEnvL :: Lens' env AwsEnv

instance HasAwsEnv AwsEnv where
  awsEnvL = id

awsSimple
  :: ( MonadResource m
     , MonadReader env m
     , HasAwsEnv env
     , AWSRequest a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => Text
  -> a
  -> (AWSResponse a -> Maybe b)
  -> m b
awsSimple name req post = do
  resp <- awsSend req
  maybe (throwString err) pure $ post resp
 where
  err = unpack name <> " successful, but processing the response failed"

awsSend
  :: ( MonadResource m
     , MonadReader env m
     , HasAwsEnv env
     , AWSRequest a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> m (AWSResponse a)
awsSend req = do
  AwsEnv env <- view awsEnvL
  send env req

awsPaginate
  :: ( MonadResource m
     , MonadReader env m
     , HasAwsEnv env
     , AWSPager a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> ConduitM () (AWSResponse a) m ()
awsPaginate req = do
  AwsEnv env <- view awsEnvL
  paginateEither env req >>= hoistEither

hoistEither :: MonadIO m => Either Error a -> m a
hoistEither = either (liftIO . throwIO) pure

awsAwait
  :: ( MonadResource m
     , MonadReader env m
     , HasAwsEnv env
     , AWSRequest a
     , Typeable a
     )
  => Wait a
  -> a
  -> m Accept
awsAwait w req = do
  AwsEnv env <- view awsEnvL
  await env w req

awsAssumeRole
  :: (MonadResource m, MonadReader env m, HasAwsEnv env)
  => Text
  -- ^ Role ARN
  -> Text
  -- ^ Session name
  -> m a
  -- ^ Action to run as the assumed role
  -> m a
awsAssumeRole role sessionName f = do
  let req = newAssumeRole role sessionName

  assumeEnv <- awsSimple "sts:AssumeRole" req $ \resp -> do
    let creds = resp ^. assumeRoleResponse_credentials
    token <- creds ^. authEnv_sessionToken

    let
      accessKeyId = creds ^. authEnv_accessKeyId
      secretAccessKey = creds ^. authEnv_secretAccessKey . _Sensitive

    pure $ fromSession accessKeyId secretAccessKey $ token ^. _Sensitive

  local (awsEnvL . unL %~ assumeEnv) f

awsWithin :: (MonadReader env m, HasAwsEnv env) => Region -> m a -> m a
awsWithin r = local $ awsEnvL . unL . env_region .~ r

awsTimeout :: (MonadReader env m, HasAwsEnv env) => Seconds -> m a -> m a
awsTimeout t = local $ over (awsEnvL . unL) (globalTimeout t)

newtype AccountId = AccountId
  { unAccountId :: Text
  }
  deriving newtype (Eq, Ord, Show, ToJSON)

-- | Handle 'ServiceError', log it and 'exitFailure'
--
-- This is useful at the top-level of the app, where we'd be crashing anyway. It
-- makes things more readable and easier to debug.
handlingServiceError :: (MonadUnliftIO m, MonadLogger m) => m a -> m a
handlingServiceError =
  handleJust @_ @SomeException (^? _ServiceError) $ \e -> do
    logError
      $ "Exiting due to AWS Service error"
      :# [ "code" .= toText (e ^. serviceError_code)
         , "message" .= fmap toText (e ^. serviceError_message)
         , "requestId" .= fmap toText (e ^. serviceError_requestId)
         ]
    exitFailure

formatServiceError :: ServiceError -> Text
formatServiceError e =
  mconcat
    [ toText $ e ^. serviceError_code
    , maybe "" ((": " <>) . toText) $ e ^. serviceError_message
    , maybe "" (("\nRequest Id: " <>) . toText) $ e ^. serviceError_requestId
    ]
