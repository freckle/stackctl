module Stackctl.AWS.Core
  ( MonadAWS
  , send
  , paginate
  , await
  , withAuth
  , localEnv

    -- * "Control.Monad.AWS" extensions
  , simple
  , discover
  , withAssumedRole

    -- * Error-handling
  , handlingServiceError
  , formatServiceError

    -- * "Amazonka" extensions
  , AccountId (..)

    -- * "Amazonka" re-exports
  , Region (..)
  , FromText (..)
  , ToText (..)

    -- * Deprecated
  , assumeRole
  ) where

import Stackctl.Prelude

import Amazonka
  ( AWSRequest
  , AWSResponse
  , Env' (auth)
  , Region
  , ServiceError
  , serviceError_code
  , serviceError_message
  , serviceError_requestId
  , _Sensitive
  , _ServiceError
  )
import qualified Amazonka
import Amazonka.Auth.Background (fetchAuthInBackground)
import Amazonka.Auth.Keys (fromSession)
import Amazonka.Data.Text (FromText (..), ToText (..))
import qualified Amazonka.Env as Amazonka
import Amazonka.STS.AssumeRole
import Control.Monad.AWS
import Control.Monad.Logger (defaultLoc, toLogStr)
import Data.Typeable (typeRep)
import Stackctl.AWS.Orphans ()
import UnliftIO.Exception.Lens (handling)

discover :: MonadLoggerIO m => m Amazonka.Env
discover = do
  env <- liftIO $ Amazonka.newEnv Amazonka.discover
  loggerIO <- askLoggerIO

  let logger level = do
        loggerIO
          defaultLoc
          "Amazonka"
          ( case level of
              Amazonka.Info -> LevelInfo
              Amazonka.Error -> LevelError
              Amazonka.Debug -> LevelDebug
              Amazonka.Trace -> LevelOther "trace"
          )
          . toLogStr
  pure $ env & Amazonka.env_logger .~ logger

simple
  :: forall a m b
   . ( HasCallStack
     , MonadIO m
     , MonadAWS m
     , AWSRequest a
     , Typeable a
     , Typeable (AWSResponse a)
     )
  => a
  -> (AWSResponse a -> Maybe b)
  -> m b
simple req post = do
  resp <- send req

  let
    name = show $ typeRep $ Proxy @a
    err = name <> " successful, but processing the response failed"

  maybe (throwString err) pure $ post resp

-- | Use 'withAssumedRole' instead
--
-- This function is like 'withAssumedRole' except it doesn't spawn a background
-- thread to keep credentials refreshed. You may encounter expired credentials
-- if the block used under 'assumeRole' goes for long enough.
assumeRole
  :: (MonadIO m, MonadAWS m)
  => Text
  -- ^ Role ARN
  -> Text
  -- ^ Session name
  -> m a
  -- ^ Action to run as the assumed role
  -> m a
assumeRole role sessionName f = do
  let req = newAssumeRole role sessionName

  assumeEnv <- simple req $ \resp -> do
    let creds = resp ^. assumeRoleResponse_credentials
    token <- creds ^. Amazonka.authEnv_sessionToken

    let
      accessKeyId = creds ^. Amazonka.authEnv_accessKeyId
      secretAccessKey = creds ^. Amazonka.authEnv_secretAccessKey . _Sensitive
      sessionToken = token ^. _Sensitive

    pure $ fromSession accessKeyId secretAccessKey sessionToken

  localEnv assumeEnv f
{-# DEPRECATED assumeRole "Use withAssumedRole instead" #-}

-- | Assume a role using the @sts:AssumeRole@ API and run an action
withAssumedRole
  :: (MonadUnliftIO m, MonadAWS m)
  => Text
  -- ^ Role ARN
  -> Text
  -- ^ Role session name
  -> m a
  -- ^ Action to run as the assumed role
  -> m a
withAssumedRole roleArn roleSessionName f = do
  keys <- withRunInIO $ \runInIO -> do
    let getCredentials = do
          resp <-
            runInIO
              $ send
              $ newAssumeRole roleArn roleSessionName
          pure $ resp ^. assumeRoleResponse_credentials

    fetchAuthInBackground getCredentials

  localEnv (\env -> env {auth = Identity keys}) f

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
  handling _ServiceError $ \e -> do
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
