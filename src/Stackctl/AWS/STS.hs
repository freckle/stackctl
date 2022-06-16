module Stackctl.AWS.STS
  ( awsGetCallerIdentityAccount
  ) where

import Stackctl.Prelude2

import Amazonka.STS.GetCallerIdentity
import Stackctl.AWS.Core

awsGetCallerIdentityAccount
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => m AccountId
awsGetCallerIdentityAccount = do
  awsSimple "GetCallerIdentity" newGetCallerIdentity $ \resp -> do
    AccountId <$> resp ^. getCallerIdentityResponse_account
