module Stackctl.AWS.STS
  ( awsGetCallerIdentityAccount
  ) where

import Stackctl.Prelude

import Amazonka.STS.GetCallerIdentity
import Stackctl.AWS.Core as AWS

awsGetCallerIdentityAccount :: (MonadIO m, MonadAWS m) => m AccountId
awsGetCallerIdentityAccount = do
  AWS.simple newGetCallerIdentity $ \resp -> do
    AccountId <$> resp ^. getCallerIdentityResponse_account
