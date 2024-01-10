module Stackctl.Telemetry
  ( MonadTelemetry (..)
  , Deployment (..)
  , DeploymentResult (..)
  , NoTelemetry (..)
  ) where

import Stackctl.Prelude

import Data.Time (UTCTime)

data Deployment = Deployment
  { deploymentStartedAt :: UTCTime
  , deploymentResult :: DeploymentResult
  }

data DeploymentResult
  = DeploymentNoChange
  | DeploymentSucceeded UTCTime
  | DeploymentFailed UTCTime String

class Monad m => MonadTelemetry m where
  recordDeployment :: Deployment -> m ()

newtype NoTelemetry m a = NoTelemetry
  { unNoTelemetry :: m a
  }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => MonadTelemetry (NoTelemetry m) where
  recordDeployment _ = pure ()
