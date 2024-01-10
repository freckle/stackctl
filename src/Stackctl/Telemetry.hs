{-# LANGUAGE NamedFieldPuns #-}

module Stackctl.Telemetry
  ( MonadTelemetry (..)
  , recordDeploymentNoChange
  , recordDeploymentSucceeded
  , recordDeploymentFailed
  , Deployment (..)
  , DeploymentResult (..)
  , NoTelemetry (..)
  ) where

import Stackctl.Prelude

import Data.Time (UTCTime, getCurrentTime)

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

recordDeploymentNoChange :: MonadTelemetry m => UTCTime -> m ()
recordDeploymentNoChange deploymentStartedAt =
  recordDeployment
    Deployment
      { deploymentStartedAt
      , deploymentResult = DeploymentNoChange
      }

recordDeploymentSucceeded :: (MonadIO m, MonadTelemetry m) => UTCTime -> m ()
recordDeploymentSucceeded deploymentStartedAt = do
  now <- liftIO getCurrentTime
  recordDeployment
    Deployment
      { deploymentStartedAt
      , deploymentResult = DeploymentSucceeded now
      }

recordDeploymentFailed
  :: (MonadIO m, MonadTelemetry m) => UTCTime -> String -> m ()
recordDeploymentFailed deploymentStartedAt err = do
  now <- liftIO getCurrentTime
  recordDeployment
    Deployment
      { deploymentStartedAt
      , deploymentResult = DeploymentFailed now err
      }
