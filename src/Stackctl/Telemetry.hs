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
import Stackctl.AWS.CloudFormation (StackName)

data Deployment = Deployment
  { deploymentStack :: StackName
  , deploymentStartedAt :: UTCTime
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

recordDeploymentNoChange :: MonadTelemetry m => StackName -> UTCTime -> m ()
recordDeploymentNoChange deploymentStack deploymentStartedAt =
  recordDeployment
    Deployment
      { deploymentStack
      , deploymentStartedAt
      , deploymentResult = DeploymentNoChange
      }

recordDeploymentSucceeded
  :: (MonadIO m, MonadTelemetry m) => StackName -> UTCTime -> m ()
recordDeploymentSucceeded deploymentStack deploymentStartedAt = do
  now <- liftIO getCurrentTime
  recordDeployment
    Deployment
      { deploymentStack
      , deploymentStartedAt
      , deploymentResult = DeploymentSucceeded now
      }

recordDeploymentFailed
  :: (MonadIO m, MonadTelemetry m) => StackName -> UTCTime -> String -> m ()
recordDeploymentFailed deploymentStack deploymentStartedAt err = do
  now <- liftIO getCurrentTime
  recordDeployment
    Deployment
      { deploymentStack
      , deploymentStartedAt
      , deploymentResult = DeploymentFailed now err
      }
