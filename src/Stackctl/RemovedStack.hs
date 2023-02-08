module Stackctl.RemovedStack
  ( RemovedStack(..)
  , inferRemovedStacks
  ) where

import Stackctl.Prelude

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Stackctl.AWS.CloudFormation
import Stackctl.AWS.Core
import Stackctl.AWS.Scope
import Stackctl.FilterOption
import UnliftIO.Directory (doesFileExist)

data RemovedStack = RemovedStack
  { rsPath :: FilePath
  , rsStackName :: StackName
  }

inferRemovedStacks
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasAwsEnv env
     , HasAwsScope env
     , HasFilterOption env
     )
  => m [Stack]
inferRemovedStacks = do
  scope <- view awsScopeL
  paths <- view $ filterOptionL . to filterOptionToFilePaths
  catMaybes <$> traverse (findRemovedStack scope) paths

findRemovedStack
  :: (MonadUnliftIO m, MonadResource m, MonadReader env m, HasAwsEnv env)
  => AwsScope
  -> FilePath
  -> m (Maybe Stack)
findRemovedStack scope path = runMaybeT $ do
  -- The filter is a full path to a specification in the current
  -- account/region...
  stackName <- hoistMaybe $ awsScopeSpecStackName scope path

  -- that no longer exists...
  guard . not =<< doesFileExist path

  -- but the Stack it would point to does
  MaybeT $ awsCloudFormationDescribeStackMaybe stackName
