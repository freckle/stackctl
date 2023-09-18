module Stackctl.RemovedStack
  ( inferRemovedStacks
  ) where

import Stackctl.Prelude

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Stackctl.AWS.CloudFormation
import Stackctl.AWS.Core as AWS
import Stackctl.AWS.Scope
import Stackctl.FilterOption
import UnliftIO.Directory (doesFileExist)

inferRemovedStacks
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadReader env m
     , HasAwsScope env
     , HasFilterOption env
     )
  => m [Stack]
inferRemovedStacks = do
  scope <- view awsScopeL
  paths <- view $ filterOptionL . to filterOptionToPaths
  catMaybes <$> traverse (findRemovedStack scope) paths

findRemovedStack
  :: (MonadUnliftIO m, MonadAWS m)
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
