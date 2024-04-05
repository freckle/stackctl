module Stackctl.RemovedStack
  ( inferRemovedStacks
  ) where

import Stackctl.Prelude

import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Stackctl.AWS.CloudFormation
import Stackctl.AWS.Core as AWS
import Stackctl.AWS.Scope
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import UnliftIO.Directory (doesFileExist)

inferRemovedStacks
  :: ( MonadUnliftIO m
     , MonadAWS m
     , MonadReader env m
     , HasAwsScope env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => m [Stack]
inferRemovedStacks = do
  scope <- view awsScopeL
  paths <- view $ filterOptionL . to filterOptionToPaths
  dir <- view $ directoryOptionL . to unDirectoryOption
  catMaybes <$> traverse (findRemovedStack scope dir) paths

findRemovedStack
  :: (MonadUnliftIO m, MonadAWS m)
  => AwsScope
  -> FilePath
  -- ^ Root directory
  -> FilePath
  -> m (Maybe Stack)
findRemovedStack scope dir path = runMaybeT $ do
  -- The filter is a full path to a specification in the current
  -- account/region...
  stackName <- hoistMaybe $ awsScopeSpecStackName scope path

  -- that no longer exists...
  guard . not =<< doesFileExist (dir </> path)

  -- but the Stack it would point to does
  MaybeT $ awsCloudFormationDescribeStackMaybe stackName
