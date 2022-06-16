module Stackctl.Spec.Discover
  ( discoverSpecs
  , buildSpecPath
  ) where

import Stackctl.Prelude2

import RIO.FilePath (isPathSeparator)
import RIO.List (dropPrefix)
import qualified RIO.NonEmpty as NE
import Stackctl.AWS
import Stackctl.DirectoryOption (HasDirectoryOption(..))
import Stackctl.FilterOption (HasFilterOption(..), filterFilePaths)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.FilePath.Glob

discoverSpecs
  :: ( MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => m [StackSpec]
discoverSpecs = do
  dir <- view directoryOptionL
  accountId <- fetchCurrentAccountId
  region <- fetchCurrentRegion
  discovered <- globRelativeTo
    dir
    [ compile
    $ "stacks"
    </> unpack (unAccountId accountId)
    <> ".*"
    </> unpack (fromRegion region)
    <> "**"
    </> "*"
    <.> "yaml"
    , compile
    $ "stacks"
    </> "*."
    <> unpack (unAccountId accountId)
    </> unpack (fromRegion region)
    <> "**"
    </> "*"
    <.> "yaml"
    ]

  filterOption <- view filterOptionL

  let
    filtered = filterFilePaths filterOption discovered
    toSpecPath = stackSpecPathFromFilePath accountId region
    (errs, specPaths) = partitionEithers $ map toSpecPath filtered

  logDebug
    $ "Discovered specs"
    :# ["discovered" .= discovered, "filtered" .= filtered, "ignored" .= errs]

  when (null filtered)
    $ logWarn
    $ "No specs found"
    :# [ "aws"
         .= object ["account" .= object ["id" .= accountId], "region" .= region]
       , "filters" .= filterOption
       , "discovered" .= length discovered
       , "errors" .= length errs
       ]

  checkForDuplicateStackNames specPaths
  sortStackSpecs <$> traverse (readStackSpec dir) specPaths

checkForDuplicateStackNames
  :: (MonadIO m, MonadLogger m) => [StackSpecPath] -> m ()
checkForDuplicateStackNames =
  traverse_ reportCollisions
    . NE.nonEmpty
    . filter ((> 1) . length)
    . NE.groupAllWith stackSpecPathStackName
 where
  reportCollisions
    :: (MonadIO m, MonadLogger m) => NonEmpty (NonEmpty StackSpecPath) -> m ()
  reportCollisions errs = do
    for_ errs $ \specPaths -> do
      let collidingPaths = stackSpecPathFilePath <$> specPaths

      logError
        $ "Multiple specifications produced the same Stack name"
        :# [ "name" .= stackSpecPathStackName (NE.head specPaths)
           , "paths" .= collidingPaths
           ]

    exitFailure

buildSpecPath
  :: (MonadResource m, MonadReader env m, HasAwsEnv env)
  => Text -- ^ @.{account-name}@ to use
  -> StackName
  -> FilePath
  -> m StackSpecPath
buildSpecPath accountName stackName stackPath =
  stackSpecPath
    <$> fetchCurrentAccountId
    <*> pure accountName
    <*> fetchCurrentRegion
    <*> pure stackName
    <*> pure stackPath

fetchCurrentAccountId
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => m AccountId
fetchCurrentAccountId = awsGetCallerIdentityAccount

-- | Fetch the discovered region for @aws@ calls made
--
-- You can't just ask for this. The only way to find it is to make an actual
-- API call that conveys the region somewhere via its response.
--
-- See <https://stackoverflow.com/a/63496689>.
--
fetchCurrentRegion
  :: (MonadResource m, MonadReader env m, HasAwsEnv env) => m Region
fetchCurrentRegion = awsEc2DescribeFirstAvailabilityZoneRegionName

globRelativeTo :: MonadIO m => FilePath -> [Pattern] -> m [FilePath]
globRelativeTo dir ps = liftIO $ do
  map (dropWhile isPathSeparator . dropPrefix dir) . concat <$> globDir ps dir
