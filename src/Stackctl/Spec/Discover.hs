module Stackctl.Spec.Discover
  ( discoverSpecs
  , buildSpecPath
  ) where

import Stackctl.Prelude2

import Data.Semigroup (sconcat)
import RIO.FilePath (isPathSeparator)
import RIO.List (dropPrefix)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption(..))
import Stackctl.FilterOption (HasFilterOption(..), filterFilePaths)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.FilePath.Glob

discoverSpecs
  :: ( MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogFunc env
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

  logDebug $ t $ "Discovered: " <> displayShow discovered
  logDebug $ t $ "Filtered: " <> displayShow filtered
  logDebug $ t $ "Ignored: " <> display (T.intercalate ", " $ map pack errs)

  colors@Colors {..} <- getColorsLogFunc

  when (null filtered)
    $ logWarn
    $ t
    $ "No specs found in "
    <> magenta (fromString dir)
    <> " for "
    <> cyan (display accountId)
    <> "/"
    <> cyan (display region)
    <> " matching "
    <> green (display filterOption)
    <> " ("
    <> display (length discovered)
    <> " discovered but not matched, "
    <> red (display $ length errs)
    <> " matched but ignored due to errors)"

  checkForDuplicateStackNames colors specPaths
  sortStackSpecs <$> traverse (readStackSpec dir) specPaths

checkForDuplicateStackNames
  :: (MonadIO m, MonadLogger m) => Colors -> [StackSpecPath] -> m ()
checkForDuplicateStackNames Colors {..} =
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

      logError $ t $ mconcat
        [ "Multiple specifications produced Stack name "
        <> cyan (display $ stackSpecPathStackName $ NE.head specPaths)
        <> ":"
        , sconcat $ ("\n  - " <>) . magenta . fromString <$> collidingPaths
        ]

    exitFailure

buildSpecPath
  :: (MonadResource m, MonadLogger m, MonadReader env m, HasAwsEnv env)
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
  :: (MonadResource m, MonadLogger m, MonadReader env m, HasAwsEnv env)
  => m AccountId
fetchCurrentAccountId = awsGetCallerIdentityAccount

-- | Fetch the discovered region for @aws@ calls made
--
-- You can't just ask for this. The only way to find it is to make an actual
-- API call that conveys the region somewhere via its response.
--
-- See <https://stackoverflow.com/a/63496689>.
--
fetchCurrentRegion
  :: (MonadResource m, MonadLogger m, MonadReader env m, HasAwsEnv env)
  => m Region
fetchCurrentRegion = awsEc2DescribeFirstAvailabilityZoneRegionName

globRelativeTo :: MonadIO m => FilePath -> [Pattern] -> m [FilePath]
globRelativeTo dir ps = liftIO $ do
  map (dropWhile isPathSeparator . dropPrefix dir) . concat <$> globDir ps dir
