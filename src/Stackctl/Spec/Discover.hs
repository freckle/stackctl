module Stackctl.Spec.Discover
  ( discoverSpecs
  , buildSpecPath
  ) where

import Stackctl.Prelude

import Data.Semigroup (sconcat)
import RIO.FilePath (isPathSeparator)
import RIO.List (dropPrefix)
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.FilterOption
import Stackctl.Options
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.FilePath.Glob

discoverSpecs
  :: ( MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     , HasOptions env
     )
  => m [StackSpec]
discoverSpecs = do
  dir <- oDirectory <$> view optionsL
  accountId <- fetchCurrentAccountId
  region <- fetchCurrentRegion
  discovered <-
    globRelativeTo dir
    $ compile
    $ "stacks"
    </> unpack (unAccountId accountId)
    <> ".*"
    </> unpack (fromRegion region)
    <> "**"
    </> "*"
    <.> "yaml"

  mFilterOption <- oFilterOption <$> view optionsL

  let
    filtered = maybe id filterFilePaths mFilterOption discovered
    toSpecPath = stackSpecPathFromFilePath accountId region
    (errs, specPaths) = partitionEithers $ map toSpecPath filtered

  logDebug $ "Discovered: " <> displayShow discovered
  logDebug $ "Filtered: " <> displayShow filtered
  logDebug $ "Ignored: " <> display (T.intercalate ", " $ map pack errs)

  colors@Colors {..} <- getColorsLogFunc

  when (null filtered)
    $ logWarn
    $ "No specs found in "
    <> magenta (fromString dir)
    <> " for "
    <> cyan (display accountId)
    <> "/"
    <> cyan (display region)
    <> maybe "" ((" matching " <>) . green . display) mFilterOption
    <> " ("
    <> display (length discovered)
    <> " discovered but not matched, "
    <> red (display $ length errs)
    <> " matched but ignored due to errors)"

  checkForDuplicateStackNames colors specPaths
  sortStackSpecs <$> traverse (readStackSpec dir) specPaths

checkForDuplicateStackNames
  :: (MonadIO m, MonadReader env m, HasLogFunc env)
  => Colors
  -> [StackSpecPath]
  -> m ()
checkForDuplicateStackNames Colors {..} =
  traverse_ reportCollisions
    . NE.nonEmpty
    . filter ((> 1) . length)
    . NE.groupAllWith stackSpecPathStackName
 where
  reportCollisions
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => NonEmpty (NonEmpty StackSpecPath)
    -> m ()
  reportCollisions errs = do
    for_ errs $ \specPaths -> do
      let collidingPaths = stackSpecPathFilePath <$> specPaths

      logError $ mconcat
        [ "Multiple specifications produced Stack name "
        <> cyan (display $ stackSpecPathStackName $ NE.head specPaths)
        <> ":"
        , sconcat $ ("\n  - " <>) . magenta . fromString <$> collidingPaths
        ]

    exitFailure

buildSpecPath
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
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
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
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
  :: (MonadResource m, MonadReader env m, HasLogFunc env, HasAwsEnv env)
  => m Region
fetchCurrentRegion = awsEc2DescribeFirstAvailabilityZoneRegionName

globRelativeTo :: MonadIO m => FilePath -> Pattern -> m [FilePath]
globRelativeTo dir p = liftIO $ do
  map (dropWhile isPathSeparator . dropPrefix dir) <$> globDir1 p dir
