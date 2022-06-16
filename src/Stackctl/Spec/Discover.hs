module Stackctl.Spec.Discover
  ( discoverSpecs
  , buildSpecPath
  ) where

import Stackctl.Prelude

import Data.List.Extra (dropPrefix)
import qualified Data.List.NonEmpty as NE
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.DirectoryOption (HasDirectoryOption(..))
import Stackctl.FilterOption (HasFilterOption(..), filterFilePaths)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.FilePath (isPathSeparator)
import System.FilePath.Glob

discoverSpecs
  :: ( MonadMask m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => m [StackSpec]
discoverSpecs = do
  dir <- view directoryOptionL
  scope@AwsScope {..} <- view awsScopeL
  discovered <- globRelativeTo
    dir
    [ compile
    $ "stacks"
    </> unpack (unAccountId awsAccountId)
    <> ".*"
    </> unpack (fromRegion awsRegion)
    <> "**"
    </> "*"
    <.> "yaml"
    , compile
    $ "stacks"
    </> "*."
    <> unpack (unAccountId awsAccountId)
    </> unpack (fromRegion awsRegion)
    <> "**"
    </> "*"
    <.> "yaml"
    ]

  filterOption <- view filterOptionL

  let
    filtered = filterFilePaths filterOption discovered
    toSpecPath = stackSpecPathFromFilePath scope
    (errs, specPaths) = partitionEithers $ map toSpecPath filtered

    context =
      [ "path" .= dir
      , "filters" .= filterOption
      , "discovered" .= length discovered
      , "filtered" .= length filtered
      , "errors" .= length errs
      ]

  withThreadContext context $ do
    logDebug "Discovered specs"
    when (null filtered) $ logWarn "No specs found"
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
  :: (MonadReader env m, HasAwsScope env)
  => StackName
  -> FilePath
  -> m StackSpecPath
buildSpecPath stackName stackPath = do
  scope <- view awsScopeL
  pure $ stackSpecPath scope stackName stackPath

globRelativeTo :: MonadIO m => FilePath -> [Pattern] -> m [FilePath]
globRelativeTo dir ps = liftIO $ do
  map (dropWhile isPathSeparator . dropPrefix dir) . concat <$> globDir ps dir
