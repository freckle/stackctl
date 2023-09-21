module Stackctl.Spec.Discover
  ( forEachSpec_
  , discoverSpecs
  , buildSpecPath
  ) where

import Stackctl.Prelude

import Data.List.Extra (dropPrefix, minimumBy)
import qualified Data.List.NonEmpty as NE
import Data.Text.Metrics (levenshtein)
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption (..), unDirectoryOption)
import Stackctl.FilterOption (HasFilterOption (..), filterStackSpecs)
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.FilePath (isPathSeparator)
import System.FilePath.Glob

forEachSpec_
  :: ( MonadMask m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => (StackSpec -> m ())
  -> m ()
forEachSpec_ f = traverse_ f =<< discoverSpecs

discoverSpecs
  :: ( MonadMask m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => m [StackSpec]
discoverSpecs = do
  dir <- unDirectoryOption <$> view directoryOptionL
  scope <- view awsScopeL
  paths <- globRelativeTo dir $ awsScopeSpecPatterns scope
  filterOption <- view filterOptionL

  let
    toSpecPath = stackSpecPathFromFilePath scope
    (errs, specPaths) = partitionEithers $ map toSpecPath paths

    context =
      [ "path" .= dir
      , "filters" .= filterOption
      , "paths" .= length paths
      , "errors" .= length errs
      , "specs" .= length specPaths
      ]

  withThreadContext context $ do
    checkForDuplicateStackNames specPaths

    allSpecs <- traverse (readStackSpec dir) specPaths

    let
      known = map stackSpecStackName allSpecs
      specs = sortStackSpecs $ filterStackSpecs filterOption allSpecs

    traverse_ (checkForUnknownDepends known) specs

    when (null specs) $ logWarn "No specs found"
    specs <$ logDebug ("Discovered specs" :# ["matched" .= length specs])

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
      let collidingPaths = stackSpecPathBasePath <$> specPaths

      logError
        $ "Multiple specifications produced the same Stack name"
        :# [ "name" .= stackSpecPathStackName (NE.head specPaths)
           , "paths" .= collidingPaths
           ]

    exitFailure

-- | Warn if a 'StackSpec' depends on a name not in the given 'StackName's
--
-- The 'StackName's are built from all specs, but we only run this with specs
-- that are filtered in.
--
-- NB. This function is written so it can easily be made into a fatal error
-- (like 'checkForDuplicateStackNames'), but we only warn for now.
checkForUnknownDepends :: MonadLogger m => [StackName] -> StackSpec -> m ()
checkForUnknownDepends known spec =
  traverse_ reportUnknownDepends
    $ NE.nonEmpty
    $ filter (`notElem` known)
    $ stackSpecDepends spec
 where
  reportUnknownDepends depends = do
    for_ depends $ \depend -> do
      let (nearest, _distance) =
            minimumBy (comparing snd)
              . map (id &&& getDistance depend)
              $ known

      logWarn
        $ "Stack lists dependency that does not exist"
        :# [ "dependency"
              .= ( unStackName (stackSpecStackName spec)
                    <> " -> "
                    <> unStackName depend
                 )
           , "hint" .= ("Did you mean " <> unStackName nearest <> "?")
           ]

  getDistance = levenshtein `on` unStackName

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
