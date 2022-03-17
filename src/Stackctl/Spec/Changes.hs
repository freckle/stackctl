module Stackctl.Spec.Changes
  ( ChangesOptions(..)
  , runChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude

import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.FilterOption
import Stackctl.Options
import qualified Stackctl.Paths as Paths
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec

data ChangesOptions = ChangesOptions
  { scoFilterOption :: Maybe FilterOption
  , scoFormat :: Format
  , scoDirectory :: FilePath
  }

-- brittany-disable-next-binding

runChangesOptions :: Parser ChangesOptions
runChangesOptions = ChangesOptions
  <$> optional (filterOption "discovered specs")
  <*> formatOption
  <*> argument str
    (  metavar "DIRECTORY"
    <> help "Read specifications in DIRECTORY"
    <> value Paths.defaultSpecs
    <> showDefault
    )

runChanges
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     , HasOptions env
     )
  => ChangesOptions
  -> m ()
runChanges ChangesOptions {..} = do
  colors <- getColorsStdout
  specs <- discoverSpecs scoDirectory scoFilterOption

  for_ specs $ \spec -> do
    logStackSpec spec

    emChangeSet <- createChangeSet spec

    case emChangeSet of
      Left err -> do
        logError $ display err
        exitFailure
      Right mChangeSet ->
        liftIO $ T.putStrLn $ utf8BuilderToText $ formatChangeSet
          colors
          spec
          scoFormat
          mChangeSet
