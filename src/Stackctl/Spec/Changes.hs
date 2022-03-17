module Stackctl.Spec.Changes
  ( SpecChangesOptions(..)
  , parseSpecChangesOptions
  , runSpecChanges
  ) where

import Stackctl.Prelude

import qualified Data.Text.IO as T
import Stackctl.AWS
import Stackctl.Colors
import Stackctl.FilterOption
import Stackctl.Options
import qualified Stackctl.Paths as Paths
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Options.Applicative

data SpecChangesOptions = SpecChangesOptions
  { scoFilterOption :: Maybe FilterOption
  , scoFormat :: Format
  , scoDirectory :: FilePath
  }

-- brittany-disable-next-binding

parseSpecChangesOptions :: Parser SpecChangesOptions
parseSpecChangesOptions = SpecChangesOptions
  <$> optional (filterOption "discovered specs")
  <*> formatOption
  <*> argument str
    (  metavar "DIRECTORY"
    <> help "Read specifications in DIRECTORY"
    <> value Paths.platformSpecs
    <> showDefault
    )

runSpecChanges
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     , HasOptions env
     )
  => SpecChangesOptions
  -> m ()
runSpecChanges SpecChangesOptions {..} = do
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
