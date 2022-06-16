module Stackctl.Spec.Changes
  ( ChangesOptions(..)
  , runChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude2

import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS
import Stackctl.ColorOption (HasColorOption)
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec

newtype ChangesOptions = ChangesOptions
  { scoFormat :: Format
  }

runChangesOptions :: Parser ChangesOptions
runChangesOptions = ChangesOptions <$> formatOption

runChanges
  :: ( MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogFunc env
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => ChangesOptions
  -> m ()
runChanges ChangesOptions {..} = do
  colors <- getColorsStdout
  specs <- discoverSpecs

  for_ specs $ \spec -> do
    logStackSpec spec

    emChangeSet <- createChangeSet spec

    case emChangeSet of
      Left err -> do
        logError $ t $ display err
        exitFailure
      Right mChangeSet ->
        liftIO $ T.putStrLn $ utf8BuilderToText $ formatChangeSet
          colors
          spec
          scoFormat
          mChangeSet
