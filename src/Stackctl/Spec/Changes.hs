module Stackctl.Spec.Changes
  ( ChangesOptions(..)
  , runChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (flushLogger)
import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.StackSpecPath

newtype ChangesOptions = ChangesOptions
  { scoFormat :: Format
  }

runChangesOptions :: Parser ChangesOptions
runChangesOptions = ChangesOptions <$> formatOption

runChanges
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsScope env
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => ChangesOptions
  -> m ()
runChanges ChangesOptions {..} = do
  specs <- discoverSpecs

  for_ specs $ \spec -> do
    withThreadContext ["stackName" .= stackSpecStackName spec] $ do
      emChangeSet <- createChangeSet spec

      case emChangeSet of
        Left err -> do
          logError $ "Error creating ChangeSet" :# ["error" .= err]
          exitFailure
        Right mChangeSet -> do
          colors <- getColorsStdout
          let name = pack $ stackSpecPathFilePath $ stackSpecSpecPath spec

          flushLogger
          liftIO $ T.putStrLn $ formatChangeSet colors name scoFormat mChangeSet
