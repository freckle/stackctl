module Stackctl.Spec.Changes
  ( ChangesOptions (..)
  , parseChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLoggerLn)
import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS hiding (action)
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.ParameterOption
import Stackctl.RemovedStack
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import Stackctl.TagOption

data ChangesOptions = ChangesOptions
  { scoFormat :: Format
  , scoOmitFull :: OmitFull
  , scoParameters :: [Parameter]
  , scoTags :: [Tag]
  , scoOutput :: Maybe FilePath
  }

-- brittany-disable-next-binding

parseChangesOptions :: Parser ChangesOptions
parseChangesOptions =
  ChangesOptions
    <$> formatOption
    <*> omitFullOption
    <*> many parameterOption
    <*> many tagOption
    <*> optional
      ( argument
          str
          ( metavar "PATH"
              <> help "Write changes summary to PATH"
              <> action "file"
          )
      )

runChanges
  :: ( MonadMask m
     , MonadUnliftIO m
     , MonadAWS m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasAwsScope env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => ChangesOptions
  -> m ()
runChanges ChangesOptions {..} = do
  -- Clear file before starting, as we have to use append for each spec
  liftIO $ traverse_ (`T.writeFile` "") scoOutput

  colors <- case scoOutput of
    Nothing -> getColorsLogger
    Just {} -> pure noColors

  let write formatted = case scoOutput of
        Nothing -> pushLoggerLn formatted
        Just p -> liftIO $ T.appendFile p $ formatted <> "\n"

  forEachSpec_ $ \spec -> do
    withThreadContext ["stackName" .= stackSpecStackName spec] $ do
      emChangeSet <- createChangeSet spec scoParameters scoTags

      case emChangeSet of
        Left err -> do
          logError $ "Error creating ChangeSet" :# ["error" .= err]
          exitFailure
        Right mChangeSet -> do
          let name = pack $ stackSpecPathFilePath $ stackSpecSpecPath spec
          write $ formatChangeSet colors scoOmitFull name scoFormat mChangeSet

  removed <- inferRemovedStacks
  traverse_ (write . formatRemovedStack colors scoFormat) removed
