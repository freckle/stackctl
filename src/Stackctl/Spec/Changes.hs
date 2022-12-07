module Stackctl.Spec.Changes
  ( ChangesOptions(..)
  , runChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude

import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS hiding (action)
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Logger
import Stackctl.ParameterOption
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.StackSpecPath

data ChangesOptions = ChangesOptions
  { scoFormat :: Format
  , scoParameters :: [Parameter]
  , scoOutput :: Maybe FilePath
  }

-- brittany-disable-next-binding

runChangesOptions :: Parser ChangesOptions
runChangesOptions = ChangesOptions
  <$> formatOption
  <*> many parameterOption
  <*> optional (argument str
    (  metavar "PATH"
    <> help "Write changes summary to PATH"
    <> action "file"
    ))

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
  -- Clear file before starting, as we have to use append for each spec
  liftIO $ traverse_ (`T.writeFile` "") scoOutput

  specs <- discoverSpecs

  for_ specs $ \spec -> do
    withThreadContext ["stackName" .= stackSpecStackName spec] $ do
      emChangeSet <- createChangeSet spec scoParameters

      case emChangeSet of
        Left err -> do
          logError $ "Error creating ChangeSet" :# ["error" .= err]
          exitFailure
        Right mChangeSet -> do
          colors <- getColorsStdout
          let
            name = pack $ stackSpecPathFilePath $ stackSpecSpecPath spec
            formatted = formatChangeSet colors name scoFormat mChangeSet

          case scoOutput of
            Nothing -> pushLogger formatted
            Just p -> liftIO $ T.appendFile p $ formatted <> "\n"
