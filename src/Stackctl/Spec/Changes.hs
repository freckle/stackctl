module Stackctl.Spec.Changes
  ( ChangesOptions(..)
  , runChangesOptions
  , runChanges
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLogStrLn)
import qualified Data.Text.IO as T
import Options.Applicative
import Stackctl.AWS hiding (action)
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.DirectoryOption (HasDirectoryOption)
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.ParameterOption
import Stackctl.Spec.Changes.Format
import Stackctl.Spec.Discover
import Stackctl.StackSpec
import Stackctl.StackSpecPath
import System.Log.FastLogger (toLogStr)

data ChangesOptions = ChangesOptions
  { scoFormat :: Format
  , scoParameters :: [Parameter]
  , scoOutput :: ChangesOutput
  }

-- brittany-disable-next-binding

runChangesOptions :: Parser ChangesOptions
runChangesOptions = ChangesOptions
  <$> formatOption
  <*> many parameterOption
  <*> argument (eitherReader readChangesOutput)
    (  metavar "PATH"
    <> help "Where to write the changes summary"
    <> action "file"
    )

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
  truncateChangesOutput scoOutput

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
          appendChangesOutput scoOutput $ formatted <> "\n"

data ChangesOutput
  = ChangesOutputStdout
  | ChangesOutputLogger
  | ChangesOutputFile FilePath

readChangesOutput :: String -> Either String ChangesOutput
readChangesOutput = \case
  "-" -> Right ChangesOutputStdout
  "@log" -> Right ChangesOutputLogger
  "@logger" -> Right ChangesOutputLogger
  path -> Right $ ChangesOutputFile path

truncateChangesOutput :: MonadIO m => ChangesOutput -> m ()
truncateChangesOutput = \case
  ChangesOutputStdout -> pure ()
  ChangesOutputLogger -> pure ()
  ChangesOutputFile path -> liftIO $ T.writeFile path ""

appendChangesOutput
  :: (MonadIO m, MonadReader env m, HasLogger env)
  => ChangesOutput
  -> Text
  -> m ()
appendChangesOutput output content = case output of
  ChangesOutputStdout -> liftIO $ T.putStr content
  ChangesOutputLogger -> pushLogger content
  ChangesOutputFile path -> liftIO $ T.appendFile path content

-- TODO: this is duplicated in Deploy.hs
pushLogger :: (MonadIO m, MonadReader env m, HasLogger env) => Text -> m ()
pushLogger msg = do
  logger <- view loggerL
  pushLogStrLn logger $ toLogStr msg
