module Stackctl.Spec.List
  ( ListOptions(..)
  , parseListOptions
  , runList
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLoggerLn)
import Options.Applicative
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption(..))
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Spec.Discover
import Stackctl.StackSpec

data ListOptions = ListOptions

-- brittany-disable-next-binding

parseListOptions :: Parser ListOptions
parseListOptions = pure ListOptions

runList
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadResource m
     , MonadLogger m
     , MonadReader env m
     , HasAwsScope env
     , HasAwsEnv env
     , HasLogger env
     , HasConfig env
     , HasDirectoryOption env
     , HasFilterOption env
     )
  => ListOptions
  -> m ()
runList _ = do
  specs <- discoverSpecs
  Colors {..} <- getColorsLogger

  for_ specs $ \spec -> do
    let
      path = stackSpecFilePath spec
      name = stackSpecStackName spec

    exists <- isJust <$> awsCloudFormationDescribeStackMaybe name

    let
      formatted :: Text
      formatted =
        "  "
          <> (if exists then green "✓ " else yellow "✗ ")
          <> cyan (unStackName name)
          <> " => "
          <> magenta (pack path)

    pushLoggerLn formatted
