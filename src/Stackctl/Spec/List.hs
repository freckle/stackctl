module Stackctl.Spec.List
  ( ListOptions (..)
  , parseListOptions
  , runList
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (pushLoggerLn)
import qualified Data.Text as T
import Options.Applicative
import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.Colors
import Stackctl.Config (HasConfig)
import Stackctl.DirectoryOption (HasDirectoryOption (..))
import Stackctl.FilterOption (HasFilterOption)
import Stackctl.Spec.Discover
import Stackctl.StackSpec

newtype ListOptions = ListOptions
  { loLegend :: Bool
  }

parseListOptions :: Parser ListOptions
parseListOptions =
  ListOptions
    <$> ( not
            <$> switch
              ( mconcat
                  [ long "no-legend"
                  , help "Don't print indicators legend at the end"
                  ]
              )
        )

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
runList ListOptions {..} = do
  colors@Colors {..} <- getColorsLogger

  forEachSpec_ $ \spec -> do
    let
      path = stackSpecFilePath spec
      name = stackSpecStackName spec

    mStackStatus <-
      fmap (^. stack_stackStatus)
        <$> awsCloudFormationDescribeStackMaybe name

    let
      indicator = maybe NotDeployed statusIndicator mStackStatus

      formatted :: Text
      formatted =
        "  "
          <> indicatorIcon colors indicator
          <> " "
          <> cyan (unStackName name)
          <> " => "
          <> magenta (pack path)

    pushLoggerLn formatted

  let legendItem i = indicatorIcon colors i <> " " <> indicatorDescription i

  when loLegend
    $ pushLoggerLn
    $ "\nLegend:\n  "
    <> T.intercalate ", " (map legendItem [minBound .. maxBound])

data Indicator
  = Deployed
  | DeployFailed
  | NotDeployed
  | Reviewing
  | Deploying
  | Unknown
  deriving stock (Bounded, Enum)

indicatorIcon :: Colors -> Indicator -> Text
indicatorIcon Colors {..} = \case
  Deployed -> green "✓"
  DeployFailed -> red "✗"
  NotDeployed -> yellow "_"
  Reviewing -> yellow "∇"
  Deploying -> cyan "⋅"
  Unknown -> magenta "?"

indicatorDescription :: Indicator -> Text
indicatorDescription = \case
  Deployed -> "deployed"
  DeployFailed -> "failed or rolled back"
  NotDeployed -> "doesn't exist"
  Reviewing -> "reviewing"
  Deploying -> "deploying"
  Unknown -> "unknown"

statusIndicator :: StackStatus -> Indicator
statusIndicator = \case
  StackStatus_REVIEW_IN_PROGRESS -> Reviewing
  StackStatus_ROLLBACK_COMPLETE -> DeployFailed
  x | statusSuffixed "_IN_PROGRESS" x -> Deploying
  x | statusSuffixed "_FAILED" x -> DeployFailed
  x | statusSuffixed "_ROLLBACK_COMPLETE" x -> DeployFailed
  x | statusSuffixed "_COMPLETE" x -> Deployed
  _ -> Unknown
 where
  statusSuffixed x = (x `T.isSuffixOf`) . fromStackStatus
