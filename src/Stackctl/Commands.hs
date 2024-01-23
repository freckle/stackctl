module Stackctl.Commands
  ( module Stackctl.Commands
  ) where

import Stackctl.Prelude

import Stackctl.AutoSSO
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.Spec.Capture
import Stackctl.Spec.Cat
import Stackctl.Spec.Changes
import Stackctl.Spec.Deploy
import Stackctl.Spec.List
import Stackctl.Subcommand
import Stackctl.Telemetry.Tags (HasTelemetryTags (..))
import Stackctl.TelemetryOption
import Stackctl.VerboseOption
import Stackctl.Version

cat
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasFilterOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => Subcommand options CatOptions
cat =
  Subcommand
    { name = "cat"
    , description = "Pretty-print specifications"
    , parse = parseCatOptions
    , run = runAppSubcommand runCat
    }

capture
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => Subcommand options CaptureOptions
capture =
  Subcommand
    { name = "capture"
    , description = "Capture deployed Stacks as specifications"
    , parse = parseCaptureOptions
    , run = runAppSubcommand runCapture
    }

changes
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasFilterOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => Subcommand options ChangesOptions
changes =
  Subcommand
    { name = "changes"
    , description = "Review changes between specification and deployed state"
    , parse = parseChangesOptions
    , run = runAppSubcommand runChanges
    }

deploy
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasFilterOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     , HasTelemetryTags options
     )
  => Subcommand options DeployOptions
deploy =
  Subcommand
    { name = "deploy"
    , description = "Deploy specifications"
    , parse = parseDeployOptions
    , run = runAppSubcommand runDeploy
    }

list
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasFilterOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => Subcommand options ListOptions
list =
  Subcommand
    { name = "ls"
    , description = "List specifications"
    , parse = parseListOptions
    , run = runAppSubcommand runList
    }

version :: Subcommand options ()
version =
  Subcommand
    { name = "version"
    , description = "Output the version"
    , parse = pure ()
    , run = \() _ -> logVersion
    }
