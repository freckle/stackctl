module Stackctl.Commands
  ( cat
  , capture
  , changes
  , deploy
  , version
  ) where

import Stackctl.Prelude

import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.Spec.Capture
import Stackctl.Spec.Cat
import Stackctl.Spec.Changes
import Stackctl.Spec.Deploy
import Stackctl.Subcommand
import Stackctl.VerboseOption
import Stackctl.Version

cat
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     , HasFilterOption options
     )
  => Subcommand options CatOptions
cat = Subcommand
  { name = "cat"
  , description = "Pretty-print specifications"
  , parse = parseCatOptions
  , run = runAppSubcommand runCat
  }

capture
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasDirectoryOption options
     )
  => Subcommand options CaptureOptions
capture = Subcommand
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
     )
  => Subcommand options ChangesOptions
changes = Subcommand
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
     )
  => Subcommand options DeployOptions
deploy = Subcommand
  { name = "deploy"
  , description = "Deploy specifications"
  , parse = parseDeployOptions
  , run = runAppSubcommand runDeploy
  }

version :: Subcommand options ()
version = Subcommand
  { name = "version"
  , description = "Output the version"
  , parse = pure ()
  , run = \() _ -> logVersion
  }
