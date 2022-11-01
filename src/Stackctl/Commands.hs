module Stackctl.Commands
  ( cat
  , capture
  , changes
  , deploy
  , version
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.AWS.Scope
import Stackctl.ColorOption
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Stackctl.Spec.Capture
import Stackctl.Spec.Cat
import Stackctl.Spec.Changes
import Stackctl.Spec.Deploy
import Stackctl.Subcommand
import Stackctl.Version

cat
  :: ( HasLogger env
     , HasAwsScope env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => Subcommand CatOptions env
cat = Subcommand
  { name = "cat"
  , description = "Pretty-print specifications"
  , parse = runCatOptions
  , run = runCat
  }

capture
  :: (HasAwsScope env, HasAwsEnv env, HasDirectoryOption env)
  => Subcommand CaptureOptions env
capture = Subcommand
  { name = "capture"
  , description = "Capture deployed Stacks as specifications"
  , parse = runCaptureOptions
  , run = runCapture
  }

changes
  :: ( HasAwsScope env
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => Subcommand ChangesOptions env
changes = Subcommand
  { name = "changes"
  , description = "Review changes between specification and deployed state"
  , parse = runChangesOptions
  , run = runChanges
  }

deploy
  :: ( HasLogger env
     , HasAwsScope env
     , HasAwsEnv env
     , HasDirectoryOption env
     , HasFilterOption env
     , HasColorOption env
     )
  => Subcommand DeployOptions env
deploy = Subcommand
  { name = "deploy"
  , description = "Deploy specifications"
  , parse = runDeployOptions
  , run = runDeploy
  }

version :: Subcommand () env
version = Subcommand
  { name = "version"
  , description = "Output the version"
  , parse = pure ()
  , run = \() -> logVersion
  }
