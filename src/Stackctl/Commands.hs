module Stackctl.Commands
  ( cat
  , capture
  , generate
  , changes
  , deploy
  , version
  ) where

import Stackctl.Prelude

import Stackctl.AWS
import Stackctl.Options
import Stackctl.Spec
import Stackctl.Subcommand
import Stackctl.Version

cat
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env, HasOptions env)
  => Subcommand CatOptions env
cat = Subcommand
  { name = "cat"
  , description = "Pretty-print specifications"
  , parse = runCatOptions
  , run = runCat
  }

capture
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand CaptureOptions env
capture = Subcommand
  { name = "capture"
  , description = "Capture deployed Stacks as specifications"
  , parse = runCaptureOptions
  , run = runCapture
  }

changes
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env, HasOptions env)
  => Subcommand ChangesOptions env
changes = Subcommand
  { name = "changes"
  , description = "Review changes between specification and deployed state"
  , parse = runChangesOptions
  , run = runChanges
  }

deploy
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand DeployOptions env
deploy = Subcommand
  { name = "deploy"
  , description = "Deploy specifications"
  , parse = runDeployOptions
  , run = runDeploy
  }

version :: HasLogFunc env => Subcommand () env
version = Subcommand
  { name = "version"
  , description = "Output the version"
  , parse = pure ()
  , run = \() -> logVersion
  }
