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
  => Subcommand SpecCatOptions env
cat = Subcommand
  { name = "cat"
  , description = "Pretty-print specifications"
  , parse = parseSpecCatOptions
  , run = runSpecCat
  }

capture
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand SpecCaptureOptions env
capture = Subcommand
  { name = "capture"
  , description = "Capture deployed Stacks as specifications"
  , parse = parseSpecCaptureOptions
  , run = runSpecCapture
  }

changes
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env, HasOptions env)
  => Subcommand SpecChangesOptions env
changes = Subcommand
  { name = "changes"
  , description = "Review changes between specification and deployed state"
  , parse = parseSpecChangesOptions
  , run = runSpecChanges
  }

deploy
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand SpecDeployOptions env
deploy = Subcommand
  { name = "deploy"
  , description = "Deploy specifications"
  , parse = parseSpecDeployOptions
  , run = runSpecDeploy
  }

version :: HasLogFunc env => Subcommand () env
version = Subcommand
  { name = "version"
  , description = "Output the version"
  , parse = pure ()
  , run = \() -> logVersion
  }
