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
  { name = "spec:cat"
  , description = "Pretty-print a Stack specification"
  , parse = parseSpecCatOptions
  , run = runSpecCat
  }

capture
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand SpecCaptureOptions env
capture = Subcommand
  { name = "spec:capture"
  , description = "Capture an App's Stack specification"
  , parse = parseSpecCaptureOptions
  , run = runSpecCapture
  }

changes
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env, HasOptions env)
  => Subcommand SpecChangesOptions env
changes = Subcommand
  { name = "spec:changes"
  , description = "Create and review changes from an App's Stack specification"
  , parse = parseSpecChangesOptions
  , run = runSpecChanges
  }

deploy
  :: (HasLogFunc env, HasResourceMap env, HasAwsEnv env)
  => Subcommand SpecDeployOptions env
deploy = Subcommand
  { name = "spec:deploy"
  , description = "Deploy changes from an App's Stack specification"
  , parse = parseSpecDeployOptions
  , run = runSpecDeploy
  }

version :: HasLogFunc env => Subcommand () env
version = Subcommand
  { name = "version"
  , description = "Output the Platform CLI's current version"
  , parse = pure ()
  , run = \() -> logVersion
  }
