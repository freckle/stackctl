module Main
  ( main
  ) where

import Stackctl.Prelude

import qualified Stackctl.Commands as Commands
import Stackctl.Subcommand

main :: IO ()
main =
  runSubcommand
    $ subcommand Commands.cat
    <> subcommand Commands.capture
    <> subcommand Commands.changes
    <> subcommand Commands.deploy
    <> subcommand Commands.version
