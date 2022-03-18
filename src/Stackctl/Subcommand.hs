module Stackctl.Subcommand
  ( Subcommand(..)
  , subcommand
  , runSubcommand
  ) where

import Stackctl.Prelude

import Options.Applicative
import qualified Stackctl.CLI as CLI
import Stackctl.Options

data Subcommand options env = Subcommand
  { name :: Text
  , description :: Text
  , parse :: Parser options
  , run :: options -> RIO env ()
  }

subcommand :: Subcommand options env -> Mod CommandFields (RIO env ())
subcommand Subcommand {..} =
  command (unpack name) (run <$> withInfo description parse)

runSubcommand :: Mod CommandFields (RIO (CLI.App Options) ()) -> IO ()
runSubcommand x = do
  (options, act) <-
    execParser
    $ withInfo "Work with Stack specifications"
    $ (,)
    <$> optionsParser
    <*> subparser x
  CLI.runApp options act

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc
