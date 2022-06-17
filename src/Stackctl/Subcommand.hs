module Stackctl.Subcommand
  ( Subcommand(..)
  , subcommand
  , runSubcommand
  , runSubcommand'
  ) where

import Stackctl.Prelude

import Options.Applicative
import qualified Stackctl.CLI as CLI
import Stackctl.Colors
import Stackctl.Options
import Stackctl.VerboseOption

data Subcommand options env = Subcommand
  { name :: Text
  , description :: Text
  , parse :: Parser options
  , run :: options -> CLI.AppT env IO ()
  }

subcommand :: Subcommand options env -> Mod CommandFields (CLI.AppT env IO ())
subcommand Subcommand {..} =
  command (unpack name) (run <$> withInfo description parse)

runSubcommand :: Mod CommandFields (CLI.AppT (CLI.App Options) IO ()) -> IO ()
runSubcommand = runSubcommand' "Work with Stack specifications" optionsParser

runSubcommand'
  :: (HasVerboseOption options, HasColorOption options)
  => Text
  -> Parser options
  -> Mod CommandFields (CLI.AppT (CLI.App options) IO ())
  -> IO ()
runSubcommand' x op sp = do
  (options, act) <- execParser $ withInfo x $ (,) <$> op <*> subparser sp
  CLI.runAppT options act

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc
