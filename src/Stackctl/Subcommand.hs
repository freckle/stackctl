module Stackctl.Subcommand
  ( Subcommand(..)
  , subcommand
  , runSubcommand
  , runSubcommand'
  ) where

import Stackctl.Prelude

import qualified Env
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
runSubcommand =
  runSubcommand' "Work with Stack specifications" envParser optionsParser

-- brittany-disable-next-binding

runSubcommand'
  :: (Semigroup options, HasVerboseOption options, HasColorOption options)
  => Text
  -> Env.Parser Env.Error options
  -> Parser options
  -> Mod CommandFields (CLI.AppT (CLI.App options) IO ())
  -> IO ()
runSubcommand' title parseEnv parseCLI sp = do
  (options, act) <- applyEnv
    <$> Env.parse (Env.header $ unpack title) parseEnv
    <*> execParser (withInfo title $ (,) <$> parseCLI <*> subparser sp)
  CLI.runAppT options act
  where applyEnv env = first (env <>)

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc
