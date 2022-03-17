module Stackctl.Subcommand
  ( Subcommand(..)
  , subcommand
  , runSubcommand
  ) where

import Stackctl.Prelude

import qualified Stackctl.CLI as CLI
import Stackctl.Options
import Options.Applicative

data Subcommand options env = Subcommand
  { name :: Text
  , description :: Text
  , parse :: Parser options
  , run :: options -> RIO env ()
  }

subcommand :: Subcommand options env -> Mod CommandFields (RIO env ())
subcommand Subcommand {..} =
  command (unpack name) (run <$> withInfo description parse)

runSubcommand :: Mod CommandFields (RIO CLI.App ()) -> IO ()
runSubcommand x = do
  (options, act) <-
    execParser
    $ withInfo "Deploy and manage Apps on the Freckle Platform"
    $ (,)
    <$> optionsParser
    <*> subparser x
  CLI.runApp options act

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc
