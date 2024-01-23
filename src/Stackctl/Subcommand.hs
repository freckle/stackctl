module Stackctl.Subcommand
  ( Subcommand (..)
  , subcommand
  , runSubcommand
  , runSubcommand'
  , runAppSubcommand
  ) where

import Stackctl.Prelude

import qualified Env
import Options.Applicative
import Stackctl.AWS (handlingServiceError)
import Stackctl.AutoSSO
import Stackctl.CLI
import Stackctl.ColorOption
import Stackctl.Options
import Stackctl.TelemetryOption
import Stackctl.VerboseOption

data Subcommand options subOptions = Subcommand
  { name :: Text
  , description :: Text
  , parse :: Parser subOptions
  , run :: subOptions -> options -> IO ()
  }

subcommand
  :: Subcommand options subOptions -> Mod CommandFields (options -> IO ())
subcommand Subcommand {..} =
  command (unpack name) (run <$> withInfo description parse)

runSubcommand :: Mod CommandFields (Options -> IO a) -> IO a
runSubcommand =
  runSubcommand' "Work with Stack specifications" envParser optionsParser

-- brittany-disable-next-binding

runSubcommand'
  :: Semigroup options
  => Text
  -> Env.Parser Env.Error options
  -> Parser options
  -> Mod CommandFields (options -> IO a)
  -> IO a
runSubcommand' title parseEnv parseCLI sp = do
  (options, act) <-
    applyEnv
      <$> Env.parse (Env.header $ unpack title) parseEnv
      <*> execParser (withInfo title $ (,) <$> parseCLI <*> subparser sp)

  act options
 where
  applyEnv env = first (env <>)

-- | Use this in the 'run' member of a 'Subcommand' that wants 'AppT'
--
-- @
--   -- ...
--   , parse = parseFooOptions
--   , run = 'runAppSubcommand' runFoo
--   }
--
-- runFoo :: (MonadReader env m, HasAws env) => FooOptions -> m ()
-- runFoo = undefined
-- @
runAppSubcommand
  :: ( HasColorOption options
     , HasVerboseOption options
     , HasAutoSSOOption options
     , HasTelemetryOption options
     )
  => (subOptions -> AppT (App options) IO a)
  -> subOptions
  -> options
  -> IO a
runAppSubcommand f subOptions options =
  runAppT options
    $ handlingServiceError
    $ f subOptions

withInfo :: Text -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ progDesc (unpack d) <> fullDesc
