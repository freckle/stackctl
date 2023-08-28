module Stackctl.AutoSSO
  ( AutoSSOOption
  , defaultAutoSSOOption
  , HasAutoSSOOption (..)
  , autoSSOOption
  , envAutoSSOOption
  , handleAutoSSO
  ) where

import Stackctl.Prelude

import Amazonka.SSO (_UnauthorizedException)
import Data.Semigroup (Last (..))
import qualified Env
import Options.Applicative
import Stackctl.AWS.Core (formatServiceError)
import Stackctl.Prompt
import System.Process.Typed
import UnliftIO.Exception.Lens (catching)

data AutoSSOOption
  = AutoSSOAlways
  | AutoSSOAsk
  | AutoSSONever
  deriving (Semigroup) via Last AutoSSOOption

defaultAutoSSOOption :: AutoSSOOption
defaultAutoSSOOption = AutoSSOAsk

readAutoSSO :: String -> Either String AutoSSOOption
readAutoSSO = \case
  "always" -> Right AutoSSOAlways
  "ask" -> Right AutoSSOAsk
  "never" -> Right AutoSSONever
  x ->
    Left $ "Invalid choice for auto-sso: " <> x <> ", must be always|ask|never"

class HasAutoSSOOption env where
  autoSSOOptionL :: Lens' env AutoSSOOption

autoSSOOption :: Parser AutoSSOOption
autoSSOOption =
  option (eitherReader readAutoSSO)
    $ mconcat [long "auto-sso", help autoSSOHelp, metavar "WHEN"]

envAutoSSOOption :: Env.Parser Env.Error AutoSSOOption
envAutoSSOOption =
  Env.var (first Env.UnreadError . readAutoSSO) "AUTO_SSO"
    $ Env.help autoSSOHelp

autoSSOHelp :: IsString a => a
autoSSOHelp = "Automatically run aws-sso-login if necessary?"

handleAutoSSO
  :: ( MonadUnliftIO m
     , MonadReader env m
     , MonadLogger m
     , HasLogger env
     , HasAutoSSOOption options
     )
  => options
  -> m a
  -> m a
handleAutoSSO options f = do
  catching _UnauthorizedException f $ \ex -> do
    case options ^. autoSSOOptionL of
      AutoSSOAlways -> do
        logWarn $ ssoErrorMessage ex
        logInfo "Running `aws sso login' automatically"
      AutoSSOAsk -> do
        logWarn $ ssoErrorMessage ex
        promptOrExit "Run `aws sso login'"
      AutoSSONever -> do
        logError $ ssoErrorMessage ex
        exitFailure

    runProcess_ $ proc "aws" ["sso", "login"]
    f
 where
  ssoErrorMessage ex =
    "AWS SSO authorization error"
      :# [ "message" .= formatServiceError ex
         , "hint" .= ("Run `aws sso login' and try again" :: Text)
         ]
