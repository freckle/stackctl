module Stackctl.Prompt
  ( prompt
  , promptContinue
  ) where

import Stackctl.Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Stackctl.Logger

prompt
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => Text
  -- ^ Message to present
  -> (Text -> Either Text a)
  -- ^ Parse user input (stripped)
  -> (a -> m r)
  -- ^ Action to take on result
  -> m r
prompt message parse dispatch = do
  flushLogger

  x <- liftIO $ do
    T.putStr $ message <> "? "
    hFlush stdout
    T.strip <$> T.getLine

  case parse x of
    Left err -> do
      logWarn $ "Invalid input" :# ["error" .= err]
      prompt message parse dispatch
    Right a -> dispatch a

promptContinue
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env) => m ()
promptContinue = prompt "Continue (y/n)" parse dispatch
 where
  parse x
    | x `elem` ["y", "Y"] = Right True
    | x `elem` ["n", "N"] = Right False
    | otherwise = Left $ "Must be y, Y, n, or N (saw " <> x <> ")"

  dispatch b = if b then pure () else exitSuccess
