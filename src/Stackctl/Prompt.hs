module Stackctl.Prompt
  ( prompt
  , promptContinue
  ) where

import Stackctl.Prelude2

import qualified Data.Text.IO as T
import qualified RIO.Text as T

prompt
  :: (MonadIO m, MonadLogger m, MonadReader env m)
  => Text
  -- ^ Message to present
  -> (Text -> Either Text a)
  -- ^ Parse user input (stripped)
  -> (a -> m r)
  -- ^ Action to take on result
  -> m r
prompt message parse dispatch = do
  x <- liftIO $ do
    T.putStr $ message <> "? "
    hFlush stdout
    T.strip <$> T.getLine

  case parse x of
    Left err -> do
      logWarn $ "Invalid input" :# ["error" .= err]
      prompt message parse dispatch
    Right a -> dispatch a

promptContinue :: (MonadIO m, MonadLogger m, MonadReader env m) => m ()
promptContinue = prompt "Continue (y/n)" parse dispatch
 where
  parse x
    | x `elem` ["y", "Y"] = Right True
    | x `elem` ["n", "N"] = Right False
    | otherwise = Left $ "Must be y, Y, n, or N (saw " <> x <> ")"

  dispatch b = if b then pure () else exitSuccess
