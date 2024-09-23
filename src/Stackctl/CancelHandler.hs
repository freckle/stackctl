module Stackctl.CancelHandler
  ( with
  , install
  , remove
  , trigger
  ) where

import Stackctl.Prelude

import System.Posix.Signals

-- | Install a 'keyboardSignal' handler, run an action, then remove it
with :: MonadUnliftIO m => m a -> m b -> m b
with f = bracket_ (install f) remove

-- | Install a 'keyboardSignal' handler that runs the given action once
install :: MonadUnliftIO m => m a -> m ()
install f = do
  withRunInIO $ \runInIO -> do
    let handler = Catch $ void $ do
          remove -- so next Ctl-C will truly cancel
          runInIO f
    void $ installHandler keyboardSignal handler Nothing

-- | Remove the current handler for 'keyboardSignal' (i.e. install 'Default')
remove :: MonadIO m => m ()
remove = liftIO $ void $ installHandler keyboardSignal Default Nothing

-- | Trigger the installed 'keyboardSignal' handler
--
-- This is used by our test suite.
trigger :: MonadIO m => m ()
trigger = liftIO $ raiseSignal keyboardSignal
