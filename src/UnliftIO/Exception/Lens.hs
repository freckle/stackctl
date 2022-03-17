-- | A copy of "Control.Exception.Lens" on 'MonadUnliftIO'
--
-- And only the parts we use in this code-base
--
module UnliftIO.Exception.Lens
  ( handling_
  , trying
  ) where

import Prelude

import Control.Lens (Getting, preview)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Data.Monoid (First)
import UnliftIO.Exception (SomeException, catchJust, tryJust)

catching_
  :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m r -> m r
catching_ l a b = catchJust (preview l) a (const b)
{-# INLINE catching_ #-}

handling_
  :: MonadUnliftIO m => Getting (First a) SomeException a -> m r -> m r -> m r
handling_ l = flip (catching_ l)
{-# INLINE handling_ #-}

trying
  :: MonadUnliftIO m
  => Getting (First a) SomeException a
  -> m r
  -> m (Either a r)
trying l = tryJust (preview l)
{-# INLINE trying #-}
