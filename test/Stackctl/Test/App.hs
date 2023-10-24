module Stackctl.Test.App
  ( TestAppT
  , runTestAppT

    -- * Re-exports
  , module Stackctl.Prelude
  , module Control.Lens
  , module Control.Monad.AWS.ViaMock
  , module Test.Hspec
  , module Test.Hspec.Expectations.Lifted
  ) where

import Stackctl.Prelude

import Blammo.Logging.Logger (newTestLogger)
import Control.Lens ((?~))
import Control.Monad.AWS
import Control.Monad.AWS.ViaMock
import Test.Hspec (Spec, describe, example, it)
import Test.Hspec.Expectations.Lifted

data TestApp = TestApp
  { taLogger :: Logger
  , taMatchers :: Matchers
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

instance HasMatchers TestApp where
  matchersL = lens taMatchers $ \x y -> x {taMatchers = y}

newtype TestAppT m a = TestAppT
  { unTestAppT :: ReaderT TestApp (LoggingT m) a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadLogger
    , MonadReader TestApp
    )
  deriving (MonadAWS) via (MockAWS (TestAppT m))

runTestAppT :: MonadUnliftIO m => TestAppT m a -> m a
runTestAppT f = do
  app <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure mempty

  runLoggerLoggingT app $ runReaderT (unTestAppT f) app
