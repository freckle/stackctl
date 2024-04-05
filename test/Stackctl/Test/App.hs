module Stackctl.Test.App
  ( TestApp
  , testAppAwsScope
  , testAppStackFilePath
  , TestAppT
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
import Stackctl.AWS.Core (AccountId (..))
import Stackctl.AWS.Scope
import Stackctl.DirectoryOption
import Stackctl.FilterOption
import Test.Hspec (Spec, describe, example, it)
import Test.Hspec.Expectations.Lifted

data TestApp = TestApp
  { taLogger :: Logger
  , taMatchers :: Matchers
  , taAwsScope :: AwsScope
  , taFilterOption :: FilterOption
  , taDirectoryOption :: DirectoryOption
  }

instance HasLogger TestApp where
  loggerL = lens taLogger $ \x y -> x {taLogger = y}

instance HasMatchers TestApp where
  matchersL = lens taMatchers $ \x y -> x {taMatchers = y}

instance HasAwsScope TestApp where
  awsScopeL = lens taAwsScope $ \x y -> x {taAwsScope = y}

instance HasFilterOption TestApp where
  filterOptionL = lens taFilterOption $ \x y -> x {taFilterOption = y}

instance HasDirectoryOption TestApp where
  directoryOptionL = lens taDirectoryOption $ \x y -> x {taDirectoryOption = y}

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

instance MonadIO m => MonadFail (TestAppT m) where
  fail msg = expectationFailure msg >> error "unreachable"

runTestAppT :: MonadUnliftIO m => TestAppT m a -> m a
runTestAppT f = do
  app <-
    TestApp
      <$> newTestLogger defaultLogSettings
      <*> pure mempty
      <*> pure testAppAwsScope
      <*> pure defaultFilterOption
      <*> pure defaultDirectoryOption

  runLoggerLoggingT app $ runReaderT (unTestAppT f) app

testAppAwsScope :: AwsScope
testAppAwsScope =
  AwsScope
    { awsAccountId = AccountId "0123456789"
    , awsAccountName = "test"
    , awsRegion = "us-east-1"
    }

-- | Gives a filepath relative to 'testAwsScope'
testAppStackFilePath :: Text -> FilePath
testAppStackFilePath base =
  "stacks"
    </> "0123456789.test"
    </> "us-east-1"
    </> unpack base
    <.> "yaml"
