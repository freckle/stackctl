module Stackctl.CancelHandlerSpec
  ( spec
  ) where

import Stackctl.Prelude

import qualified Stackctl.CancelHandler as CancelHandler
import Test.Hspec

spec :: Spec
spec = do
  describe "with" $ do
    it "installs a handler for the duration of a block" $ example $ do
      done <- newEmptyMVar

      CancelHandler.install $ putMVar done ()
      CancelHandler.trigger

      takeMVar done `shouldReturn` ()
