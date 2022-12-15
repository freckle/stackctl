{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Stackctl.Config.RequiredVersionSpec
  ( spec
  ) where

import Stackctl.Prelude

import Data.Version
import Stackctl.Config.RequiredVersion
import Test.Hspec

data TestCase = TestCase
  { tcOp :: Maybe Text
  , tcRequired :: Text
  , tcVersions :: [([Int], Bool)]
  }

-- NB. These aren't exhaustive, but I am le tired
testCases :: [TestCase]
testCases =
  [ TestCase
    Nothing
    "1.2.3.4"
    [ ([1, 2, 3, 4], True)
    , ([1, 2, 3, 3], False)
    , ([1, 2, 3, 5], False)
    , ([1, 2, 4], False)
    ]
  , TestCase
    (Just "=")
    "1.2.3.4"
    [ ([1, 2, 3, 4], True)
    , ([1, 2, 3, 3], False)
    , ([1, 2, 3, 5], False)
    , ([1, 2, 4], False)
    ]
  , TestCase
    (Just ">=")
    "1.2.3.4"
    [ ([1, 2, 3, 4], True)
    , ([1, 2, 3, 5], True)
    , ([1, 2, 4], True)
    , ([1, 2, 3, 3], False)
    , ([1, 2, 3], False)
    ]
  , TestCase
    (Just ">=")
    "1.2"
    [ ([1, 2], True)
    , ([1, 2, 1], True)
    , ([1, 1, 9], False)
    , ([1, 1, 9, 11], False)
    , ([1, 0], False)
    ]
  , TestCase
    (Just "<=")
    "1.2.3.4"
    [ ([1, 2, 3, 4], True)
    , ([1, 2, 3, 3], True)
    , ([1, 2, 3], True)
    , ([1, 2, 3, 5], False)
    , ([1, 2, 3, 4, 1], False)
    , ([1, 2, 4], False)
    ]
  , TestCase
    (Just "<=")
    "1.2"
    [ ([1, 2], True)
    , ([1, 2, 1], False)
    , ([1, 1, 9], True)
    , ([1, 1, 9, 11], True)
    , ([1, 0], True)
    ]
  , TestCase
    (Just "=~")
    "1.2.3.4"
    [([1, 2, 3, 3], False), ([1, 2, 3, 4], True), ([1, 2, 3, 5], False)]
  , TestCase
    (Just "=~")
    "1.2.3"
    [ ([1, 2, 3], True)
    , ([1, 2, 3, 1], True)
    , ([1, 2, 3, 9], True)
    , ([1, 2, 4], False)
    , ([1, 2], False)
    ]
  ]

spec :: Spec
spec = do
  describe "RequiredVersion" $ do
    for_ testCases $ \TestCase {..} -> do
      for_ tcVersions $ \(vb, satisfies) -> do
        let
          t = maybe "" (<> " ") tcOp <> tcRequired
          v = makeVersion vb

          docstring =
            "treats \""
              <> unpack t
              <> "\" as "
              <> (if satisfies then "satisfied" else "NOT satisfied")
              <> " by "
              <> showVersion v

          Right rv = requiredVersionFromText t

        it docstring $ if satisfies
          then rv `shouldSatisfy` (`isRequiredVersionSatisfied` v)
          else rv `shouldSatisfy` (not . (`isRequiredVersionSatisfied` v))
