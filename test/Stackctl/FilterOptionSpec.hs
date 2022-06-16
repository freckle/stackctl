module Stackctl.FilterOptionSpec
  ( spec
  ) where

import Stackctl.Prelude2

import Stackctl.FilterOption
import Test.Hspec

spec :: Spec
spec = do
  describe "filterFilePaths" $ do
    it "filters paths matching any of the given patterns" $ do
      let
        option =
          filterOptionFromPaths $ "some-path" :| ["prefix/*", "**/suffix"]
        paths =
          [ "some-path"
          , "some-path-other"
          , "other-some-path"
          , "prefix/foo"
          , "prefix/foo-bar"
          , "prefix/foo-bar/prefix"
          , "foo/suffix"
          , "foo/bar/suffix"
          , "foo/suffix/bar"
          ]

      filterFilePaths option paths
        `shouldMatchList` [ "some-path"
                          , "prefix/foo"
                          , "prefix/foo-bar"
                          , "foo/suffix"
                          , "foo/bar/suffix"
                          ]
