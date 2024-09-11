module Stackctl.Spec.Changes.FormatSpec
  ( spec
  )
where

import Stackctl.Prelude

import Data.Aeson
import Stackctl.AWS.CloudFormation (ChangeSetType (..), changeSetFromResponse)
import Stackctl.Colors
import Stackctl.Spec.Changes.Format
import System.FilePath ((-<.>))
import System.FilePath.Glob (globDir1)
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  describe "formatChangeSet" $ do
    paths <- runIO $ globDir1 "**/*.json" "test/files/change-sets"

    for_ paths $ \path -> do
      for_ [minBound .. maxBound] $ \fmt -> do
        it (path <> " as " <> show fmt) $ do
          formatChangeSetGolden path fmt

formatChangeSetGolden :: FilePath -> Format -> IO (Golden Text)
formatChangeSetGolden path fmt = do
  actual <-
    formatChangeSet noColors OmitFull "some-stack" fmt
      . (changeSetFromResponse ChangeSetType_UPDATE <=< decodeStrict)
      . encodeUtf8
      <$> readFileUtf8 path

  pure
    $ Golden
      { output = actual
      , encodePretty = unpack
      , writeToFile = writeFileUtf8
      , readFromFile = readFileUtf8
      , goldenFile = path -<.> ext
      , actualFile = Nothing
      , failFirstTime = False
      }
 where
  ext = case fmt of
    FormatTTY -> "txt"
    FormatPullRequest -> "md"
