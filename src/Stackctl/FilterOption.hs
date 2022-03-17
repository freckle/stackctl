module Stackctl.FilterOption
  ( FilterOption
  , filterOption
  , filterOptionFromPaths
  , filterFilePaths
  ) where

import Stackctl.Prelude

import Options.Applicative
import qualified RIO.NonEmpty as NE
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (splitOn)
import System.FilePath.Glob

newtype FilterOption = FilterOption
  { unFilterOption :: NonEmpty Pattern
  }

instance Display FilterOption where
  display =
    display
      . T.intercalate ","
      . map (pack . decompile)
      . NE.toList
      . unFilterOption

filterOption :: String -> Parser FilterOption
filterOption items = option (eitherReader readFilterOption) $ mconcat
  [ long "filter"
  , metavar "PATTERN[,PATTERN]"
  , help $ "Filter " <> items <> " to match PATTERN(s)"
  ]

filterOptionFromPaths :: NonEmpty FilePath -> FilterOption
filterOptionFromPaths = FilterOption . fmap compile

readFilterOption :: String -> Either String FilterOption
readFilterOption =
  maybe (Left err) (Right . FilterOption)
    . NE.nonEmpty
    . map (compile . unpack)
    . filter (not . T.null)
    . map T.strip
    . T.splitOn ","
    . pack
  where err = "Must be non-empty, comma-separated list of non-empty patterns"

filterFilePaths :: FilterOption -> [FilePath] -> [FilePath]
filterFilePaths fo = filter $ \path -> any (`match` path) $ unFilterOption fo
