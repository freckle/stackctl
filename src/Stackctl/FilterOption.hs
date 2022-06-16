module Stackctl.FilterOption
  ( FilterOption
  , HasFilterOption(..)
  , filterOption
  , filterOptionFromPaths
  , filterFilePaths
  ) where

import Stackctl.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Options.Applicative
import System.FilePath.Glob

newtype FilterOption = FilterOption
  { unFilterOption :: NonEmpty Pattern
  }

instance ToJSON FilterOption where
  toJSON = toJSON . showFilterOption
  toEncoding = toEncoding . showFilterOption

class HasFilterOption env where
  filterOptionL :: Lens' env FilterOption

instance HasFilterOption FilterOption where
  filterOptionL = id

filterOption :: String -> Parser FilterOption
filterOption items = option (eitherReader readFilterOption) $ mconcat
  [ long "filter"
  , metavar "PATTERN[,PATTERN]"
  , help $ "Filter " <> items <> " to match PATTERN(s)"
  , value defaultFilterOption
  , showDefaultWith showFilterOption
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

showFilterOption :: FilterOption -> String
showFilterOption =
  unpack
    . T.intercalate ","
    . map (pack . decompile)
    . NE.toList
    . unFilterOption

defaultFilterOption :: FilterOption
defaultFilterOption = filterOptionFromPaths $ pure "**/*"

filterFilePaths :: FilterOption -> [FilePath] -> [FilePath]
filterFilePaths fo = filter $ \path -> any (`match` path) $ unFilterOption fo
