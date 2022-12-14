module Stackctl.FilterOption
  ( FilterOption
  , HasFilterOption(..)
  , filterOption
  , filterOptionFromPaths
  , filterStackSpecs
  ) where

import Stackctl.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Options.Applicative
import Stackctl.AWS.CloudFormation (StackName(..))
import Stackctl.StackSpec
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

filterStackSpecs :: FilterOption -> [StackSpec] -> [StackSpec]
filterStackSpecs fo =
  filter $ \spec -> any (`matchStackSpec` spec) $ unFilterOption fo

matchStackSpec :: Pattern -> StackSpec -> Bool
matchStackSpec p spec = or
  [ match p $ unpack $ unStackName $ stackSpecStackName spec
  , match p $ stackSpecStackFile spec
  , match p $ stackSpecTemplateFile spec
  ]
