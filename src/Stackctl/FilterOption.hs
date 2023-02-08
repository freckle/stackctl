module Stackctl.FilterOption
  ( FilterOption
  , defaultFilterOption
  , HasFilterOption(..)
  , envFilterOption
  , filterOption
  , filterOptionFromPaths
  , filterOptionFromText
  , filterOptionToPaths
  , filterStackSpecs
  ) where

import Stackctl.Prelude

import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last(..))
import qualified Data.Text as T
import qualified Env
import Options.Applicative
import Stackctl.AWS.CloudFormation (StackName(..))
import Stackctl.StackSpec
import System.FilePath (hasExtension)
import System.FilePath.Glob

newtype FilterOption = FilterOption
  { unFilterOption :: NonEmpty Pattern
  }
  deriving Semigroup via Last FilterOption

instance ToJSON FilterOption where
  toJSON = toJSON . showFilterOption
  toEncoding = toEncoding . showFilterOption

class HasFilterOption env where
  filterOptionL :: Lens' env FilterOption

instance HasFilterOption FilterOption where
  filterOptionL = id

envFilterOption :: String -> Env.Parser Env.Error FilterOption
envFilterOption items =
  Env.var (first Env.UnreadError . readFilterOption) "FILTERS"
    $ Env.help
    $ "Filter "
    <> items
    <> " by patterns"

filterOption :: String -> Parser FilterOption
filterOption items = option (eitherReader readFilterOption) $ mconcat
  [ long "filter"
  , metavar "PATTERN[,PATTERN]"
  , help $ "Filter " <> items <> " to match PATTERN(s)"
  ]

filterOptionFromPaths :: NonEmpty FilePath -> FilterOption
filterOptionFromPaths = FilterOption . fmap compile

filterOptionFromText :: Text -> Maybe FilterOption
filterOptionFromText =
  fmap FilterOption
    . NE.nonEmpty
    . concatMap expandPatterns
    . filter (not . T.null)
    . map T.strip
    . T.splitOn ","

expandPatterns :: Text -> [Pattern]
expandPatterns t = map compile $ s : expanded
 where
  expanded
    | "**" `T.isPrefixOf` t = suffixed
    | otherwise = map ("**" </>) $ s : suffixed

  suffixed
    | "*" `T.isSuffixOf` t || hasExtension s = []
    | otherwise = (s </> "*") : map (s <.>) extensions

  extensions = ["json", "yaml"]

  s = unpack t

readFilterOption :: String -> Either String FilterOption
readFilterOption = note err . filterOptionFromText . pack
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

filterOptionToPaths :: FilterOption -> [FilePath]
filterOptionToPaths = map decompile . NE.toList . unFilterOption

filterStackSpecs :: FilterOption -> [StackSpec] -> [StackSpec]
filterStackSpecs fo =
  filter $ \spec -> any (`matchStackSpec` spec) $ unFilterOption fo

matchStackSpec :: Pattern -> StackSpec -> Bool
matchStackSpec p spec = or
  [ match p $ unpack $ unStackName $ stackSpecStackName spec
  , match p $ stackSpecStackFile spec
  , match p $ stackSpecTemplateFile spec
  ]
