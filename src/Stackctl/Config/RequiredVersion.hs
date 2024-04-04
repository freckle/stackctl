module Stackctl.Config.RequiredVersion
  ( RequiredVersion (..)
  , RequiredVersionOp (..)
  , requiredVersionToText
  , requiredVersionFromText
  , isRequiredVersionSatisfied

    -- * Exported for testing
  , (=~)
  ) where

import Stackctl.Prelude

import Data.Aeson
import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Version hiding (parseVersion)
import qualified Data.Version as Version
import Test.QuickCheck
import Text.ParserCombinators.ReadP (readP_to_S)

data RequiredVersion = RequiredVersion
  { requiredVersionOp :: RequiredVersionOp
  , requiredVersionCompareWith :: Version
  }
  deriving stock (Eq, Ord, Show)

instance Arbitrary RequiredVersion where
  arbitrary = RequiredVersion <$> arbitrary <*> arbitrary

instance FromJSON RequiredVersion where
  parseJSON =
    withText "RequiredVersion" $ either fail pure . requiredVersionFromText

instance ToJSON RequiredVersion where
  toJSON = toJSON . requiredVersionToText
  toEncoding = toEncoding . requiredVersionToText

requiredVersionToText :: RequiredVersion -> Text
requiredVersionToText RequiredVersion {..} =
  requiredVersionOpToText requiredVersionOp
    <> " "
    <> pack
      (showVersion requiredVersionCompareWith)

requiredVersionFromText :: Text -> Either String RequiredVersion
requiredVersionFromText = fromWords . T.words
 where
  fromWords :: [Text] -> Either String RequiredVersion
  fromWords = \case
    [w] -> parseRequiredVersion "=" w
    [op, w] -> parseRequiredVersion op w
    ws ->
      Left
        $ show (unpack $ T.unwords ws)
        <> " did not parse as optional operator and version string"

  parseRequiredVersion :: Text -> Text -> Either String RequiredVersion
  parseRequiredVersion op w = RequiredVersion <$> parseOp op <*> parseVersion w

  parseOp :: Text -> Either String RequiredVersionOp
  parseOp = \case
    "=" -> Right RequiredVersionEQ
    "==" -> Right RequiredVersionEQ
    "<" -> Right RequiredVersionLT
    "<=" -> Right RequiredVersionLTE
    ">" -> Right RequiredVersionGT
    ">=" -> Right RequiredVersionGTE
    "=~" -> Right RequiredVersionIsh
    op ->
      Left
        $ "Invalid comparison operator ("
        <> unpack op
        <> "), may only be =, <, <=, >, >=, or =~"

  parseVersion :: Text -> Either String Version
  parseVersion t =
    fmap (fst . NE.last)
      $ note ("Failed to parse as a version " <> s)
      $ NE.nonEmpty
      $ readP_to_S Version.parseVersion s
   where
    s = unpack t

isRequiredVersionSatisfied :: RequiredVersion -> Version -> Bool
isRequiredVersionSatisfied RequiredVersion {..} =
  (`requiredVersionCompare` requiredVersionCompareWith)
 where
  requiredVersionCompare = requiredVersionOpCompare requiredVersionOp

data RequiredVersionOp
  = RequiredVersionEQ
  | RequiredVersionLT
  | RequiredVersionLTE
  | RequiredVersionGT
  | RequiredVersionGTE
  | RequiredVersionIsh
  deriving stock (Eq, Ord, Show, Bounded, Enum)

instance Arbitrary RequiredVersionOp where
  arbitrary = arbitraryBoundedEnum

requiredVersionOpToText :: RequiredVersionOp -> Text
requiredVersionOpToText = \case
  RequiredVersionEQ -> "=="
  RequiredVersionLT -> "<"
  RequiredVersionLTE -> "<="
  RequiredVersionGT -> ">"
  RequiredVersionGTE -> ">="
  RequiredVersionIsh -> "=~"

requiredVersionOpCompare :: RequiredVersionOp -> Version -> Version -> Bool
requiredVersionOpCompare = \case
  RequiredVersionEQ -> (==)
  RequiredVersionLT -> (<)
  RequiredVersionLTE -> (<=)
  RequiredVersionGT -> (>)
  RequiredVersionGTE -> (>=)
  RequiredVersionIsh -> (=~)

(=~) :: Version -> Version -> Bool
a =~ b = a >= b && a < incrementVersion b
 where
  incrementVersion = onVersion $ backwards $ onHead (+ 1)
  onVersion f = makeVersion . f . versionBranch
  backwards f = reverse . f . reverse
  onHead f as = maybe as (uncurry (:) . first f) $ uncons as
