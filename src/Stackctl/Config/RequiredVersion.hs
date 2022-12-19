module Stackctl.Config.RequiredVersion
  ( RequiredVersion(..)
  , requiredVersionFromText
  , isRequiredVersionSatisfied
  ) where

import Stackctl.Prelude

import Data.Aeson
import Data.List (uncons)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Version hiding (parseVersion)
import qualified Data.Version as Version
import Text.ParserCombinators.ReadP (readP_to_S)

data RequiredVersion = RequiredVersion
  { requiredVersionOp :: Text
  , requiredVersionCompare :: Version -> Version -> Bool
  , requiredVersionCompareWith :: Version
  }

instance Show RequiredVersion where
  show RequiredVersion {..} =
    unpack requiredVersionOp <> " " <> showVersion requiredVersionCompareWith

instance FromJSON RequiredVersion where
  parseJSON =
    withText "RequiredVersion" $ either fail pure . requiredVersionFromText

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
  parseRequiredVersion op w = do
    v <- parseVersion w

    case op of
      "=" -> Right $ RequiredVersion op (==) v
      "<" -> Right $ RequiredVersion op (<) v
      "<=" -> Right $ RequiredVersion op (<=) v
      ">" -> Right $ RequiredVersion op (>) v
      ">=" -> Right $ RequiredVersion op (>=) v
      "=~" -> Right $ RequiredVersion op (=~) v
      _ ->
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
    where s = unpack t

(=~) :: Version -> Version -> Bool
a =~ b = a >= b && a < incrementVersion b
 where
  incrementVersion = onVersion $ backwards $ onHead (+ 1)
  onVersion f = makeVersion . f . versionBranch
  backwards f = reverse . f . reverse
  onHead f as = maybe as (uncurry (:) . first f) $ uncons as

isRequiredVersionSatisfied :: RequiredVersion -> Version -> Bool
isRequiredVersionSatisfied RequiredVersion {..} =
  (`requiredVersionCompare` requiredVersionCompareWith)
