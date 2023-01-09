module Stackctl.TagOption
  ( tagOption
  ) where

import Stackctl.Prelude

import qualified Data.Text as T
import Options.Applicative
import Stackctl.AWS.CloudFormation (Tag, newTag)

tagOption :: Parser Tag
tagOption = option (eitherReader readTag) $ mconcat
  [ short 't'
  , long "tag"
  , metavar "KEY=[VALUE]"
  , help "Override the given Tag for this operation"
  ]

readTag :: String -> Either String Tag
readTag s = case T.breakOn "=" t of
  (_, v) | T.null v -> Left $ "No '=' found (" <> s <> ")"
  (k, _) | T.null k -> Left $ "Empty key (" <> s <> ")"
  (k, "=") -> Right $ newTag k ""
  (k, v) -> Right $ newTag k $ T.drop 1 v
  where t = pack s
