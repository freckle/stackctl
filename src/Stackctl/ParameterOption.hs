module Stackctl.ParameterOption
  ( parameterOption
  ) where

import Stackctl.Prelude

import qualified Data.Text as T
import Options.Applicative
import Stackctl.AWS.CloudFormation (Parameter, makeParameter)

parameterOption :: Parser Parameter
parameterOption =
  option (eitherReader readParameter)
    $ mconcat
      [ short 'p'
      , long "parameter"
      , metavar "KEY=[VALUE]"
      , help "Override the given Parameter for this operation"
      ]

readParameter :: String -> Either String Parameter
readParameter s = case T.breakOn "=" t of
  (_, v) | T.null v -> Left $ "No '=' found (" <> s <> ")"
  (k, _) | T.null k -> Left $ "Empty key (" <> s <> ")"
  (k, "=") -> Right $ makeParameter k $ Just ""
  (k, v) -> Right $ makeParameter k $ Just $ T.drop 1 v
 where
  t = pack s
