module Stackctl.Tag
  ( Tag(..)
  , tagOption
  ) where

import Stackctl.Prelude

import Options.Applicative

newtype Tag = Tag
  { unTag :: Text
  }
  deriving newtype Display

-- brittany-disable-next-binding

tagOption :: String -> Parser Tag
tagOption h = Tag <$> strOption
  (  short 't'
  <> long "tag"
  <> metavar "TAG"
  <> help h
  )
