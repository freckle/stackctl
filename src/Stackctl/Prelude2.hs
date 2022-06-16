{-# OPTIONS_GHC -Wno-orphans #-}

module Stackctl.Prelude2
  ( module X
  , decodeUtf8
  ) where

import RIO as X hiding
  ( LogLevel(..)
  , LogSource
  , logDebug
  , logDebugS
  , logError
  , logErrorS
  , logInfo
  , logInfoS
  , logOther
  , logOtherS
  , logWarn
  , logWarnS
  )

import Blammo.Logging as X
import Control.Error.Util as X (hush, note)
import Data.Aeson as X (ToJSON(..), object)
import RIO.Directory as X (withCurrentDirectory)
import RIO.FilePath as X
  (dropExtension, takeBaseName, takeDirectory, (<.>), (</>))
import RIO.Process as X
import RIO.Text as X (pack, unpack)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

instance ToJSON Utf8Builder where
  toJSON = toJSON . utf8BuilderToText
  toEncoding = toEncoding . utf8BuilderToText
