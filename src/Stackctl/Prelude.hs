module Stackctl.Prelude
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
import Data.Text as X (pack, unpack)
import System.FilePath as X
  (dropExtension, takeBaseName, takeDirectory, (<.>), (</>))
import UnliftIO.Directory as X (withCurrentDirectory)

{-# ANN module ("HLint: ignore Avoid restricted qualification" :: String) #-}

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
