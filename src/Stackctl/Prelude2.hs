module Stackctl.Prelude2
  ( module X
  , decodeUtf8
  , t
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

import Control.Error.Util as X (hush, note)
import Control.Monad.Logger.CallStack as X
import RIO.Directory as X (withCurrentDirectory)
import RIO.FilePath as X
  (dropExtension, takeBaseName, takeDirectory, (<.>), (</>))
import RIO.Process as X
import RIO.Text as X (pack, unpack)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

-- Temporary to make logX functions work mostly as-is
t :: Utf8Builder -> Text
t = utf8BuilderToText
