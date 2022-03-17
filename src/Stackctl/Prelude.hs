module Stackctl.Prelude
  ( module X
  , decodeUtf8
  ) where

import Control.Error.Util as X (hush, note)
import RIO as X
import RIO.Directory as X (withCurrentDirectory)
import RIO.FilePath as X
  (dropExtension, takeBaseName, takeDirectory, (<.>), (</>))
import RIO.Process as X
import RIO.Text as X (pack, unpack)

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
