module Stackctl.Prelude
  ( module X
  , decodeUtf8
  , parseEither
  , extendObject
  ) where

import Control.Error.Util as X (hush, note)
import RIO as X
import RIO.Directory as X (withCurrentDirectory)
import RIO.FilePath as X
  (dropExtension, takeBaseName, takeDirectory, (<.>), (</>))
import RIO.Process as X
import RIO.Text as X (pack, unpack)

import Data.Aeson
import qualified RIO.HashMap as HashMap

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

parseEither :: FromJSON a => Value -> Either String a
parseEither v = case fromJSON v of
  Error msg -> Left msg
  Success a -> Right a

extendObject :: Value -> Value -> Value
extendObject (Object a) (Object b) = Object $ HashMap.union b a
extendObject a _ = a
