module Stackctl.StackDescription
  ( StackDescription (..)
  , addStackDescription
  ) where

import Stackctl.Prelude

import Control.Lens ((?~))
import Data.Aeson (FromJSON, Value (..))
import qualified Data.Aeson as JSON
import Data.Aeson.Lens
import Data.ByteString.Char8 as BS8
import qualified Data.Yaml as Yaml

newtype StackDescription = StackDescription
  { unStackDescription :: Text
  }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

data BodyContent
  = BodyContentJSON Value
  | BodyContentYaml Value

addStackDescription :: Maybe StackDescription -> Text -> Text
addStackDescription mStackDescription body = fromMaybe body $ do
  StackDescription d <- mStackDescription
  bc <- getBodyContent bs
  decodeUtf8 <$> case bc of
    BodyContentJSON v -> updateJSON d bs <$ guard (not $ hasDescription v)
    BodyContentYaml v -> updateYaml d bs <$ guard (not $ hasDescription v)
 where
  bs = encodeUtf8 body

getBodyContent :: ByteString -> Maybe BodyContent
getBodyContent body =
  asum
    [ BodyContentJSON . Object <$> JSON.decodeStrict body
    , hush $ BodyContentYaml . Object <$> Yaml.decodeEither' body
    ]

-- Inserting a key is easy to do in Yaml without the parsing round-trip that
-- would strip formatting and comments. But updating a key is hard. To avoid
-- this, we just say that we never clobber existing keys.
hasDescription :: Value -> Bool
hasDescription = isJust . (^? key "Description" . _String)

-- For JSON, don't worry about preserving formatting; do a proper update.
updateJSON :: Text -> ByteString -> ByteString
updateJSON d = atKey "Description" ?~ String d

-- For Yaml, insert textually to avoid a round-trip dropping comments or
-- changing whitespace. We rely on 'Show' as a naive escape.
updateYaml :: Text -> ByteString -> ByteString
updateYaml d bs = "Description: " <> BS8.pack (show d) <> "\n" <> bs
