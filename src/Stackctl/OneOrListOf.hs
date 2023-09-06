module Stackctl.OneOrListOf
  ( OneOrListOf
  , fromList
  ) where

import Stackctl.Prelude

import Data.Aeson

-- | Type representing one @a@ or a list of @a@
--
-- This type is isomorphic both @'NonEmpty' a@ and @'Either' a [a]@. Its primary
-- use-case is to parse Yaml (through its 'FromJSON') where users may specify a
-- list of values, but specifying a single value is worth supporting, typically
-- for backwards-compatibility:
--
-- @
-- something:
--   field:
--     - one
--     - two
--
-- something:
--   field: one # => should be treated like field: [one]
-- @
--
-- The 'Foldable' instance should be used to treat the value like a list, such
-- as extracting it directly via 'toList'.
--
-- Implementation note: this type preserves the form in which it was decoded (in
-- other words, it's not a @newtype@ over one of the isomorphic types mentioned
-- above), so that we can encode it back out in the same format.
data OneOrListOf a = One a | List [a]
  deriving stock (Eq, Show, Generic, Foldable)

fromList :: [a] -> OneOrListOf a
fromList = List

instance Semigroup (OneOrListOf a) where
  One a <> One b = List [a, b]
  One a <> List bs = List $ a : bs
  List as <> One b = List $ as <> [b]
  List as <> List bs = List $ as <> bs

instance FromJSON a => FromJSON (OneOrListOf a) where
  parseJSON = \case
    Array xs -> List . toList <$> traverse parseJSON xs
    v -> One <$> parseJSON v

instance ToJSON a => ToJSON (OneOrListOf a) where
  toJSON = \case
    One a -> toJSON a
    List as -> toJSON as
  toEncoding = \case
    One a -> toEncoding a
    List as -> toEncoding as
