-- | List of 'Parameter' unique by key
--
-- This module is expected to be imported qualified.
--
module Stackctl.Parameters
  ( Parameters
  , singleton
  , fromList
  , toList
  , lookup
  , require
  ) where

import Stackctl.Prelude hiding (lookup, toList)

import Data.Aeson
import qualified RIO.HashMap as HashMap
import Stackctl.AWS

newtype Parameters = Parameters
    { unParameters :: HashMap Text (Maybe Text)
    }
    deriving newtype (Monoid, FromJSON)

instance Semigroup Parameters where
  a <> b =
    Parameters $ HashMap.unionWith (<|>) (unParameters b) (unParameters a)

singleton :: Parameter -> Parameters
singleton = maybe mempty (Parameters . uncurry HashMap.singleton) . toPair

fromList :: [Parameter] -> Parameters
fromList = Parameters . HashMap.fromListWith (<|>) . mapMaybe toPair

toList :: Parameters -> [Parameter]
toList = map (uncurry makeParameter) . HashMap.toList . unParameters

toPair :: Parameter -> Maybe (Text, Maybe Text)
toPair p = do
  -- It's "impossible" to have a Parameter without a Key, but the AWS type is
  -- over-nullable, as usual. This Nothing will bubble up to singleton and
  -- fromList and result in the input being ignored: singleton will produce
  -- mempty and fromList will discard the element.
  k <- p ^. parameter_parameterKey
  pure (k, p ^. parameter_parameterValue)

lookup :: Text -> Parameters -> Maybe Text
lookup k = join . HashMap.lookup k . unParameters

require :: MonadIO m => Text -> Parameters -> m Text
require k = maybe (throwString $ unpack k <> " is required") pure . lookup k
