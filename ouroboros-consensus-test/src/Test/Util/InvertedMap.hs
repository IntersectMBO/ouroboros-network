module Test.Util.InvertedMap (
    -- * InvertedMap type
    InvertedMap
    -- * Query
  , Test.Util.InvertedMap.null
    -- * Construction
  , toMap
  , unsafeInvertedMap
    -- * Conversion
  , fromMap
  , unsafeCoercion
    -- * Filter
  , spanAntitone
    -- * Min/Max
  , minViewWithKey
  ) where

import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Type.Coercion

-- | An inverted 'Map'
--
-- INVARIANT the @k@s are all unique
--
-- INVARIANT the 'NonEmpty's are all ascending
--
newtype InvertedMap v k = UnsafeInvertedMap {getInvertedMap :: Map v (NonEmpty k)}
  deriving (Show)

unsafeCoercion :: Coercion (InvertedMap v k) (Map v (NonEmpty k))
unsafeCoercion = Coercion

unsafeInvertedMap :: Map v (NonEmpty k) -> InvertedMap v k
unsafeInvertedMap = UnsafeInvertedMap

-- | This inverts the given 'Map'
--
fromMap :: Ord v => Map k v -> InvertedMap v k
fromMap m =
    UnsafeInvertedMap $ Map.fromListWith (<>) $
    [ (v, k NE.:| []) | (k, v) <- Map.toList m ]

minViewWithKey :: InvertedMap v k -> Maybe ((v, NonEmpty k), InvertedMap v k)
minViewWithKey =
    fmap (fmap UnsafeInvertedMap) . Map.minViewWithKey . getInvertedMap

null :: InvertedMap v k -> Bool
null = Map.null . getInvertedMap

spanAntitone :: (v -> Bool) -> InvertedMap v k -> (InvertedMap v k, InvertedMap v k)
spanAntitone f (UnsafeInvertedMap m) = (UnsafeInvertedMap l, UnsafeInvertedMap r)
  where
    (l, r) = Map.spanAntitone f m

-- | This inverts the given 'InvertedMap'
--
-- Inversion is an <https://en.wikipedia.org/wiki/Involution_(mathematics)>, so
-- this returns to 'Map'.
--
toMap :: Ord k => InvertedMap v k -> Map k v
toMap (UnsafeInvertedMap m) =
    Map.fromList $
    [ (k, v) | (v, ks) <- Map.toList m, k <- NE.toList ks ]
