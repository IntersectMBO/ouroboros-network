-- |

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
module LedgerOnDisk.KVHandle.RangeQuery where

import LedgerOnDisk.KVHandle.OnDiskMappings
import Data.Map (Map)
import Data.Int
import qualified Data.Map.Strict as Map
type RangeQueryId = Int

data Query k v = Query
  { pointQueries :: !(Keys k v)
  , rangeQueries :: !(Map k (Map RangeQueryId Int64))
  }
  deriving stock (Eq, Show)

type instance SemigroupMap Query = KeysAreOrd

instance Ord k => Semigroup (Query k v) where
  x <> y = Query
    { pointQueries = pointQueries x <> pointQueries y
    , rangeQueries = let
        go = Map.unionWith max
        in Map.unionWith go (rangeQueries x) (rangeQueries y)
    }

instance Ord k => Monoid (Query k v) where
  mempty = Query
    { pointQueries = mempty
    , rangeQueries = Map.empty
    }

data RangeQueryMetadata k v = RangeQueryMetadata
    { remainingRows :: !Int64
    , returnedRows :: !Int64
    , nextKey :: !k
    } deriving stock (Eq, Show)
