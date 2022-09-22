{-# LANGUAGE PatternSynonyms #-}

-- | This module is a restricted interface to the
-- @Data.Map.Diff.Strict.Internal@ module, which should enforce several
-- invariants related to group theory.
module Data.Map.Diff.Strict (
    module X
  , pattern Delete
  , pattern Insert
  ) where

import           Data.Map.Diff.Strict.Internal as X (Diff, DiffEntry,
                     DiffHistory, Keys (..), Values (..), diff, diffKeys,
                     forwardValues, forwardValuesAndKeys, fromList,
                     fromListDeletes, fromListInserts, fromSeq,
                     isNonEmptyHistory, keysFromList, restrictValues,
                     singletonDelete, singletonInsert, valuesFromList)

import qualified Data.Map.Diff.Strict.Internal as Internal



{-# COMPLETE Insert, Delete #-}

pattern Insert :: v -> DiffEntry v
pattern Insert x = Internal.Insert x

pattern Delete :: v -> DiffEntry v
pattern Delete x = Internal.Delete x
