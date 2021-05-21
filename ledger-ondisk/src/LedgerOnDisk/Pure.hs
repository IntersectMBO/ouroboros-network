{-# LANGUAGE LambdaCase #-}

-- |
module LedgerOnDisk.Pure where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import LedgerOnDisk.Class

pureApplyOperation ::
  (Eq k, Hashable k) =>
  HashSet k ->
  ( HashMap k (Maybe v) ->
    (HashMap k (D v), a)
  ) ->
  HashMap k v ->
  (a, HashMap k v)
pureApplyOperation scope op m =
  let restricted_map = HashMap.mapWithKey (\k _ -> HashMap.lookup k m) $ HashSet.toMap scope
      (updates, a) = op restricted_map
   in (a, applyDtoHashMap updates m )

-- we demand that domain keys (applyDToHashMaybeMap x y)  == keys y
applyDtoHashMaybeMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k (Maybe v) -> HashMap k (Maybe v)
applyDtoHashMaybeMap d_map = HashMap.mapWithKey $ \k mb_v -> applyD mb_v (HashMap.lookupDefault mempty k d_map)
