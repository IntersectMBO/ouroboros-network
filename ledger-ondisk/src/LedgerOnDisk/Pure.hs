{-# LANGUAGE LambdaCase #-}

-- |
module LedgerOnDisk.Pure where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import LedgerOnDisk.Diff

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

