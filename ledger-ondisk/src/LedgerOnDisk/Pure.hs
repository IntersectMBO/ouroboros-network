{-# LANGUAGE LambdaCase #-}

-- |
module LedgerOnDisk.Pure where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Monoid
import LedgerOnDisk.Class

pureApplyOperation ::
  (Eq k, Hashable k) =>
  HashSet k ->
  ( HashMap k (Maybe v) ->
    (HashMap k (DiffItem v), a)
  ) ->
  HashMap k v ->
  (a, HashMap k v)
pureApplyOperation scope op m =
  let restricted_map = HashMap.mapWithKey (\k _ -> HashMap.lookup k m) $ HashSet.toMap scope
      (updates, a) = op restricted_map
      go k =
        Endo . \case
          DIUpdate v -> HashMap.insert k v
          DIRemove -> HashMap.delete k
      new_map = appEndo (HashMap.foldMapWithKey go updates) m
   in (a, new_map)
