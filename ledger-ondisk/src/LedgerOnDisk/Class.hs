{-# language GADTs#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}
module LedgerOnDisk.Class where

import Data.HashMap.Strict(HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet


newtype QueryScope k = QueryScope (HashSet k)

-- data Operation k v a where
--   OInsert :: k -> v -> Operation k v ()
--   ODelete :: k -> Operation k v ()
--   OLookup :: k -> Operation k v v

data DiffItem v where
  DIUpdate :: v -> DiffItem v
  DIRemove :: DiffItem v

type OperationResult k v = HashMap k (DiffItem v)

class MonadKV k v m | m -> k v where
  data ResultSet m
  prepareOperation :: QueryScope k -> m (ResultSet m)
  submitOperation :: ResultSet m -> (HashMap k v -> (OperationResult k v, a)) -> m a
