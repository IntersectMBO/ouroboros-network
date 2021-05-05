{-# language GADTs#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LedgerOnDisk.Class where

import Data.HashMap.Strict(HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable


newtype QueryScope k = QueryScope (HashSet k)
  deriving newtype (Semigroup, Monoid)

-- data Operation k v a where
--   OInsert :: k -> v -> Operation k v ()
--   ODelete :: k -> Operation k v ()
--   OLookup :: k -> Operation k v v

data DiffItem v where
  DIUpdate :: v -> DiffItem v
  DIRemove :: DiffItem v

type OperationResult k v = HashMap k (DiffItem v)

class (Eq k, Hashable k, Monad m) => MonadKV k v m | m -> k v where
  data ResultSet m
  prepareOperation :: QueryScope k -> m (ResultSet m)
  submitOperation :: ResultSet m -> (HashMap k v -> (OperationResult k v, a)) -> m a

submitOperation_ :: MonadKV k v m => ResultSet m -> (HashMap k v -> OperationResult k v) -> m ()
submitOperation_ rs = submitOperation rs . fmap (,())

insert :: MonadKV k v m => k -> v -> m ()
insert k v = do
  rs <- prepareOperation mempty
  submitOperation_ rs $ \_ -> HashMap.fromList [ (k, DIUpdate v) ]

lookup :: MonadKV k v m => k -> m v
lookup k = do
  rs <- prepareOperation . QueryScope . HashSet.singleton $ k
  submitOperation rs $ \x -> (mempty,  x ! k)

delete :: MonadKV k v m => k -> m v
delete k = do
  rs <- prepareOperation $ QueryScope . HashSet.singleton $ k
  submitOperation rs $ \x -> (HashMap.fromList [(k, DIRemove)], x ! k)
