{-# language GADTs#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module LedgerOnDisk.Class where

import Data.HashMap.Strict(HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Coerce

newtype QueryScope k = QueryScope (HashSet k)
  deriving newtype (Semigroup, Monoid)

querySingle :: k -> QueryScope k
querySingle = coerce . HashSet.singleton

data DiffItem v where
  DIUpdate :: v -> DiffItem v
  DIRemove :: DiffItem v

type OperationResult k v = HashMap k (DiffItem v)

class (Eq k, Hashable k, Monad m) => MonadKV k v m | m -> k v where
  data ResultSet m
  data Err m
  prepareOperation :: QueryScope k -> m (ResultSet m)
  submitOperation :: ResultSet m -> (HashMap k (Maybe v) -> (OperationResult k v, a)) -> m (Either (Err m) a)


submitOperation_ :: MonadKV k v m => ResultSet m -> (HashMap k (Maybe v) -> OperationResult k v) -> m (Maybe (Err m))
submitOperation_ rs = fmap (either Just (const Nothing)) . submitOperation rs . fmap (,())

insert :: MonadKV k v m => k -> v -> m (Either (Err m) (Maybe v))
insert k v = do
  rs <- prepareOperation mempty
  submitOperation rs $ \m -> (HashMap.fromList [ (k, DIUpdate v) ], join $ HashMap.lookup k m)

lookup :: MonadKV k v m => k -> m (Either (Err m) (Maybe v))
lookup k = do
  rs <- prepareOperation . querySingle $ k
  submitOperation rs $ \x -> (mempty,  x ! k) -- guaranteed by laws to succeed

delete :: MonadKV k v m => k -> m (Either (Err m) (Maybe v))
delete k = do
  rs <- prepareOperation $ QueryScope . HashSet.singleton $ k
  submitOperation rs $ \x -> (HashMap.fromList [(k, DIRemove)], x ! k)
