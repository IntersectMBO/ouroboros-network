{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# language GADTs#-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DefaultSignatures #-}
module LedgerOnDisk.Class where

import Data.HashMap.Strict(HashMap, (!))
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet(HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Coerce
import Control.Monad
import Test.QuickCheck
import Data.TreeDiff.Class
import Test.QuickCheck.Instances.UnorderedContainers ()
-- import Data.Proxy

newtype QueryScope k = QueryScope (HashSet k)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid, Eq, ToExpr, Arbitrary)

querySingle :: (Hashable k) => k -> QueryScope k
querySingle = coerce . HashSet.singleton

data DiffItem v where
  DIUpdate :: v -> DiffItem v
  DIRemove :: DiffItem v
  deriving stock (Show, Eq)

instance Arbitrary v => Arbitrary (DiffItem v) where
  arbitrary = oneof [ pure DIRemove, DIUpdate <$> arbitrary ]
  shrink di = case di of
    DIRemove -> []
    DIUpdate v -> DIUpdate <$> shrink v

type OperationResult k v = HashMap k (DiffItem v)

type KVOperation k v a = (HashMap k (Maybe v) -> (OperationResult k v, a))

data KVBaseError where
  KVEBadResultSet :: KVBaseError
  deriving stock (Show, Eq)


class (Eq k, Hashable k, Monad m) => MonadKV k v m | m -> k v where
  type Err m
  type Err m = KVBaseError
  data ResultSet m
  prepareOperation :: QueryScope k -> m (ResultSet m)
  submitOperation :: ResultSet m -> (HashMap k (Maybe v) -> (OperationResult k v, a)) -> m (Either (Err m) a)
  fromKVBaseError :: proxy m -> KVBaseError -> Err m
  default fromKVBaseError :: Coercible KVBaseError (Err m) => proxy m -> KVBaseError -> Err m
  fromKVBaseError _ = coerce
  toKVBaseError :: proxy m -> Err m -> Maybe KVBaseError
  default toKVBaseError :: Coercible KVBaseError (Err m) => proxy m -> Err m -> Maybe KVBaseError
  toKVBaseError _ = Just . coerce
  -- close :: ResultSet m -> m (_)

-- pattern KVBaseError :: forall k v m e. (MonadKV k v m, Err m ~ e) => () => KVBaseError -> e
-- pattern KVBaseError e <-  (toKVBaseError (Proxy @ m) -> Just e) where
--   KVBaseError e = fromKVBaseError (Proxy @ m) e


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
