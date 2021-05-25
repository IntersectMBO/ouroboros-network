{-# LANGUAGE LambdaCase #-}
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module LedgerOnDisk.Class
  (module LedgerOnDisk.Class
  , module LedgerOnDisk.Diff
  )
where

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
import Data.Monoid
import GHC.Generics (Generic)
-- import Data.Proxy

import LedgerOnDisk.Diff

newtype QueryScope k = QueryScope (HashSet k)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid, Eq, ToExpr, Arbitrary)

querySingle :: (Hashable k) => k -> QueryScope k
querySingle = coerce . HashSet.singleton


type KVOperationResult k v = HashMap k (D v)
type KVOperation k v a = (HashMap k (Maybe v) -> (KVOperationResult k v, a))

data BaseError where
  BEBadReadSet :: BaseError
  deriving stock (Show, Eq, Generic)

instance Arbitrary BaseError where
  arbitrary = pure BEBadReadSet
  shrink = genericShrink

class (Eq k, Hashable k, Monad m) => MonadKV k v m | m -> k v where
  type Err m
  type Err m = BaseError
  data ReadSet m
  prepareOperation :: QueryScope k -> m (ReadSet m)
  submitOperation :: ReadSet m -> (HashMap k (Maybe v) -> (KVOperationResult k v, a)) -> m (Either (Err m) a)


  fromKVBaseError :: proxy m -> BaseError -> Err m
  default fromKVBaseError :: Coercible BaseError (Err m) => proxy m -> BaseError -> Err m
  fromKVBaseError _ = coerce
  toKVBaseError :: proxy m -> Err m -> Maybe BaseError
  default toKVBaseError :: Coercible BaseError (Err m) => proxy m -> Err m -> Maybe BaseError
  toKVBaseError _ = Just . coerce
  -- close :: ReadSet m -> m (_)

-- pattern KVBaseError :: forall k v m e. (MonadKV k v m, Err m ~ e) => () => KVBaseError -> e
-- pattern KVBaseError e <-  (toKVBaseError (Proxy @ m) -> Just e) where
--   KVBaseError e = fromKVBaseError (Proxy @ m) e


submitOperation_ :: MonadKV k v m => ReadSet m -> (HashMap k (Maybe v) -> KVOperationResult k v) -> m (Maybe (Err m))
submitOperation_ rs = fmap (either Just (const Nothing)) . submitOperation rs . fmap (,())

insert :: MonadKV k v m => k -> v -> m (Either (Err m) (Maybe v))
insert k v = do
  rs <- prepareOperation mempty
  submitOperation rs $ \m -> (HashMap.fromList [ (k, DChangeTo v) ], join $ HashMap.lookup k m)

lookup :: MonadKV k v m => k -> m (Either (Err m) (Maybe v))
lookup k = do
  rs <- prepareOperation . querySingle $ k
  submitOperation rs $ \x -> (mempty,  x ! k) -- guaranteed by laws to succeed

delete :: MonadKV k v m => k -> m (Either (Err m) (Maybe v))
delete k = do
  rs <- prepareOperation $ QueryScope . HashSet.singleton $ k
  submitOperation rs $ \x -> (HashMap.fromList [(k, DRemove)], x ! k)
