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
import Data.Monoid
import GHC.Generics (Generic)
-- import Data.Proxy

newtype QueryScope k = QueryScope (HashSet k)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid, Eq, ToExpr, Arbitrary)

querySingle :: (Hashable k) => k -> QueryScope k
querySingle = coerce . HashSet.singleton

data D v where
  DChangeTo :: v -> D v
  DRemove :: D v
  DNoChange :: D v
  -- this is interesting, but it makes things more complicated. Inhibits functor
  -- instance (though I think this could be surmounted with a CoYoneda trick)
  -- DIMappend :: Monoid v => v -> D v
  deriving stock (Show, Eq)

instance Semigroup (D v) where
  x <> DNoChange = x
  _ <> y = y

instance Monoid (D v) where
  mempty = DNoChange

applyDforK :: (Eq k, Hashable k) => k -> D v -> HashMap k v -> HashMap k v
applyDforK k = \case
  DChangeTo v -> HashMap.insert k v
  DRemove -> HashMap.delete k
  DNoChange -> id
  -- DIMappend v -> HashMap.insertWith (<>) k v

applyDtoHashMap :: (Eq k, Hashable k) => HashMap k (D v) -> HashMap k v -> HashMap k v
applyDtoHashMap d = appEndo (HashMap.foldMapWithKey go d) where
  go k = Endo . applyDforK k

instance Arbitrary v => Arbitrary (D v) where
  arbitrary = oneof [ pure DRemove, DChangeTo <$> arbitrary ]
  shrink di = case di of
    DRemove -> []
    DChangeTo v -> DChangeTo <$> shrink v
    _ -> []

type KVOperationResult k v = HashMap k (D v)
type KVOperation k v a = (HashMap k (Maybe v) -> (KVOperationResult k v, a))

data BaseError where
  BEBadResultSet :: BaseError
  deriving stock (Show, Eq, Generic)

instance Arbitrary BaseError where
  arbitrary = pure BEBadResultSet
  shrink = genericShrink

class (Eq k, Hashable k, Monad m) => MonadKV k v m | m -> k v where
  type Err m
  type Err m = BaseError
  data ResultSet m
  prepareOperation :: QueryScope k -> m (ResultSet m)
  submitOperation :: ResultSet m -> (HashMap k (Maybe v) -> (KVOperationResult k v, a)) -> m (Either (Err m) a)


  fromKVBaseError :: proxy m -> BaseError -> Err m
  default fromKVBaseError :: Coercible BaseError (Err m) => proxy m -> BaseError -> Err m
  fromKVBaseError _ = coerce
  toKVBaseError :: proxy m -> Err m -> Maybe BaseError
  default toKVBaseError :: Coercible BaseError (Err m) => proxy m -> Err m -> Maybe BaseError
  toKVBaseError _ = Just . coerce
  -- close :: ResultSet m -> m (_)

-- pattern KVBaseError :: forall k v m e. (MonadKV k v m, Err m ~ e) => () => KVBaseError -> e
-- pattern KVBaseError e <-  (toKVBaseError (Proxy @ m) -> Just e) where
--   KVBaseError e = fromKVBaseError (Proxy @ m) e


submitOperation_ :: MonadKV k v m => ResultSet m -> (HashMap k (Maybe v) -> KVOperationResult k v) -> m (Maybe (Err m))
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
