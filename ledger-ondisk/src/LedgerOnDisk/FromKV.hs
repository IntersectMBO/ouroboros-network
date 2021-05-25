{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module LedgerOnDisk.FromKV where

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Proxy
import Data.Functor.Identity
import LedgerOnDisk.Diff
import Control.Monad.Except
import Data.GADT.Compare
import Control.Monad.State
import Data.Coerce
import Data.Kind
import Data.Dependent.Sum (DSum ((:=>)))


class MonadKV1 f m | m -> f where
  data KV1Err m
  data ReadSet m
  prepare :: DMap f Proxy -> m (ReadSet m)
  submit :: ReadSet m -> (DMap f Identity -> (DMap f D, a)) -> m (Either (KV1Err m) a)

-- An instance "a" of this class is isomorphic to (O a, DMap (K a) Identity)
class GCompare (K a) => FromKV' a where
  type O a :: Type -- The "other" part of the type, that's not in the dmap
  type K a :: Type -> Type
  fromKV :: O a -> DMap (K a) Identity -> a
  toKV :: a -> (O a, DMap (K a) Identity)

  -- It's not clear that this is needed
  applyDiff :: DMap (K a) D -> a -> a

class (K a ~ f, FromKV' a) => FromKV f a where
instance (K a ~ f, FromKV' a) => FromKV f a where


load :: (MonadError (KV1Err m) m, MonadKV1 f m, FromKV f a, GCompare f)
  => O a
  -> DMap f Proxy
  -> m a
load other dm_px = prepare dm_px >>= \rs -> do
  r <- submit rs $ \dm_i -> (mempty, fromKV other dm_i)
  either throwError pure r


newtype KVStateT s m a = KVStateT { unKVStateT :: StateT (s, DMap (K s) Identity, DMap (K s) D) m a }
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

kvsApplyD :: (Monad m, FromKV f s) => DSum f D -> KVStateT s m ()
kvsApplyD (k :=> v) = KVStateT $ modify' $ \(s, dm_i, dm_d) -> let
  singleton_dm = DMap.singleton k v
  new_dm_d = DMap.unionWithKey (\_k v1 v2 -> v1 <> v2) dm_d singleton_dm
  new_dm_i = applyDtoDMap singleton_dm dm_i
  in (applyDiff singleton_dm s, new_dm_i, new_dm_d)


-- deriving stock instance Functor m => Functor (KVStateT s m)
-- instance Functor m => Functor (KVStateT s m) where
--   fmap f (KVStateT m) = KVStateT $ fmap f m
--   x <$ (KVStateT m) = KVStateT $ x <$ m

  -- (<*>) :: forall x y (f :: * -> *).  KVStateT s m (x -> y) -> KVStateT s m x -> KVStateT s m y
  -- KVStateT (x :: StateT (s, DMap f Identity, DMap f D) _ _) <*> KVStateT (y :: StateT (s, DMap f Identity, DMap f D) _ _) = KVStateT $ x <*> y
