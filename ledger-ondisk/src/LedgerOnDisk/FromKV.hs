{-# LANGUAGE LambdaCase #-}
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

{-# LANGUAGE EmptyCase #-}
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
import Data.Functor ((<&>))
import GHC.Generics hiding (D)
import Data.Void
import Data.Functor.Const


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

  applyDiff :: DMap (K a) D -> a -> a

class (K a ~ f, FromKV' a) => FromKV f a where
instance (K a ~ f, FromKV' a) => FromKV f a where

newtype VoidF a = VoidF (Const Void a)

instance GEq VoidF where
  geq ~(VoidF (Const v)) _ = absurd v

instance GCompare VoidF where
  gcompare ~(VoidF (Const v)) _ = absurd v

instance FromKV' (V1 p) where
  type O (V1 p) = Void
  type K (V1 p) = VoidF

  fromKV v _ = absurd v
  toKV v = case v of {}
  applyDiff _ v = case v of {}

-- instance GEq (U1 p) where

-- instance FromKV' (U1 p) where
--   type O (U1 p) = ()
--   type K (U1 p) = Const ()


load :: (MonadError (KV1Err m) m, MonadKV1 f m, FromKV f a, GCompare f)
  => O a
  -> DMap f Proxy
  -> m a
load other dm_px = prepare dm_px >>= \rs -> do
  r <- submit rs $ \dm_i -> (mempty, fromKV other dm_i)
  either throwError pure r


newtype KVStateT s m a = KVStateT { unKVStateT :: StateT (s, DMap (K s) D) m a }
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadTrans)

kvsApplyD :: (Monad m, FromKV f s) => DSum f D -> KVStateT s m ()
kvsApplyD (k :=> v) = KVStateT $ modify' $ \(s, dm_d) -> let
  singleton_dm = DMap.singleton k v
  new_dm_d = DMap.unionWithKey (\_k v1 v2 -> v1 <> v2) dm_d singleton_dm
  in (applyDiff singleton_dm s, new_dm_d)

kvsGet :: (Monad m, FromKV f s) => KVStateT s m s
kvsGet = KVStateT . gets $ \(s,_) -> s

runKVStateT :: Functor m => KVStateT s m a -> s -> m (s, DMap (K s) D, a)
runKVStateT (KVStateT m) s = runStateT m (s, DMap.empty) <&> \case
  (a, (s', dm_d)) -> (s', dm_d, a)
