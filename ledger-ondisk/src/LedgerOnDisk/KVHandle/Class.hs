{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module LedgerOnDisk.KVHandle.Class where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans
import Data.Coerce
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Kind
import LedgerOnDisk.Class
import Data.HashSet (HashSet)
import Control.Monad.State.Strict
import GHC.Generics hiding (Diff)
import Control.Monad.Writer
import Control.Monad.Catch
import Control.Monad.Except
import Data.Functor.Const
import Control.Lens
-- Questions for reviewers:
-- Are these interfaces reasonable?
-- Is the DMappend constructor reasonable?
-- Any better name Ideas?
-- Any better formulations of constructFromDB?

-- TODO review ACID state API

class (Monoid (KVDiff a), Eq (KVKey a), Hashable (KVKey a)) => KVable a where
  type KVKey a :: Type
  type KVDiff a :: Type
  type KVFromDB a :: Type

  applyDiff :: KVDiff a -> a -> a

  -- This is formulated as a lens.
  constructFromDB :: Lens' a (KVFromDB a)
  -- isomorphism between a and (KVfromDB a, b)

instance (Eq k, Hashable k) => KVable (HashMap k v) where
  type KVKey (HashMap k v) = k
  type KVDiff (HashMap k v) = HashMap k (Diff v)
  type KVFromDB (HashMap k v) = (HashMap k v)

  applyDiff = applyDtoHashMap
  constructFromDB = id

instance KVable a => KVable (a, b) where
  type KVKey (a, b) = KVKey a
  type KVDiff (a, b) = KVDiff a
  type KVFromDB (a, b) = KVFromDB a

  applyDiff d (a, b) = (applyDiff d a, b)
  constructFromDB f (a, b) = (,b) <$> constructFromDB f a

class KVable kv => KVHandle kv t | t -> kv where
  type KVMonad t :: (Type -> Type) -> Constraint
  type KVMonad t = MonadIO
  type KVReadSet t :: Type
  type KVErr t :: Type
  type KVErr t = ()

  prepare :: KVMonad t m => t -> QueryScope (KVKey kv) -> m (KVReadSet t)
  submit :: KVMonad t m => t -> KVReadSet t -> (KVFromDB kv -> (KVDiff kv, a)) -> m (Either (KVErr t) a)

newtype KVMT kv m a = KVMT { unKVM :: StateT (kv, KVDiff kv) m a  }
  deriving stock (Generic)
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadWriter w, MonadThrow, MonadCatch, MonadMask, MonadIO, MonadTrans, MonadError e)

instance MonadState s m => MonadState s (KVMT kv m) where
  get = KVMT $ lift get
  put = KVMT . lift . put

kvGet :: Monad m => KVMT kv m kv
kvGet = KVMT $ gets fst

kvTell :: (KVable kv, Monad m) => KVDiff kv -> KVMT kv m ()
kvTell d1 =
  -- TODO is forcing d0 <> d1 the right idea?
  KVMT $ modify' $ \(kv, d0) -> case d0 <> d1 of
    d -> (applyDiff d1 kv, d)


-- TODO property test that the result sati
runKVMT :: (Monad m, KVable kv) => KVMT kv m a -> kv -> m (kv, KVDiff kv, a)
runKVMT (KVMT m) kv0 = do
  (a, (kv, diff)) <- runStateT m (kv0, mempty)
  pure (kv, diff, a)

newtype KVableT t m a = KVableT {unKVableT :: ReaderT t m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

askT :: Monad m => KVableT t m t
askT = KVableT $ ask

instance ( KVMonad t (KVableT t m)
         , Monad m
         , KVHandle kv t
         , KVErr t ~ BaseError
         , HashMap k v ~ KVFromDB kv
         , k ~ KVKey kv
         , HashMap k (Diff v) ~ KVDiff kv , Eq k
         , Hashable k
         ) => MonadKV k v (KVableT t m) where
  type Err (KVableT t m) = KVErr t
  newtype ReadSet (KVableT t m) = KVableTReadSet {unKVableTReadSet :: KVReadSet t}
  prepareOperation qs = do
    t <- askT
    coerce <$> prepare t qs
  submitOperation (KVableTReadSet rs) op = do
    t <- askT
    submit t rs $ \from_db -> op (fmap pure from_db)
