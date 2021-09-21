{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
-- |

module LedgerOnDisk.V2.DumbDb where

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Diff
import Data.IORef
import Data.Map (Map)
import Control.Concurrent.STM
import LedgerOnDisk.V2.Db
import LedgerOnDisk.V2.Query
import Control.Exception
import Data.Proxy
import Control.Monad
import Data.Kind
import Control.Lens
import Data.Functor
import Data.Foldable



instance (AllMap KeysOrd state) => Db (DumbDb (state :: StateKind MapFlavour)) where
  type DbState (DumbDb state) = state
  type DbSeqId (DumbDb state) = OdmConstMap state Int

  readDb DumbDb {backingStore} odmq = odmProductMapToTuple <$> zipAcOnDiskMappings (Proxy :: Proxy KeysOrd) go backingStore odmq
    where
      go :: Ord k => BackingStore (t :: MapFlavour) k v -> Query t k v -> IO (ProductMap QueryResult (ConstMap Int) t k v)
      go bs q = do
        (r, sq) <- readBackingStore bs q
        pure $ ProductMap r (ConstMap sq)

  writeDb DumbDb {backingStore} old_sq new_sq odmd = void $ zip4AcOnDiskMappings (Proxy :: Proxy KeysOrd) go old_sq new_sq backingStore odmd
    where
      go :: Ord k => ConstMap Int t k v -> ConstMap Int t k v -> BackingStore (t :: MapFlavour) k v -> DiffMap t k v -> IO (NullMap t k v)
      go (ConstMap old_sq') (ConstMap new_sq') bs dm = writeBackingStore (Just old_sq') new_sq'  bs dm $> NullMap

data BackingStore (t :: MapFlavour) k v where
  BackingStoreRW :: Ord k =>
    { bsRwRef :: !(IORef (Map k v))
    , bsRwId :: !(TMVar Int)
    } -> BackingStore 'MK_RW k v
  BackingStoreRWU :: (Ord k, Semigroup v) =>
    { bsRwuRef :: !(IORef (Map k v))
    , bsRwuId :: !(TMVar Int)
    } -> BackingStore 'MK_RWU k v

data BackingStoreSnapshot (t :: MapFlavour) k v where
  BackingStoreSnapshot :: Map k v -> BackingStoreSnapshot 'MK_RO k v

type OdmBackingStore state = OnDiskMappings state BackingStore

data DumbDb (state :: StateKind MapFlavour) = DumbDb
  { backingStore :: OdmBackingStore state
  }

-- class KvDb dbhandle (t :: MapFlavour) k v where
--   type KvSeqId dbhandle t k v :: Type

--   readKvDb :: dbhandle -> Query t k v -> IO (QueryResult t k v, KvSeqId dbhandle t k v)
--   writeKvdb :: dbhandle -> KvSeqId dbhandle t k v -> KvSeqId dbhandle t k v -> DiffMap t k v -> IO (KvSeqId dbhandle t k v )


bsRef :: BackingStore t k v -> IORef (Map k v)
bsRef = \case
  BackingStoreRW{bsRwRef} -> bsRwRef
  BackingStoreRWU{bsRwuRef} -> bsRwuRef

bsId ::  BackingStore t k v -> TMVar Int
bsId = \case
  BackingStoreRW{bsRwId} -> bsRwId
  BackingStoreRWU{bsRwuId} -> bsRwuId



holdingSeqIdLock :: BackingStore t k v -> (Int ->  IO (a,  Int)) -> IO a
holdingSeqIdLock bs act = let
  i_tmv = bsId bs
  in bracketOnError
  (atomically $ takeTMVar i_tmv)
  (atomically . putTMVar  i_tmv) $ \sq -> mask $ \restore -> do
    (r, sq') <- restore $ act sq
    atomically $ putTMVar i_tmv sq'
    pure r



data BadSeqIdException = BadSeqIdException
  { was :: Int
  , expected :: Int
  } deriving stock (Show, Eq)
  deriving anyclass (Exception)


writeBackingStore :: forall  t k v.
  Ord k
  => Maybe Int
  -> Int
  -> BackingStore t k v
  -> DiffMap t k v
  -> IO ()
writeBackingStore mb_old_sq new_sq bs dm = holdingSeqIdLock bs $ \sq -> do
  for_ mb_old_sq $ \old_sq -> when (old_sq /= sq) $ throwIO $ BadSeqIdException { was = sq, expected = old_sq }
  atomicModifyIORef' (bsRef bs) ((,((), new_sq)) . applyDiffMapToMap dm)

readBackingStore :: forall  (t :: MapFlavour) k v. Ord k
  => BackingStore t k v
  -> Query t k v
  -> IO (QueryResult t k v, Int)
readBackingStore bs q = holdingSeqIdLock bs $ \sq -> (,sq) . (,sq) . queryMap q <$> readIORef (bsRef bs)

newBackingStoreRW :: Ord k => Map k v -> IO (BackingStore 'MK_RW k v)
newBackingStoreRW m = do
  bsRwRef <- newIORef m
  bsRwId <- newTMVarIO 0
  pure BackingStoreRW{..}

newBackingStoreRWU :: (Ord k, Semigroup v) => Map k v -> IO (BackingStore 'MK_RWU k v)
newBackingStoreRWU m = do
  bsRwuRef <- newIORef m
  bsRwuId <- newTMVarIO 0
  pure BackingStoreRWU{..}

snapshotBackingStore :: BackingStore t k v -> IO (BackingStoreSnapshot 'MK_RO k v, Int)
snapshotBackingStore bs = holdingSeqIdLock bs $ \i -> do
  m <- readIORef (bsRef bs)
  pure ((BackingStoreSnapshot m, i), i)
