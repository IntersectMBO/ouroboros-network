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
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
-- |

module LedgerOnDisk.V2.DumbDb where

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Diff
import LedgerOnDisk.V2.Example
import Data.IORef
import Data.Map (Map)
import Control.Concurrent.STM
import LedgerOnDisk.V2.Db
import LedgerOnDisk.V2.Query
import Control.Exception
import Data.Proxy
import Control.Monad
import Control.Lens hiding (op)
import Data.Functor
import Data.Foldable
import LedgerOnDisk.V2.Snapshot
import Data.Monoid
import Control.Monad.State.Strict
import Data.Coerce
import Test.QuickCheck
import qualified Data.Map as Map
import Data.SOP (And)

-- data RealDb = RealDb { DumbDb, Map SlotId OdmConstMap state Int }

instance
  ( AllMap KeysOrd state
  , AllMap (MapIs Semigroup DiffMap) state
  , TableTypeTag state ~ TableTag
  ) => Db (DumbDb (state :: StateKind MapFlavour)) where
  type DbState (DumbDb state) = state
  type DbSeqId (DumbDb state) = OdmConstMap state Int

  readDb DumbDb {backingStore} odmq = odmProductMapToTuple <$> zipAcOnDiskMappings (Proxy :: Proxy KeysOrd) go backingStore odmq
    where
      go :: Ord k => BackingStore (t :: MapFlavour) k v -> DbQuery t k v -> IO (ProductMap DbQueryResult (ConstMap Int) t k v)
      go bs q = do
        ((qr, _mb_sq), sq) <- readBackingStore bs $ queryBackingStore q
        pure $ ProductMap qr (ConstMap sq)

  writeDb DumbDb {backingStore} old_sq new_sq dbdiff = withBStores old_sq new_sq go backingStore where
    go bstores = flip runStateT bstores $ do
      runDbDiff (\si -> get >>= handleSnapshot si old_sq >>= put) (\d -> get >>= handleDiff d >>= put) dbdiff
    -- zip4AcOnDiskMappings (Proxy :: Proxy KeysOrd) go old_sq new_sq backingStore odmd
    -- where
    --   -- go :: Ord k => ConstMap Int t k v -> ConstMap Int t k v -> BackingStore (t :: MapFlavour) k v -> DiffMap t k v -> IO (NullMap t k v)
    --   go (ConstMap old_sq') (ConstMap new_sq') bs dm = writeBackingStore (Just old_sq') new_sq'  bs $ pure . (NullMap,) . Just . applyDiffMapToBStore dm



data BackingStore (t :: MapFlavour) k v = BackingStore
  { bsId :: !(TMVar Int)
  , bsRef :: !(IORef (BStore t k v))
  }

data BStore (t :: MapFlavour) k v where
  RWStore :: Ord k => Map k v -> BStore 'MK_RW k v
  RWUStore :: (Ord k, Semigroup v) => Map k v -> BStore 'MK_RWU k v
  ROStore :: Ord k => Int -> Map k v -> BStore 'MK_RO k v

type OdmBackingStore state = OnDiskMappings state BackingStore

newtype DumbDb (state :: StateKind MapFlavour) = DumbDb
  { backingStore :: OdmBackingStore state
  }

holdingSeqIdLock :: BackingStore t k v -> (Int ->  IO (a,  Int)) -> IO a
holdingSeqIdLock BackingStore{bsId} act = let
  in bracketOnError
  (atomically $ takeTMVar bsId)
  (atomically . putTMVar  bsId) $ \sq -> mask $ \restore -> do
    (r, sq') <- restore $ act sq
    atomically $ putTMVar bsId sq'
    pure r

holdingSeqIdLock_ :: BackingStore t k v -> (Int ->  IO a) -> IO a
holdingSeqIdLock_ bs act = holdingSeqIdLock bs $ \i -> (,i) <$> act i

data NonMonotonicSeqIdException = NonMonotonicSeqIdException
  { badId :: !Int
  , lastGoodId :: !Int
  } deriving stock (Show, Eq)
  deriving anyclass (Exception)


data BadSeqIdException = BadSeqIdException
  { was :: !Int
  , expected :: !Int
  } deriving stock (Show, Eq)
  deriving anyclass (Exception)

newBStore :: TableTag t k v -> Map k v -> BStore t k v
newBStore t m = case t of
  TableTagRO -> ROStore (-1) Map.empty
  TableTagRW -> RWStore m
  TableTagRWU  -> RWUStore m

-- class (KeysOrd t k v, Coercible (TableTag MapFlavour t k v) (MapTag t)) => TableTagCoercible t k v
-- instance (KeysOrd t k v, Coercible (TableTag MapFlavour t k v) (MapTag t)) => TableTagCoercible t k v

newDumbDb :: ( HasOnDiskMappings state, TableTypeTag state ~ TableTag, AllMap KeysOrd state)
  => OnDiskMappings state DataMap -- ^
  -> IO (DumbDb state)
newDumbDb initial_map = DumbDb <$> zipAcOnDiskMappings (Proxy :: Proxy KeysOrd) go tableTags initial_map where
  mk_backing_store bstore = do
    bsId <- newTMVarIO 0
    bsRef <- newIORef bstore
    pure BackingStore {..}

  go :: forall t k v. TableTag t k v -> DataMap t k v -> IO (BackingStore t k v)
  go t (DataMap m) = mk_backing_store (newBStore t m)

resetDumbDb :: (HasOnDiskMappings state, TableTypeTag state ~ TableTag) => OnDiskMappings state DataMap -> DumbDb state -> IO ()
resetDumbDb odm_m DumbDb{backingStore} = zip3AOnDiskMappings_  go tableTags odm_m backingStore where
  go :: TableTag t k v -> DataMap t k v -> BackingStore t k v -> IO ()
  go t (DataMap m) BackingStore{bsRef, bsId} = do
    atomically . void . tryTakeTMVar $ bsId
    writeIORef bsRef (newBStore (coerce t) m)
    atomically $ putTMVar bsId 0


writeBackingStore :: forall t k v a.
  ()
  => Maybe Int
  -> Int
  -> BackingStore t k v
  -> (BStore t k v -> IO (a, Maybe (BStore t k v)))
  -> IO a
writeBackingStore mb_old_sq new_sq bs act = holdingSeqIdLock bs $ \sq -> do
  when (new_sq <= sq) $ throwIO $ NonMonotonicSeqIdException { badId = new_sq, lastGoodId = sq }
  for_ mb_old_sq $ \old_sq -> do
    when (old_sq /= sq) $ throwIO $ BadSeqIdException { was = sq, expected = old_sq }
  let ref = bsRef bs
  readIORef ref >>= act >>= \(a, mb_bst) -> for_ mb_bst (writeIORef ref) $> (a, new_sq)

writeBackingStore_ :: Maybe Int -> Int -> BackingStore t k v -> (BStore t k v -> IO (Maybe (BStore t k v))) -> IO ()
writeBackingStore_ mb_i j bs f = writeBackingStore mb_i j bs $ fmap ((),) . f

readBackingStore :: forall  (t :: MapFlavour) k v a. ()
  => BackingStore t k v
  -> (BStore t k v -> a)
  -> IO (a, Int)
readBackingStore bs q = holdingSeqIdLock_ bs $ \sq -> readIORef (bsRef bs) <&> (,sq) . q


-- readBackingStoreInternal :: forall (t :: MapFlavour) k v. (Ord k) => IORef (BStore t k v) -> IO (Map k v)
-- readBackingStoreInternal ref = readIORef ref <&> \case
--   RWStore m -> m
--   RWUStore m -> m
--   ROStore _ m -> m

backingStoreFromBStore :: BStore t k v -> IO (BackingStore t k v)
backingStoreFromBStore bst = do
  bsRef <- newIORef bst
  bsId <- newTMVarIO 0
  pure BackingStore{..}

newBackingStoreRW :: Ord k => Map k v -> IO (BackingStore 'MK_RW k v)
newBackingStoreRW m = backingStoreFromBStore $ RWStore m

newBackingStoreRWU :: (Ord k, Semigroup v) => Map k v -> IO (BackingStore 'MK_RWU k v)
newBackingStoreRWU m = backingStoreFromBStore $ RWUStore m

newBackingStoreRO :: (Ord k, Semigroup v) => Int -> Map k v -> IO (BackingStore 'MK_RO k v)
newBackingStoreRO i m = backingStoreFromBStore $ ROStore i m

snapshotBackingStore :: forall t k v. BackingStore t k v -> IO (BStore 'MK_RO k v)
snapshotBackingStore bs = do
  (go,  sq) <- readBackingStore bs $ \case
    RWStore m -> (`ROStore` m)
    RWUStore m -> (`ROStore` m)
    ROStore s m -> const $ ROStore s m
  pure . go $ sq

data MidSwizzle t k v where
  TakeSnapshot :: BackingStore t k v -> MidSwizzle 'MK_RO k v
  Keep :: MidSwizzle t k v
  -- RelabelSnapshot :: BackingStore 'MK_RO k v -> MidSwizzle 'MK_RO k v

applySwizzle :: forall (state :: StateKind MapFlavour). HasOnDiskMappings state
  => SnapshotInstructions state
  -> OnDiskMappings state (ConstMap Int)
  -> OnDiskMappings state BStore
  -> OnDiskMappings state BStore
applySwizzle (SnapshotInstructions f) odm_ids odm_bs = zipOnDiskMappings go swizzled odm_bs
  where
    swizzled = f $ zipOnDiskMappings ProductMap odm_bs odm_ids
    go :: forall (t :: MapFlavour) k v. SnapshotOfTable (BStore `ProductMap` ConstMap Int) t k v -> BStore t k v -> BStore t k v
    go sot_bs bs0 = case sot_bs of
      KeepTable -> bs0
      SnapshotOfTable (bs `ProductMap` ConstMap i) -> case bs of
        RWStore m -> ROStore i m
        RWUStore m -> ROStore i m
        ROStore j m -> ROStore j m

applyDiffMapToBStore :: DiffMap t k v -> BStore t k v -> BStore t k v
applyDiffMapToBStore _ x@ROStore{} = x
applyDiffMapToBStore dm (RWStore m) = RWStore $ applyDiffMapToMap dm m
applyDiffMapToBStore dm (RWUStore m) = RWUStore $ applyDiffMapToMap dm m


queryBackingStore ::  DbQuery t k v -> BStore t k v -> (DbQueryResult t k v, Maybe Int)
queryBackingStore (DbQuery q) = \case
  RWStore m -> (coerce $ queryMap q m, Nothing)
  RWUStore m -> (coerce $ queryMap q m, Nothing)
  ROStore sq m -> (coerce $ queryMap q m, Just sq)

withBStores :: HasOnDiskMappings state
  => DbSeqId (DumbDb state) -> DbSeqId (DumbDb state) ->
  (OnDiskMappings state BStore -> IO (a, OnDiskMappings state BStore))
  -> OnDiskMappings state BackingStore -> IO a
withBStores  expected_seqids new_seqids op odm_bs = bracketOnError take_ids write_ids $ \old_seqids ->
  mask $ \restore -> do
    (r, new_odm_bs) <- restore $ do
      let
        go (ConstMap old_sq) (ConstMap expected_sq) (ConstMap new_sq) BackingStore{bsRef}
          | old_sq >= new_sq = throwIO NonMonotonicSeqIdException{badId = new_sq, lastGoodId = old_sq}
          | old_sq /= expected_sq = throwIO BadSeqIdException{ was = old_sq, expected = expected_sq}
          | otherwise = readIORef bsRef
      odm_bstores <- zip4AOnDiskMappings go old_seqids expected_seqids new_seqids odm_bs
      op odm_bstores
    let
      go new_bstore BackingStore{bsRef} = writeIORef bsRef new_bstore
    zipAOnDiskMappings_ go new_odm_bs odm_bs
    void $ write_ids new_seqids
    pure r
  where
    take_ids = atomically $ traverseOnDiskMappings
      (\BackingStore {bsId} -> ConstMap <$> takeTMVar bsId)
      odm_bs
    write_ids new_ids = atomically $ zipAOnDiskMappings
      (\(ConstMap new_id) BackingStore {bsId} -> putTMVar bsId new_id $> NullMap)  new_ids odm_bs

handleSnapshot :: (HasOnDiskMappings state, Applicative m) => SnapshotInstructions state -> OnDiskMappings state (ConstMap Int) ->  OnDiskMappings state BStore -> m (OnDiskMappings state BStore)
handleSnapshot si seqids bstores =  pure $ applySwizzle si seqids bstores

handleDiff :: (HasOnDiskMappings state, Applicative m) => OnDiskMappings state DiffMap -> OnDiskMappings state BStore -> m (OnDiskMappings state BStore)
handleDiff diffs bstores = pure $ zipOnDiskMappings applyDiffMapToBStore diffs bstores

prop_dumb_db_reconcile_read :: (Arbitrary (OdmQuery ExampleState)) => OnDiskMappings ExampleState DataMap -> Blind (DbDiff ExampleState) -> Blind (DbDiff ExampleState) -> OdmQuery ExampleState -> Property
prop_dumb_db_reconcile_read initial_map (Blind d1) (Blind d2) q = ioProperty $ do
  ddb <- newDumbDb initial_map
  pure $ prop_reconcile_read ddb (resetDumbDb initial_map)
    (pureOnDiskMappings $ ConstMap 0)
    (pureOnDiskMappings $ ConstMap 1)
    (pureOnDiskMappings $ ConstMap 2)
    d1 d2 q


{-


"hello"
"hello"
"hello"
No instance for (Arbitrary (SnapshotInstructions ExampleState))
  arising from a use of ‘propEvaluation’
-}
