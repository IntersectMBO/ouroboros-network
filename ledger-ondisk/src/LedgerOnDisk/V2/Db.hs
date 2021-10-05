{-# language DataKinds #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module LedgerOnDisk.V2.Db where

import Data.Kind

-- import Control.Monad.Class.MonadAsync
-- import Control.Monad.Class.MonadSTM

import LedgerOnDisk.V2.OnDiskMappings
import LedgerOnDisk.V2.Query
import LedgerOnDisk.V2.Diff
import LedgerOnDisk.V2.Snapshot
import Data.Monoid
import Control.Monad.Trans.Writer.CPS
import Data.Functor
import Control.Monad
import Control.Monad.Trans
import qualified Control.Foldl as Foldl
import Data.Functor.Identity
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.Either
import Data.Coerce

newtype DbQuery t k v = DbQuery (Query k v)
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

deriving newtype instance (Ord k, Arbitrary k) => Arbitrary (DbQuery t k v)

newtype DbQueryResult t k v = DbQueryResult (QueryResult k v)
  deriving stock (Eq, Show)

type OdmConstMap state a = OnDiskMappings state (ConstMap a)
type OdmQuery state = OnDiskMappings state DbQuery
type OdmPtMap state = OnDiskMappings state PTMap
type OdmDiffMap state = OnDiskMappings state DiffMap
type OdmQueryResult state = OnDiskMappings state DbQueryResult

newtype DbDiff state = DbDiff (Endo [Either (SnapshotInstructions state) (OdmDiffMap state)])
  deriving newtype (Semigroup, Monoid)

instance
  ( AllMap (MapIs Arbitrary DiffMap) state
  , Arbitrary (SnapshotInstructions state)
  ) => Arbitrary (DbDiff state) where
  arbitrary = do
    l <- arbitrary
    pure . DbDiff . Endo $ \s -> l <> s
  shrink = fmap (\x -> DbDiff $ Endo (x <>)) . shrink . flip appEndo [] . coerce

-- newtype DbTableTag t k v = DbTableTag (MapTag t)

class
  ( HasOnDiskMappings (DbState dbhandle)
  , TableTypeTag (DbState dbhandle) ~ TableTag
  )=> Db dbhandle where

  type DbState dbhandle :: StateKind MapFlavour
  type DbSeqId dbhandle :: Type


  readDb :: dbhandle
    -> OdmQuery (DbState dbhandle)
    -> IO (OdmQueryResult (DbState dbhandle), DbSeqId dbhandle)

  writeDb :: dbhandle
    -> DbSeqId dbhandle
    -> DbSeqId dbhandle
    -> DbDiff (DbState dbhandle) -> IO ()

runDbDiff :: forall a m state. (Monoid a, Monad m, HasOnDiskMappings  state, AllMap (MapIs Semigroup DiffMap) state)
  => (SnapshotInstructions state -> m a)
  -> (OdmDiffMap state -> m a)
  -> DbDiff state -> m a
runDbDiff run_snapshot run_diff (DbDiff (flip appEndo [] -> ds0))
  | null ds0 = pure mempty
  | d : ds <- ds0 = do
      let
        run_either e = either run_snapshot run_diff e
        go prev next = case (prev, next) of
          (Left si1, Left si2) -> pure . Left $ si1 <> si2
          (Right d1, Right d2) -> pure . Right $ d1 <> d2
          (l, r) -> (lift (run_either l) >>= tell) $> r
      (last_one, r1) <- runWriterT $ foldM go d ds
      r2 <- run_either last_one
      pure $ r1 <> r2


data ReconciliationError
  = StaleQuery
  | NonRORangeQuery
  | NonROCountRows
  deriving stock (Eq, Show)


{-

The following are equivalent, up to ReconciliationErrors.
This means that if all reconcileReads return Rights then they are equal.

If r1 is Right, so are r2 and r3
If r2 is Right, so is r3

do
  (qr, _) <- readDb h q
  writeDb s0 s1 h diff1
  writeDb s1 s2 h diff2
  r1 <- reconcileRead (diff1 <> diff2) qr

  ===

do
  writeDb s0 s1 h diff1
  (qr, _) <- readDb h q
  writeDb s1 s2 h diff2
  r2 <- reconcileRead (diff2) qr

  ===

do
  writeDb s0 s1 h diff1
  writeDb s1 s2 h diff2
  (qr, _) <- readDb h q
  pure qr

-}

reconcileRead :: forall state. (HasOnDiskMappings state, AllMap (MapIs Semigroup DiffMap) state)
  => OdmQuery state -- ^ The Query submitted to readDb
  -> DbDiff state -- ^ The DbDiffs that have been written with seqIds > the seqId returned with the query result
  -> OdmQueryResult state -- ^ The QueryResult returned from  readDb
  -> Either ReconciliationError (OdmQueryResult state)
reconcileRead odm_q diff odmqr0 = flip Foldl.appEndoM odmqr0 . runIdentity $ runDbDiff go_si go_dm diff where
  go_si :: SnapshotInstructions  state -> Identity (Foldl.EndoM (Either ReconciliationError) (OdmQueryResult state))
  go_si si = Identity . Foldl.EndoM $ \odmqr -> pure $ runSnapshotInstructions go_ro go_other si
    (zip3OnDiskMappings (\a b c -> a `ProductMap` b `ProductMap` c) tableTags odm_q odmqr) where
    go_ro (tag `ProductMap` q `ProductMap` DbQueryResult qr) = DbQueryResult qr
    go_other (_ `ProductMap` _ `ProductMap` qr)= qr

  go_dm odm_dm = Identity . Foldl.EndoM $ \odmqr -> zip3AOnDiskMappings go odm_q odm_dm odmqr where
    go (DbQuery q') dm (DbQueryResult qr@QueryResult{qrPointQueries}) = pure . DbQueryResult $ qr
      { qrPointQueries = applyDiffMapToMap (restrictDiffMap (qPointQueries q') dm) qrPointQueries }


prop_reconcile_read :: (Db handle, AllMap (MapIs Semigroup DiffMap) (DbState handle), Eq (OdmQueryResult (DbState handle)), Show (OdmQueryResult (DbState handle)))
  => handle
  -> (handle -> IO ())
  -> DbSeqId handle -> DbSeqId handle -> DbSeqId handle
  -> DbDiff (DbState handle) -> DbDiff (DbState handle)
  -> OdmQuery (DbState handle)
  -> Property
prop_reconcile_read h reset_handle s0 s1 s2 d1 d2 q = monadicIO $ do
  liftIO $ reset_handle h
  r1 <- liftIO $ do
    (qr, _) <- readDb h q
    writeDb h s0 s1 d1
    writeDb h s1 s2 d2
    pure $ reconcileRead q (d1 <> d2) qr

  liftIO $ reset_handle h
  r2 <- liftIO $ do
    writeDb h s0 s1 d1
    (qr, _) <- readDb h q
    writeDb h s1 s2 d2
    pure $ reconcileRead q d2 qr

  liftIO $ reset_handle h
  r3 <- liftIO $ do
    writeDb h s0 s1 d1
    writeDb h s1 s2 d2
    (qr, _) <- readDb h q
    pure (Right qr)

  monitor $ \p -> p .&&. conjoin ([ r1 === r2 | isRight r1 ] <> [ r2 === r3 | isRight r2 ])
