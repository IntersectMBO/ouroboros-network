{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore (tests) where

import           Control.Monad.Class.MonadThrow
import           Control.Monad.Except hiding (lift)
import           Control.Monad.IOSim
import           Control.Monad.Reader
import           Control.Tracer (nullTracer)
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import           Data.Typeable
import qualified System.Directory as Dir
import           System.IO.Temp (createTempDirectory)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))

import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Property, Testable)
import           Test.QuickCheck.Gen.Unsafe
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic (PropertyM)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.FS.API hiding (Handle)
import           Ouroboros.Consensus.Storage.FS.API.Types hiding (Handle)
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.DiffSeq as DS
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB
import           Ouroboros.Consensus.Storage.LedgerDB.OnDisk
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..))

import qualified Test.Util.FS.Sim.MockFS as MockFS
import           Test.Util.FS.Sim.STM
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.Slotting.Arbitrary ()
import           Test.Util.Orphans.ToExpr ()
import           Test.Util.TestLedgerState

import           Test.QuickCheck.StateModel as StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Run as Lockstep

import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Lockstep
import qualified Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Registry

{-------------------------------------------------------------------------------
  Main test tree
-------------------------------------------------------------------------------}

-- TODO: use scaling instead of withMaxSuccess
tests :: TestTree
tests = testGroup "BackingStore" [
    testProperty "InMemory IOSim SimHasFS" $ QC.withMaxSuccess 1000
      testWithIOSim
  , testProperty "InMemory IO SimHasFS" $ QC.withMaxSuccess 1000 $
      testWithIO $
        setupBSEnv InMemoryBackingStore setupSimHasFS (pure ())
  , testProperty "InMemory IO IOHasFS" $ QC.withMaxSuccess 1000 $
      testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv InMemoryBackingStore (setupIOHasFS fp) cleanup
  , testProperty "LMDB IO IOHasFS" $ QC.withMaxSuccess 200 $
      testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv (LMDBBackingStore testLMDBLimits) (setupIOHasFS fp) cleanup
  , testCase "labelledExamples" labelledExamples
  ]

testLMDBLimits :: LMDB.LMDBLimits
testLMDBLimits = LMDB.LMDBLimits
  { -- 100 MiB should be more than sufficient for the tests we're running here.
    -- If the database were to grow beyond 100 Mebibytes, resulting in a test
    -- error, then something in the LMDB backing store or tests has changed and
    -- we should reconsider this value.
    LMDB.lmdbMapSize = 100 * 1024 * 1024
    -- 3 internal databases: 1 for the settings, 1 for the state, and 1 for the
    -- ledger tables.
  , LMDB.lmdbMaxDatabases = 3

  , LMDB.lmdbMaxReaders = maxOpenValueHandles
  }

testWithIOSim :: Actions (Lockstep (BackingStoreState K V D)) -> Property
testWithIOSim acts = monadicSim $ do
  BSEnv {bsSomeHasFS, bsBackingStore, bsCleanup, bsRegistry} <-
    QC.run (setupBSEnv InMemoryBackingStore setupSimHasFS (pure ()))
  void $
    runPropertyIOLikeMonad $
      runPropertyReaderT (StateModel.runActions acts) (bsSomeHasFS, bsBackingStore, bsRegistry)
  QC.run bsCleanup
  pure True

testWithIO::
     IO (BSEnv IO K V D)
  -> Actions (Lockstep T) -> Property
testWithIO mkBSEnv = runActionsBracket pT mkBSEnv bsCleanup runner

runner ::
     RealMonad m ks vs d a
  -> BSEnv m ks vs d
  -> m a
runner c r = unIOLikeMonad . runReaderT c $
  (bsSomeHasFS r, bsBackingStore r, bsRegistry r)

-- | Generate minimal examples for each label.
labelledExamples :: IO ()
labelledExamples = do
  -- TODO: remove
  threadDelay 1
  QC.labelledExamples $ tagActions pT

{-------------------------------------------------------------------------------
  Resources
-------------------------------------------------------------------------------}

data BSEnv m ks vs d = BSEnv {
    bsSomeHasFS    :: SomeHasFS m
  , bsBackingStore :: BS.BackingStore m ks vs d
  , bsRegistry     :: HandleRegistry m (BS.BackingStoreValueHandle m ks vs)
  , bsCleanup      :: m ()
  }

-- | Set up a simulated @'HasFS'@.
setupSimHasFS :: IOLike m => m (SomeHasFS m)
setupSimHasFS = SomeHasFS . simHasFS <$> newTVarIO MockFS.empty

-- | Set up a @'HasFS'@ for @'IO'@.
setupIOHasFS :: MonadIO m => FilePath -> m (SomeHasFS m)
setupIOHasFS = pure . SomeHasFS . ioHasFS . MountPoint

-- | In case we are running tests in @'IO'@, we must do some temporary directory
-- management.
setupTempDir :: MonadIO m => m (FilePath, m ())
setupTempDir = do
  sysTmpDir <- liftIO Dir.getTemporaryDirectory
  qsmTmpDir <- liftIO $ createTempDirectory sysTmpDir "BS_QSM"
  pure (qsmTmpDir, liftIO $ Dir.removeDirectoryRecursive qsmTmpDir)

setupBSEnv ::
     IOLike m
  => BackingStoreSelector m
  -> m (SomeHasFS m)
  -> m ()
  -> m (BSEnv m K V D)
setupBSEnv bss mkSfhs cleanup = do
  bsSomeHasFS@(SomeHasFS hfs) <- mkSfhs

  createDirectory hfs (mkFsPath ["copies"])

  LedgerBackingStore bsBackingStore <-
    newBackingStore
      nullTracer
      bss
      bsSomeHasFS
      polyEmptyLedgerTables

  bsRegistry <- initHandleRegistry

  let
    bsCleanup = do
      catches (BS.bsClose bsBackingStore) closeHandlers
      cleanup

  pure BSEnv {bsSomeHasFS, bsBackingStore, bsCleanup, bsRegistry}

-- | A backing store will throw an error on close if it has already been closed,
-- which we ignore if we are performing a close as part of resource cleanup.
closeHandlers :: IOLike m => [Handler m ()]
closeHandlers = [
    Handler $ \case
      BS.TVarBackingStoreClosedExn -> pure ()
      e                            -> throwIO e
  , Handler $ \case
      LMDB.DbErrClosed -> pure ()
      e                -> throwIO e
  ]

{-------------------------------------------------------------------------------
  Types under test
-------------------------------------------------------------------------------}

type T = BackingStoreState K V D

pT :: Proxy T
pT = Proxy

type K = LedgerTables (SimpleLedgerState (Fixed Word) (Fixed Word)) KeysMK
type V = LedgerTables (SimpleLedgerState (Fixed Word) (Fixed Word)) ValuesMK
type D = LedgerTables (SimpleLedgerState (Fixed Word) (Fixed Word)) DiffMK

{-------------------------------------------------------------------------------
  @'HasOps'@ instances
-------------------------------------------------------------------------------}

instance Mock.EmptyValues V where
  emptyValues = polyEmptyLedgerTables

instance Mock.ApplyDiff V D where
  applyDiff = zipLedgerTables rawApplyDiffs

instance Mock.LookupKeysRange K V where
  lookupKeysRange = \prev n vs ->
      case prev of
        Nothing ->
          mapLedgerTables (rangeRead n) vs
        Just ks ->
          zipLedgerTables (rangeRead' n) ks vs
    where
      rangeRead :: Int -> ValuesMK k v -> ValuesMK k v
      rangeRead n (ApplyValuesMK (DS.Values vs)) =
        ApplyValuesMK $ DS.Values $ Map.take n vs

      rangeRead' ::
          Ord k
        => Int
        -> KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      rangeRead' n ksmk vsmk =
          case Set.lookupMax ks of
            Nothing -> ApplyValuesMK $ DS.Values Map.empty
            Just  k -> ApplyValuesMK $ DS.Values $
              Map.take n $ snd $ Map.split k vs
        where
          ApplyKeysMK (DS.Keys ks)     = ksmk
          ApplyValuesMK (DS.Values vs) = vsmk

instance Mock.LookupKeys K V where
  lookupKeys = zipLedgerTables readKeys
    where
      readKeys ::
          Ord k
        => KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      readKeys (ApplyKeysMK ks) (ApplyValuesMK vs) =
        ApplyValuesMK $ DS.restrictValues vs ks

instance Mock.ValuesLength V where
  valuesLength (SimpleLedgerTables (ApplyValuesMK (DS.Values m))) =
    Map.size m

instance Mock.MakeDiff V D where
  diff t1 t2 = zipLedgerTables (rawForgetValues .: rawCalculateDifference) t1 t2

instance Mock.DiffSize D where
  diffSize (SimpleLedgerTables (ApplyDiffMK (Diff.Diff m))) = Map.size m

instance Mock.KeysSize K where
  keysSize (SimpleLedgerTables (ApplyKeysMK (Diff.Keys s))) = Set.size s

instance Mock.HasOps K V D

{-------------------------------------------------------------------------------
  Utilities

  TODO: should become available in a newer version of @quickcheck-dynamic@.
-------------------------------------------------------------------------------}

-- | Copied from the @Test.QuickCheck.Extras@ module in the @quickcheck-dynamic@
-- package.
runPropertyReaderT ::
     Monad m
  => PropertyM (ReaderT e m) a
  -> e
  -> PropertyM m a
runPropertyReaderT p e = QC.MkPropertyM $ \k -> do
  m <- QC.unPropertyM p $ fmap lift . k
  return $ runReaderT m e

runPropertyIOLikeMonad ::
     IOLikeMonadC m
  => PropertyM (IOLikeMonad m) a
  -> PropertyM m a
runPropertyIOLikeMonad p = QC.MkPropertyM $ \k -> do
  m <- QC.unPropertyM p $ fmap ioLikeMonad . k
  return $ unIOLikeMonad m

-- | Copied from @Ouroboros.Network.Testing.QuickCheck@.
runSimGen :: (forall s. QC.Gen (IOSim s a)) -> QC.Gen a
runSimGen f = do
    Capture eval <- capture
    return $ runSimOrThrow (eval f)

-- | Copied from @Ouroboros.Network.Testing.QuickCheck@.
monadicSim :: Testable a => (forall s. PropertyM (IOSim s) a) -> Property
monadicSim m = QC.property (runSimGen (QC.monadic' m))

{-------------------------------------------------------------------------------
  Orphan Arbitrary instances
-------------------------------------------------------------------------------}

deriving newtype instance QC.Arbitrary (ApplyMapKind' mk k v)
                       => QC.Arbitrary (
                            LedgerTables
                              (SimpleLedgerState k v)
                              (ApplyMapKind' mk)
                            )

instance (Ord k, QC.Arbitrary k)
      => QC.Arbitrary (KeysMK k v) where
  arbitrary = ApplyKeysMK <$> QC.arbitrary
  shrink (ApplyKeysMK ks) = ApplyKeysMK <$> QC.shrink ks

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (DiffMK k v) where
  arbitrary = ApplyDiffMK <$> QC.arbitrary
  shrink (ApplyDiffMK d)= ApplyDiffMK <$> QC.shrink d

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (ValuesMK k v) where
  arbitrary = ApplyValuesMK <$> QC.arbitrary
  shrink (ApplyValuesMK d)= ApplyValuesMK <$> QC.shrink d

deriving newtype instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
                       => QC.Arbitrary (DS.Values k v)

deriving newtype instance (Ord k, QC.Arbitrary k)
                       => QC.Arbitrary (DS.Keys k v)

deriving newtype instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
                       => QC.Arbitrary (DS.Diff k v)

instance QC.Arbitrary v => QC.Arbitrary (DS.NEDiffHistory v) where
  arbitrary = DS.NEDiffHistory <$> ((:<||) <$> QC.arbitrary <*> QC.arbitrary)
  shrink (DS.NEDiffHistory h) =
    fmap DS.NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ QC.shrink (NESeq.toSeq h)

instance QC.Arbitrary v => QC.Arbitrary (DS.DiffEntry v) where
  arbitrary = do
    constr <- QC.elements [
        DS.Insert
      , DS.Delete
      , DS.UnsafeAntiInsert
      , DS.UnsafeAntiDelete
      ]
    constr <$> QC.arbitrary

instance QC.Arbitrary ks => QC.Arbitrary (BS.RangeQuery ks) where
  arbitrary = BS.RangeQuery <$> QC.arbitrary <*> QC.arbitrary
  shrink (BS.RangeQuery x y) = BS.RangeQuery <$> QC.shrink x <*> QC.shrink y

newtype Fixed a = Fixed a
  deriving newtype (Show, Eq, Ord)
  deriving newtype (NoThunks, ToCBOR, FromCBOR)

deriving via QC.Fixed a instance QC.Arbitrary a => QC.Arbitrary (Fixed a)
