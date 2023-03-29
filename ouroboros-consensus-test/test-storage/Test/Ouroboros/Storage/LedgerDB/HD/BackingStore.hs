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

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Test.Ouroboros.Storage.LedgerDB.HD.BackingStore (
    labelledExamples
  , tests
  ) where

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Except hiding (lift)
import           Control.Monad.IOSim
import           Control.Monad.Reader
import qualified Data.Map.Diff.Strict as Diff
import qualified Data.Map.Diff.Strict.Internal as Diff
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Sequence.NonEmpty (NESeq (..))
import qualified Data.Sequence.NonEmpty as NESeq
import qualified Data.Set as Set
import           Data.Typeable
import           Ouroboros.Consensus.Ledger.Tables
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.InMemory as BS
import qualified Ouroboros.Consensus.Storage.LedgerDB.BackingStore.LMDB as LMDB
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.IOLike hiding (MonadMask (..))
import qualified System.Directory as Dir
import           System.FS.API hiding (Handle)
import           System.FS.API.Types hiding (Handle)
import           System.FS.IO (ioHasFS)
import qualified System.FS.Sim.MockFS as MockFS
import           System.FS.Sim.STM
import           System.IO.Temp (createTempDirectory)
import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Lockstep
import qualified Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Mock as Mock
import           Test.Ouroboros.Storage.LedgerDB.HD.BackingStore.Registry
import qualified Test.QuickCheck as QC
import           Test.QuickCheck (Arbitrary (..), Property, Testable)
import           Test.QuickCheck.Gen.Unsafe
import qualified Test.QuickCheck.Monadic as QC
import           Test.QuickCheck.Monadic (PropertyM)
import           Test.QuickCheck.StateModel as StateModel
import           Test.QuickCheck.StateModel.Lockstep as Lockstep
import           Test.QuickCheck.StateModel.Lockstep.Run as Lockstep
import           Test.Tasty
import           Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import           Test.Util.LedgerStateOnlyTables
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.ToExpr ()

{-------------------------------------------------------------------------------
  Main test tree
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "BackingStore" [
    adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IOSim SimHasFS" testWithIOSim
  , adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO SimHasFS" $ testWithIO $
        setupBSEnv InMemoryBackingStore setupSimHasFS (pure ())
  , adjustOption (scaleQuickCheckTests 10) $
      testProperty "InMemory IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv InMemoryBackingStore (setupIOHasFS fp) cleanup
  , adjustOption (scaleQuickCheckTests 2) $
      testProperty "LMDB IO IOHasFS" $ testWithIO $ do
        (fp, cleanup) <- setupTempDir
        setupBSEnv (LMDBBackingStore testLMDBLimits) (setupIOHasFS fp) cleanup
  ]

scaleQuickCheckTests :: Int -> QuickCheckTests -> QuickCheckTests
scaleQuickCheckTests c (QuickCheckTests n) = QuickCheckTests $ c * n

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
  BSEnv {bsRealEnv, bsCleanup} <-
    QC.run (setupBSEnv InMemoryBackingStore setupSimHasFS (pure ()))
  void $
    runPropertyIOLikeMonad $
      runPropertyReaderT (StateModel.runActions acts) bsRealEnv
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
runner c r = unIOLikeMonad . runReaderT c $ bsRealEnv r

-- | Generate minimal examples for each label.
labelledExamples :: IO ()
labelledExamples = do
  -- TODO: the thread delay ensures that we do not start printing labelled
  -- exampes throughout other test output, but it is not a very nice solution.
  -- We should find a better alternative.
  threadDelay 1
  QC.labelledExamples $ tagActions pT

{-------------------------------------------------------------------------------
  Resources
-------------------------------------------------------------------------------}

data BSEnv m ks vs d = BSEnv {
    bsRealEnv :: RealEnv m ks vs d
  , bsCleanup :: m ()
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
  sfhs@(SomeHasFS hfs) <- mkSfhs

  createDirectory hfs (mkFsPath ["copies"])

  let bsi = newBackingStoreInitialiser mempty bss

  bsVar <- newMVar =<< bsi sfhs (BS.InitFromValues Origin emptyLedgerTables)

  rr <- initHandleRegistry

  let
    bsCleanup = do
      bs <- readMVar bsVar
      catches (BS.bsClose bs) closeHandlers
      cleanup

  pure BSEnv {
      bsRealEnv = RealEnv {
          reSomeHasFS = sfhs
        , reBackingStoreInit = bsi
        , reBackingStore = bsVar
        , reRegistry = rr
        }
    , bsCleanup
    }

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

type K = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) KeysMK
type V = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) ValuesMK
type D = LedgerTables (OTLedgerState (Fixed Word) (Fixed Word)) DiffMK

{-------------------------------------------------------------------------------
  @'HasOps'@ instances
-------------------------------------------------------------------------------}

instance Mock.EmptyValues V where
  emptyValues = emptyLedgerTables

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
      rangeRead n (ValuesMK vs) =
        ValuesMK $ Map.take n vs

      rangeRead' ::
          Ord k
        => Int
        -> KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      rangeRead' n ksmk vsmk =
          case Set.lookupMax ks of
            Nothing -> ValuesMK Map.empty
            Just  k -> ValuesMK $
              Map.take n $ snd $ Map.split k vs
        where
          KeysMK ks   = ksmk
          ValuesMK vs = vsmk

instance Mock.LookupKeys K V where
  lookupKeys = zipLedgerTables readKeys
    where
      readKeys ::
          Ord k
        => KeysMK k v
        -> ValuesMK k v
        -> ValuesMK k v
      readKeys (KeysMK ks) (ValuesMK vs) =
        ValuesMK $ Map.restrictKeys vs ks

instance Mock.ValuesLength V where
  valuesLength (OTLedgerTables (ValuesMK m)) =
    Map.size m

instance Mock.MakeDiff V D where
  diff t1 t2 = zipLedgerTables (rawForgetValues .: rawCalculateDifference) t1 t2

instance Mock.DiffSize D where
  diffSize (OTLedgerTables (DiffMK (Diff.Diff m))) = Map.size m

instance Mock.KeysSize K where
  keysSize (OTLedgerTables (KeysMK s)) = Set.size s

instance Mock.HasOps K V D

{-------------------------------------------------------------------------------
  Utilities

  TODO: these definitions are duplicated code, and they should become available
  in @quickcheck-dynamic-0.3.0@. We should remove these duplicates once that
  version is released.
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

deriving newtype instance QC.Arbitrary (mk k v)
                       => QC.Arbitrary (
                            LedgerTables
                              (OTLedgerState k v)
                              mk
                            )

instance (Ord k, QC.Arbitrary k)
      => QC.Arbitrary (KeysMK k v) where
  arbitrary = KeysMK <$> QC.arbitrary
  shrink (KeysMK ks) = KeysMK <$> QC.shrink ks

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (DiffMK k v) where
  arbitrary = DiffMK <$> QC.arbitrary
  shrink (DiffMK d) = DiffMK <$> QC.shrink d

instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
      => QC.Arbitrary (ValuesMK k v) where
  arbitrary = ValuesMK <$> QC.arbitrary
  shrink (ValuesMK vs) = ValuesMK <$> QC.shrink vs

deriving newtype instance (Ord k, QC.Arbitrary k, QC.Arbitrary v)
                       => QC.Arbitrary (Diff.Diff k v)

instance QC.Arbitrary v => QC.Arbitrary (Diff.NEDiffHistory v) where
  arbitrary = Diff.NEDiffHistory <$> ((:<||) <$> QC.arbitrary <*> QC.arbitrary)
  shrink (Diff.NEDiffHistory h) =
    fmap Diff.NEDiffHistory $ mapMaybe NESeq.nonEmptySeq $ QC.shrink (NESeq.toSeq h)

instance QC.Arbitrary v => QC.Arbitrary (Diff.DiffEntry v) where
  arbitrary = do
    constr <- QC.elements [
        Diff.Insert
      , Diff.Delete
      , Diff.UnsafeAntiInsert
      , Diff.UnsafeAntiDelete
      ]
    constr <$> QC.arbitrary

instance QC.Arbitrary ks => QC.Arbitrary (BS.RangeQuery ks) where
  arbitrary = BS.RangeQuery <$> QC.arbitrary <*> QC.arbitrary
  shrink (BS.RangeQuery x y) = BS.RangeQuery <$> QC.shrink x <*> QC.shrink y

newtype Fixed a = Fixed a
  deriving newtype (Show, Eq, Ord)
  deriving newtype (NoThunks, ToCBOR, FromCBOR)

deriving via QC.Fixed a instance QC.Arbitrary a => QC.Arbitrary (Fixed a)
