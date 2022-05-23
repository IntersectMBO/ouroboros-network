{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple unit tests for the LMDB backing store.
module Test.Ouroboros.Storage.LedgerDB.HD.LMDB (tests) where

import qualified Control.Tracer as Trace
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           Test.Tasty (TestTree, testGroup)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty
import           Test.Util.TestBlock ()
import qualified System.Directory as Dir
import           System.IO.Temp

import           Cardano.Slotting.Slot (WithOrigin(At))

import           Ouroboros.Consensus.Block (WithOrigin(Origin), SlotNo)
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (SomeHasFS))
import           Ouroboros.Consensus.Storage.FS.API.Types (MountPoint (MountPoint))
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB

{-------------------------------------------------------------------------------
  Unit tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "LMDB" [
    test1
  ]

-- | Perform a few simple writes and reads.
--
-- The test asserts that:
-- * The database sequence number increases monotonically across writes.
-- * The @`BackingStoreValueHandle`@s have a view of the database that is
--   consistent with the state of the database at the point in time when
--   the handles were created. That is, the value handles do not show changes
--   that were written /after/ the value handle was opened.
test1 :: TestTree
test1 =
  withLMDB Nothing Nothing $ \mbs ->
    Tasty.testCaseSteps "simple insert and read" $ \step -> do
      bs <- mbs

      step "write1"
      HD.bsWrite bs 1 simpleWrite1
      (seq1, vh1) <- HD.bsValueHandle bs
      Tasty.assertEqual "" (At 1) seq1

      step "write2"
      HD.bsWrite bs 2 simpleWrite2
      (seq2, vh2) <- HD.bsValueHandle bs
      Tasty.assertEqual "" (At 2) seq2

      step "read1"
      TLedgerTables
        { od_tbl1 = ApplyValuesMK (HD.UtxoValues t1_1)
        , od_tbl2 = ApplyValuesMK (HD.UtxoValues t2_1)
        } <- HD.bsvhRead vh1 simpleKeys
      Tasty.assertEqual "" (Map.singleton 1 True) t1_1
      Tasty.assertEqual "" (Map.singleton "1" 1) t2_1

      step "read2"
      TLedgerTables
        { od_tbl1 = ApplyValuesMK (HD.UtxoValues t1_2)
        , od_tbl2 = ApplyValuesMK (HD.UtxoValues t2_2)
        } <- HD.bsvhRead vh2 simpleKeys
      Tasty.assertEqual "" (Map.singleton 1 False) t1_2
      Tasty.assertEqual "" (Map.singleton "1" 2) t2_2

-- | Initialise an `LMDB` backing store and use it to run actions test-case steps.
withLMDB ::
     forall l. TableStuff l
  => Maybe LMDB.LMDBLimits
  -> Maybe (LedgerTables l ValuesMK)
  -> (IO (LMDB.LMDBBackingStore l IO) -> TestTree)
  -> Tasty.TestTree
withLMDB mayLimits _mayInitVals act =
    Tasty.withResource (initLMDB mayLimits) cleanupLMDB $ \x -> act $ fmap snd x

-- | Initialise an @`LMDBBackingStore`@ inside a temporary directory.
initLMDB ::
     forall l. TableStuff l
  => Maybe LMDB.LMDBLimits
  -> IO (FilePath, LMDB.LMDBBackingStore l IO)
initLMDB mayLimits = do
  x <- Dir.getTemporaryDirectory
  tmpdir <- createTempDirectory x "cardano-lmdb"
  let
    fs = someHasFSIO tmpdir
    limits = fromMaybe LMDB.defaultLMDBLimits mayLimits
    emptyInit :: LMDB.LMDBInit l
    emptyInit = LMDB.LIInitialiseFromMemory Origin polyEmptyLedgerTables
  bs <- LMDB.newLMDBBackingStore
    (show `Trace.contramap` Trace.stdoutTracer)
    limits fs emptyInit
  pure (tmpdir, bs)

-- | Remove a temporary directory that was created for an @`LMDBBackingStore`@.
cleanupLMDB :: (FilePath, b) -> IO ()
cleanupLMDB (tmpdir, _) = do
  Dir.removeDirectoryRecursive tmpdir

someHasFSIO :: FilePath -> SomeHasFS IO
someHasFSIO fp = SomeHasFS $ ioHasFS $ MountPoint fp

{-------------------------------------------------------------------------------
  Test utilities: Simple ledger
-------------------------------------------------------------------------------}

data T mk = T
  { seq_no :: WithOrigin SlotNo
  , tbl1 :: mk Int Bool
  , tbl2 :: mk Text Word
  }

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => Eq (T mk)

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => (Eq (LedgerTables T mk))

instance ShowLedgerState T where
  showsLedgerState _ T{..} = showParen True
    $ showString "T { seq_no = "
    . shows seq_no
    . showString ", tbl1 = "
    . showsApplyMapKind tbl1
    . showString ", tbl2 = "
    . showsApplyMapKind tbl2
    . showString " }"

instance ShowLedgerState (LedgerTables T) where
  showsLedgerState _ TLedgerTables{..} = showParen True
    $ showString "TLedgerTables { od_tbl1 = "
    . showsApplyMapKind od_tbl1
    . showString ", od_tbl2 = "
    . showsApplyMapKind od_tbl2
    . showString " }"

instance TableStuff T where
  data LedgerTables T mk = TLedgerTables
    { od_tbl1 :: mk Int Bool
    , od_tbl2 :: mk Text Word
    }

  projectLedgerTables T{..} = TLedgerTables { od_tbl1 = tbl1, od_tbl2 = tbl2 }

  withLedgerTables t TLedgerTables{..} = t { tbl1 = od_tbl1, tbl2 = od_tbl2 }

  pureLedgerTables f = TLedgerTables { od_tbl1 = f, od_tbl2 = f }

  mapLedgerTables f TLedgerTables{..} = TLedgerTables (f od_tbl1) (f od_tbl2)

  traverseLedgerTables f TLedgerTables{..} = TLedgerTables <$> f od_tbl1 <*> f od_tbl2

  zipLedgerTables f l r = TLedgerTables (f (od_tbl1 l) (od_tbl1 r)) (f (od_tbl2 l) (od_tbl2 r))

  zipLedgerTablesA f l r =
    TLedgerTables <$>
      f (od_tbl1 l) (od_tbl1 r) <*>
      f (od_tbl2 l) (od_tbl2 r)

  zipLedgerTables2 f l m r =
    TLedgerTables
      (f (od_tbl1 l) (od_tbl1 m) (od_tbl1 r))
      (f (od_tbl2 l) (od_tbl2 m) (od_tbl2 r))

  foldLedgerTables f TLedgerTables{..} = f od_tbl1 <> f od_tbl2

  foldLedgerTables2 f l r = f (od_tbl1 l) (od_tbl1 r) <> f (od_tbl2 l) (od_tbl2 r)

  namesLedgerTables = TLedgerTables { od_tbl1 = NameMK "tbl1", od_tbl2 = NameMK "tbl2" }

{-------------------------------------------------------------------------------
  Test utilities: Simple ledger tables
-------------------------------------------------------------------------------}

simpleWrite1 :: LedgerTables T DiffMK
simpleWrite1 = TLedgerTables
  { od_tbl1 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton 1 (HD.UtxoEntryDiff True HD.UedsIns)
  , od_tbl2 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton "1" (HD.UtxoEntryDiff 1 HD.UedsIns)
  }

simpleWrite2 :: LedgerTables T DiffMK
simpleWrite2 = TLedgerTables
  { od_tbl1 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton 1 (HD.UtxoEntryDiff False HD.UedsIns)
  , od_tbl2 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton "1" (HD.UtxoEntryDiff 2 HD.UedsIns)
  }

simpleKeys :: LedgerTables T KeysMK
simpleKeys = TLedgerTables
  { od_tbl1 = ApplyKeysMK $ HD.UtxoKeys $ Set.singleton 1
  , od_tbl2 = ApplyKeysMK $ HD.UtxoKeys $ Set.singleton "1"
  }
