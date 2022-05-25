{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Simple unit tests for the LMDB backing store.
module Main (main) where

import qualified Control.Tracer as Trace
import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified System.Directory as Dir
import           System.IO.Temp

import           Test.Tasty (TestTree)
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Slotting.Slot (WithOrigin (At))

import           Ouroboros.Consensus.Block (SlotNo, WithOrigin (Origin))
import           Ouroboros.Consensus.Ledger.Basics
import           Ouroboros.Consensus.Storage.FS.API (SomeHasFS (SomeHasFS))
import           Ouroboros.Consensus.Storage.FS.API.Types
                     (MountPoint (MountPoint))
import           Ouroboros.Consensus.Storage.FS.IO (ioHasFS)
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.BackingStore as HD
import qualified Ouroboros.Consensus.Storage.LedgerDB.HD.LMDB as LMDB

import           Test.Util.TestBlock ()

main :: IO ()
main = Tasty.defaultMain test1

{-------------------------------------------------------------------------------
  Unit tests
-------------------------------------------------------------------------------}

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
    { lgrTbl1 = ApplyValuesMK (HD.UtxoValues t1_1)
    , lgrTbl2 = ApplyValuesMK (HD.UtxoValues t2_1)
    } <- HD.bsvhRead vh1 simpleKeys
  Tasty.assertEqual "" (Map.singleton 1 True) t1_1
  Tasty.assertEqual "" (Map.singleton "1" 1) t2_1

  step "read2"
  TLedgerTables
    { lgrTbl1 = ApplyValuesMK (HD.UtxoValues t1_2)
    , lgrTbl2 = ApplyValuesMK (HD.UtxoValues t2_2)
    } <- HD.bsvhRead vh2 simpleKeys
  Tasty.assertEqual "" (Map.singleton 1 False) t1_2
  Tasty.assertEqual "" (Map.singleton "1" 2) t2_2

-- | Initialise an `LMDB` backing store and use it to run actions test-case steps.
withLMDB ::
     forall l. (TableStuff l, SufficientSerializationForAnyBackingStore l)
  => Maybe LMDB.LMDBLimits
  -> Maybe (LedgerTables l ValuesMK)
  -> (IO (LMDB.LMDBBackingStore l IO) -> TestTree)
  -> Tasty.TestTree
withLMDB mayLimits _mayInitVals act =
    Tasty.withResource (initLMDB mayLimits) cleanupLMDB $ \x -> act $ fmap snd x

-- | Initialise an @`LMDBBackingStore`@ inside a temporary directory.
initLMDB ::
     forall l. (TableStuff l, SufficientSerializationForAnyBackingStore l)
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
  { seqNo :: WithOrigin SlotNo
  , tbl1   :: mk Int Bool
  , tbl2   :: mk Text Word
  }

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => Eq (T mk)

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => (Eq (LedgerTables T mk))

instance ShowLedgerState T where
  showsLedgerState _ T{seqNo, tbl1, tbl2} = showParen True
    $ showString "T { seqNo = "
    . shows seqNo
    . showString ", tbl1 = "
    . showsApplyMapKind tbl1
    . showString ", tbl2 = "
    . showsApplyMapKind tbl2
    . showString " }"

instance ShowLedgerState (LedgerTables T) where
  showsLedgerState _ TLedgerTables{lgrTbl1, lgrTbl2} = showParen True
    $ showString "TLedgerTables { lgrTbl1 = "
    . showsApplyMapKind lgrTbl1
    . showString ", lgrTbl2 = "
    . showsApplyMapKind lgrTbl2
    . showString " }"

instance TableStuff T where
  data LedgerTables T mk = TLedgerTables
    { lgrTbl1 :: mk Int Bool
    , lgrTbl2 :: mk Text Word
    }

  projectLedgerTables T{tbl1, tbl2} =
    TLedgerTables { lgrTbl1 = tbl1, lgrTbl2 = tbl2 }

  withLedgerTables t TLedgerTables{lgrTbl1, lgrTbl2} =
    t { tbl1 = lgrTbl1, tbl2 = lgrTbl2 }

  pureLedgerTables f =
    TLedgerTables { lgrTbl1 = f, lgrTbl2 = f }

  mapLedgerTables f TLedgerTables{lgrTbl1, lgrTbl2} =
    TLedgerTables (f lgrTbl1) (f lgrTbl2)

  traverseLedgerTables f TLedgerTables{lgrTbl1, lgrTbl2} =
    TLedgerTables <$> f lgrTbl1 <*> f lgrTbl2

  zipLedgerTables f l r =
    TLedgerTables (f (lgrTbl1 l) (lgrTbl1 r)) (f (lgrTbl2 l) (lgrTbl2 r))

  zipLedgerTablesA f l r =
    TLedgerTables <$> f (lgrTbl1 l) (lgrTbl1 r) <*> f (lgrTbl2 l) (lgrTbl2 r)

  zipLedgerTables2 f l m r =
    TLedgerTables
      (f (lgrTbl1 l) (lgrTbl1 m) (lgrTbl1 r))
      (f (lgrTbl2 l) (lgrTbl2 m) (lgrTbl2 r))

  zipLedgerTables2A f l c r =
    TLedgerTables <$> f (lgrTbl1 l) (lgrTbl1 c) (lgrTbl1 r)
                  <*> f (lgrTbl2 l) (lgrTbl2 c) (lgrTbl2 r)

  foldLedgerTables f TLedgerTables{lgrTbl1, lgrTbl2} =
    f lgrTbl1 <> f lgrTbl2

  foldLedgerTables2 f l r =
    f (lgrTbl1 l) (lgrTbl1 r) <> f (lgrTbl2 l) (lgrTbl2 r)

  namesLedgerTables =
    TLedgerTables { lgrTbl1 = NameMK "tbl1", lgrTbl2 = NameMK "tbl2" }

instance  SufficientSerializationForAnyBackingStore T where
  codecLedgerTables =
    TLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)
                  (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

{-------------------------------------------------------------------------------
  Test utilities: Simple ledger tables
-------------------------------------------------------------------------------}

simpleWrite1 :: LedgerTables T DiffMK
simpleWrite1 = TLedgerTables
  { lgrTbl1 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton 1 (HD.UtxoEntryDiff True HD.UedsIns)
  , lgrTbl2 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton "1" (HD.UtxoEntryDiff 1 HD.UedsIns)
  }

simpleWrite2 :: LedgerTables T DiffMK
simpleWrite2 = TLedgerTables
  { lgrTbl1 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton 1 (HD.UtxoEntryDiff False HD.UedsIns)
  , lgrTbl2 = ApplyDiffMK $ HD.UtxoDiff $ Map.singleton "1" (HD.UtxoEntryDiff 2 HD.UedsIns)
  }

simpleKeys :: LedgerTables T KeysMK
simpleKeys = TLedgerTables
  { lgrTbl1 = ApplyKeysMK $ HD.UtxoKeys $ Set.singleton 1
  , lgrTbl2 = ApplyKeysMK $ HD.UtxoKeys $ Set.singleton "1"
  }
