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
module Main (main) where

import qualified Control.Tracer as Trace
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified System.Directory as Dir
import           System.IO.Temp

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

withLMDB ::
     forall l. (TableStuff l, SufficientSerializationForAnyBackingStore l)
  => Maybe LMDB.LMDBLimits
  -> (IO (LMDB.LMDBBackingStore l IO) -> Tasty.TestTree)
  -> Tasty.TestTree
withLMDB mb_limits act = let
    init_lmdb = do
      x <- Dir.getTemporaryDirectory
      tmpdir <- createTempDirectory x "cardano-lmdb"
      let
        fs = someHasFSIO tmpdir
        limits = fromMaybe LMDB.defaultLMDBLimits mb_limits
        empty_init :: LMDB.LMDBInit l
        empty_init = LMDB.LIInitialiseFromMemory Origin polyEmptyLedgerTables
      bs <- LMDB.newLMDBBackingStore
        (show `Trace.contramap` Trace.stdoutTracer)
        limits fs empty_init
      pure (tmpdir, bs)
    cleanup_lmdb (tmpdir, _) = do
      Dir.removeDirectoryRecursive tmpdir
  in Tasty.withResource init_lmdb cleanup_lmdb $ \x -> act $ fmap snd x

main :: IO ()
main = Tasty.defaultMain test1

test1 :: Tasty.TestTree
test1 = withLMDB Nothing $ \mbs -> Tasty.testCaseSteps "simple insert and read" $ \step -> do
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


someHasFSIO :: FilePath -> SomeHasFS IO
someHasFSIO fp = SomeHasFS $ ioHasFS $ MountPoint fp

data T mk = T
  { seq_no :: WithOrigin SlotNo
  , tbl1   :: mk Int Bool
  , tbl2   :: mk Text Word
  }

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => Eq (T mk)

deriving instance (Eq (mk Int Bool), Eq (mk Text Word)) => (Eq (LedgerTables T mk))

instance ShowLedgerState T where
  showsLedgerState _ T{seq_no, tbl1, tbl2} = showParen True
    $ showString "T { seq_no = "
    . shows seq_no
    . showString ", tbl1 = "
    . showsApplyMapKind tbl1
    . showString ", tbl2 = "
    . showsApplyMapKind tbl2
    . showString " }"

instance ShowLedgerState (LedgerTables T) where
  showsLedgerState _ TLedgerTables{od_tbl1, od_tbl2} = showParen True
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

  projectLedgerTables T{tbl1, tbl2} =
    TLedgerTables { od_tbl1 = tbl1, od_tbl2 = tbl2 }

  withLedgerTables t TLedgerTables{od_tbl1, od_tbl2} =
    t { tbl1 = od_tbl1, tbl2 = od_tbl2 }

  pureLedgerTables f =
    TLedgerTables { od_tbl1 = f, od_tbl2 = f }

  traverseLedgerTables f TLedgerTables{od_tbl1, od_tbl2} =
    TLedgerTables <$> f od_tbl1 <*> f od_tbl2

  zipLedgerTablesA f l r =
    TLedgerTables <$> f (od_tbl1 l) (od_tbl1 r) <*> f (od_tbl2 l) (od_tbl2 r)

  zipLedgerTables2A f l c r =
    TLedgerTables <$> f (od_tbl1 l) (od_tbl1 c) (od_tbl1 r)
                  <*> f (od_tbl2 l) (od_tbl2 c) (od_tbl2 r)

  namesLedgerTables =
    TLedgerTables { od_tbl1 = NameMK "tbl1", od_tbl2 = NameMK "tbl2" }

instance  SufficientSerializationForAnyBackingStore T where
  codecLedgerTables =
    TLedgerTables (CodecMK toCBOR toCBOR fromCBOR fromCBOR)
                  (CodecMK toCBOR toCBOR fromCBOR fromCBOR)

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
