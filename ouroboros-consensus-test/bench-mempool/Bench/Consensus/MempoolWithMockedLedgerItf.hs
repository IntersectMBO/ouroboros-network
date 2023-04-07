{-# LANGUAGE FlexibleContexts #-}

-- | Mempool with a mocked ledger interface
module Bench.Consensus.MempoolWithMockedLedgerItf (
    InitialMempoolAndModelParams (..)
    -- * Mempool with a mocked LedgerDB interface
  , MempoolWithMockedLedgerItf (getMempool)
  , getTxs
  , openMempoolWithMockedLedgerItf
  ) where

import           Bench.Consensus.Mempool.Params
                     (InitialMempoolAndModelParams (..))
import           Control.Concurrent.Class.MonadSTM.Strict
                     (MonadSTM (atomically))
import           Control.DeepSeq (NFData (rnf))
import           Control.Tracer (Tracer, nullTracer)
import           Data.Foldable (foldMap')
import           Ouroboros.Consensus.HeaderValidation as Header
import           Ouroboros.Consensus.Ledger.Basics (LedgerState)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables as Ledger
import qualified Ouroboros.Consensus.Ledger.Tables.Utils as Ledger
import           Ouroboros.Consensus.Mempool (Mempool)
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Storage.LedgerDB (LedgerDB)
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
                     (LedgerBackingStore)
import qualified Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets as LedgerDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.Update as LedgerDB
import           System.Directory (getTemporaryDirectory)
import           System.FS.API (SomeHasFS (SomeHasFS))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)
import           System.IO.Temp (createTempDirectory)

data MempoolWithMockedLedgerItf m blk = MempoolWithMockedLedgerItf {
      getLedgerInterface    :: !(Mempool.LedgerInterface m blk)
    , getLedgerDB           :: !(LedgerDB (LedgerState blk))
    , getLedgerBackingStore :: !(LedgerBackingStore m (LedgerState blk))
    , getMempool            :: !(Mempool m blk)
    }

instance NFData (MempoolWithMockedLedgerItf m blk) where
  -- TODO: check we're OK with skipping the evaluation of the
  -- MempoolWithMockedLedgerItf. The only data we could force here is the
  -- 'LedgerState' inside 'getLedgerStateTVar', but that would require adding a
  -- 'NFData' constraint and perform unsafe IO. Since we only require this
  -- instance to be able to use
  -- [env](<https://hackage.haskell.org/package/tasty-bench-0.3.3/docs/Test-Tasty-Bench.html#v:env),
  -- and we only care about initializing the mempool before running the
  -- benchmarks, maybe this definition is enough.
  rnf MempoolWithMockedLedgerItf {} = ()

openMempoolWithMockedLedgerItf ::
     ( Ledger.LedgerSupportsMempool blk
     , Ledger.HasTxId (Ledger.GenTx blk)
     , Header.ValidateEnvelope blk
     , Ledger.CanSerializeLedgerTables (LedgerState blk)
     )
  => Mempool.MempoolCapacityBytesOverride
  -> Tracer IO (Mempool.TraceEventMempool blk)
  -> (Ledger.GenTx blk -> Mempool.TxSizeInBytes)
  -> InitialMempoolAndModelParams IO blk
    -- ^ Initial ledger state for the mocked Ledger DB interface.
  -> IO (MempoolWithMockedLedgerItf IO blk)
openMempoolWithMockedLedgerItf capacityOverride tracer txSizeImpl params = do
    -- Set up a backing store with initial values
    sysTmpDir <- getTemporaryDirectory
    tmpDir <- createTempDirectory sysTmpDir "mempool-bench"
    let lbsi = LedgerDB.newBackingStoreInitialiser nullTracer bss
        sfhs = SomeHasFS $ ioHasFS $ MountPoint tmpDir
        values = Ledger.projectLedgerTables backingState
    lbs <- LedgerDB.newBackingStore lbsi sfhs values

    -- Set up an empty changelog and populate it by applying blocks
    let ldb0 = LedgerDB.mkWithAnchor $ Ledger.forgetLedgerTables backingState
    ldb <- LedgerDB.pushMany
              (const $ pure ())
              ldbcfg
              (fmap LedgerDB.ReapplyVal blks)
              (LedgerDB.readKeySets lbs)
              ldb0

    -- Create a ledger interface, mimicking @getLedgerTablesAtFor@ from the
    -- @ChainDB.Impl.LgrDB@ module.
    let ledgerItf = Mempool.LedgerInterface {
            Mempool.getCurrentLedgerState = pure $ LedgerDB.current ldb
          , Mempool.getLedgerTablesAtFor = \pt txs -> do
              let keys = foldMap' Ledger.getTransactionKeySets txs
              LedgerDB.getLedgerTablesAtFor pt keys ldb lbs
          }

    mempool <- Mempool.openMempoolWithoutSyncThread
                   ledgerItf
                   lcfg
                   capacityOverride
                   tracer
                   txSizeImpl
    pure MempoolWithMockedLedgerItf {
        getLedgerInterface    = ledgerItf
      , getLedgerDB           = ldb
      , getLedgerBackingStore = lbs
      , getMempool            = mempool
    }
  where
    MempoolAndModelParams {
        immpBackingState         = backingState
      , immpLedgerConfig         = ldbcfg
      , immpBackingStoreSelector = bss
      , immpChangelogBlocks      = blks
      } = params

    lcfg = LedgerDB.ledgerDbCfg ldbcfg

getTxs ::
     (Ledger.LedgerSupportsMempool blk)
  => MempoolWithMockedLedgerItf IO blk -> IO [Ledger.GenTx blk]
getTxs mockedMempool = do
    snapshotTxs <- fmap Mempool.snapshotTxs $ atomically
                                            $ Mempool.getSnapshot
                                            $ getMempool mockedMempool
    pure $ fmap (Ledger.txForgetValidated . fst) snapshotTxs
