{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Creating a mempool
module Ouroboros.Consensus.Mempool.Init (
    openMempool
  , openMempoolWithoutSyncThread
  ) where

import           Control.Monad (void)
import           Control.Tracer
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended (LedgerTables (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API hiding (MempoolCapacityBytes,
                     MempoolCapacityBytesOverride, MempoolSize,
                     TraceEventMempool, computeMempoolCapacity)
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common
import           Ouroboros.Consensus.Mempool.Query
import           Ouroboros.Consensus.Mempool.Update
import           Ouroboros.Consensus.Storage.LedgerDB
                     (LedgerDB (ledgerDbChangelog))
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
                     (changelogDiffs)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (Watcher (..), forkLinkedWatcher)

{-------------------------------------------------------------------------------
  Opening the mempool
-------------------------------------------------------------------------------}

-- | Create a @Mempool m blk@ in @m@ to manipulate the mempool. It will also
-- fork a thread that syncs the mempool and the ledger when the ledger changes.
openMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => ResourceRegistry m
  -> LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk)
openMempool registry ledger cfg capacityOverride tracer txSize = do
    env <- initMempoolEnv ledger cfg capacityOverride tracer txSize
    forkSyncStateOnTipPointChange registry env
    return $ mkMempool env

-- | Spawn a thread which syncs the 'Mempool' state whenever the 'LedgerState'
-- changes.
forkSyncStateOnTipPointChange :: forall m blk.
  ( IOLike m
  , LedgerSupportsMempool blk
  , HasTxId (GenTx blk)
  , ValidateEnvelope blk
  )
  => ResourceRegistry m
  -> MempoolEnv m blk
  -> m ()
forkSyncStateOnTipPointChange registry menv =
    void $ forkLinkedWatcher
      registry
      "Mempool.syncStateOnTipPointChange"
      Watcher {
          wFingerprint = id
        , wInitial     = Nothing
        , wNotify      = action
        , wReader      = getCurrentTip
        }
  where
    action :: Point blk -> m ()
    action _tipPoint = void $ implSyncWithLedger menv

    -- Using the tip ('Point') allows for quicker equality checks
    getCurrentTip :: STM m (Point blk)
    getCurrentTip =
          ledgerTipPoint
      <$> getCurrentLedgerState (mpEnvLedger menv)

-- | Unlike 'openMempool', this function does not fork a background thread
-- that synchronises with the ledger state whenever the later changes.
--
-- Intended for testing purposes.
openMempoolWithoutSyncThread
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerInterface m blk
  -> LedgerConfig blk
  -> MempoolCapacityBytesOverride
  -> Tracer m (TraceEventMempool blk)
  -> (GenTx blk -> TxSizeInBytes)
  -> m (Mempool m blk)
openMempoolWithoutSyncThread ledger cfg capacityOverride tracer txSize =
    mkMempool <$> initMempoolEnv ledger cfg capacityOverride tracer txSize

mkMempool
  :: ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => MempoolEnv m blk -> Mempool m blk
mkMempool mpEnv = Mempool
    { tryAddTxs      = implTryAddTxs mpEnv
    , removeTxs      = implRemoveTxs mpEnv
    , syncWithLedger = fst <$> implSyncWithLedger mpEnv
    , getSnapshot    = snapshotFromIS <$> readTMVar istate
    , getSnapshotFor = \slot st ldb (LedgerBackingStoreValueHandle s vh) ->
        let BackingStoreValueHandle {
                bsvhClose
              , bsvhRangeRead
              , bsvhRead
              } = vh
        in implGetSnapshotFor mpEnv slot st
            (unExtLedgerStateTables $ changelogDiffs $ ledgerDbChangelog ldb)
            (LedgerBackingStoreValueHandle s $
             BackingStoreValueHandle {
                  bsvhClose
                , bsvhRangeRead = \(RangeQuery prev count) ->
                      fmap unExtLedgerStateTables
                    . bsvhRangeRead
                    $ RangeQuery
                       (fmap ExtLedgerStateTables prev)
                       count
                , bsvhRead =
                      fmap unExtLedgerStateTables
                    . bsvhRead
                    . ExtLedgerStateTables
                }
            )
    , getCapacity    = isCapacity <$> readTMVar istate
    , getTxSize      = txSize
    }
  where
    MempoolEnv {
        mpEnvStateVar         = istate
      , mpEnvTxSize           = txSize
      } = mpEnv
