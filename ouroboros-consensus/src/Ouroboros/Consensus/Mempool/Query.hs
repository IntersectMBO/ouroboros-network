{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (
    implGetSnapshotFor
  , pureGetSnapshotFor
  ) where

import           Ouroboros.Consensus.Block.Abstract (Point, SlotNo, castHash,
                     pointHash)
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API hiding (MempoolCapacityBytes,
                     MempoolCapacityBytesOverride, MempoolSize,
                     TraceEventMempool, computeMempoolCapacity)
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common
import qualified Ouroboros.Consensus.Mempool.TxSeq as TxSeq
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
                     (PointNotFound (PointNotFound))
import           Ouroboros.Consensus.Util.IOLike

implGetSnapshotFor ::
     ( IOLike m
     , LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolEnv m blk
  -> Point blk -- ^ The point on the ledger database where we want to acquire a
               -- snapshot on
  -> SlotNo -- ^ Get snapshot for this slot number (usually the current slot)
  -> TickedLedgerState blk DiffMK -- ^ The ledger state at 'pt' ticked to 'slot'
  -> m (Maybe (MempoolSnapshot blk))
implGetSnapshotFor mpEnv pt slot ticked = do
  is <- atomically $ readTMVar istate
  if pointHash (isTip is) == castHash (getTipHash ticked) &&
     isSlotNo is == slot
    then
      -- We are looking for a snapshot exactly for the ledger state we already
      -- have cached, then just return it.
      pure . Just . snapshotFromIS $ is
    else do
      -- We need to revalidate the transactions.
      let txs = [ txForgetValidated . TxSeq.txTicketTx $ tx
                | tx <- TxSeq.toList $ isTxs is
                ]
          go = do
            mTbs <- getLedgerTablesAtFor ldgrInterface pt txs
            case mTbs of
              Right tbs            -> pure $ Just $ getSnap is tbs
              Left PointNotFound{} -> pure Nothing
      go
  where
    getSnap is tbs = pureGetSnapshotFor
                       capacityOverride
                       cfg
                       tbs
                       is
                       (ForgeInKnownSlot slot ticked)
    MempoolEnv { mpEnvStateVar         = istate
               , mpEnvLedgerCfg        = cfg
               , mpEnvLedger           = ldgrInterface
               , mpEnvCapacityOverride = capacityOverride
               } = mpEnv

-- | Get a snapshot of the mempool state that is valid with respect to
-- the given ledger state, together with the ticked ledger state.
pureGetSnapshotFor
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     )
  => MempoolCapacityBytesOverride
  -> LedgerConfig blk
  -> LedgerTables (LedgerState blk) ValuesMK
  -> InternalState blk
  -> ForgeLedgerState blk
  -> MempoolSnapshot blk
pureGetSnapshotFor _ _ _ _ ForgeInUnknownSlot{} =
  error "Tried to get a snapshot for unknown slot"
pureGetSnapshotFor capacityOverride cfg values is (ForgeInKnownSlot slot st) =
  snapshotFromIS $
    if (pointHash (isTip is) == castHash (getTipHash st) && isSlotNo is == slot)
    then is
    else fst $ revalidateTxsFor
                 capacityOverride
                 cfg
                 slot
                 st
                 values
                 (isLastTicketNo is)
                 (TxSeq.toList $ isTxs is)
