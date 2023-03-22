{-# LANGUAGE FlexibleContexts #-}

-- | Queries to the mempool
module Ouroboros.Consensus.Mempool.Query (pureGetSnapshotFor) where

import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Mempool.API hiding (MempoolCapacityBytes,
                     MempoolCapacityBytesOverride, MempoolSize,
                     TraceEventMempool, computeMempoolCapacity)
import           Ouroboros.Consensus.Mempool.Capacity
import           Ouroboros.Consensus.Mempool.Impl.Common

-- | Get a snapshot of the mempool state that is valid with respect to
-- the given ledger state
pureGetSnapshotFor
  :: ( LedgerSupportsMempool blk
     , HasTxId (GenTx blk)
     , ValidateEnvelope blk
     )
  => LedgerConfig blk
  -> ForgeLedgerState blk
  -> MempoolCapacityBytesOverride
  -> InternalState blk
  -> MempoolSnapshot blk
pureGetSnapshotFor cfg blockLedgerState capacityOverride =
      snapshotFromIS
    . internalStateFromVR
    . validateStateFor capacityOverride cfg blockLedgerState

