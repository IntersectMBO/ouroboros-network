{-# LANGUAGE FlexibleContexts #-}
-- | Pure side of the Mempool implementation.
--
-- Operations are performed in a pure style returning data types that model
-- the control flow through the operation and can then be interpreted to perform
-- the actual STM/IO operations.
module Ouroboros.Consensus.Mempool.Impl.Pure {-# DEPRECATED "User Ouroboros.Consensus.Mempool instead" #-} (
    -- * Mempool
    pureGetSnapshotFor
  , pureRemoveTxs
  , pureSyncWithLedger
    -- * MempoolSnapshot
  , implSnapshotFromIS
  ) where

import Ouroboros.Consensus.Ledger.Basics (LedgerState, GetTip)
import           Ouroboros.Consensus.Ticked (Ticked1)
import           Ouroboros.Consensus.Ledger.SupportsMempool

import           Ouroboros.Consensus.Mempool.Update
import           Ouroboros.Consensus.Mempool.Query
import           Ouroboros.Consensus.Mempool.Impl.Common
import           Ouroboros.Consensus.Mempool.API

{-# DEPRECATED implSnapshotFromIS "Use Ouroboros.Consensus.Mempool.Impl.Common (snapshotFromIS)" #-}
implSnapshotFromIS
  :: (HasTxId (GenTx blk), GetTip (Ticked1 (LedgerState blk)))
  => InternalState blk
  -> MempoolSnapshot blk
implSnapshotFromIS = snapshotFromIS
