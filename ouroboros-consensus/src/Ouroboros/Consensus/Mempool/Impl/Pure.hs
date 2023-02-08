-- | Pure side of the Mempool implementation.
--
-- Operations are performed in a pure style returning data types that model
-- the control flow through the operation and can then be interpreted to perform
-- the actual STM/IO operations.
module Ouroboros.Consensus.Mempool.Impl.Pure {-# DEPRECATED "User Ouroboros.Consensus.Mempool instead!" #-} (
    -- * Mempool
    pureGetSnapshotFor
  , pureRemoveTxs
  , pureSyncWithLedger
    -- * MempoolSnapshot
  , snapshotFromIS
  ) where

import           Ouroboros.Consensus.Mempool.Update
import           Ouroboros.Consensus.Mempool.Query
import           Ouroboros.Consensus.Mempool.Impl.Common
