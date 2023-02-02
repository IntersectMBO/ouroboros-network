-- | Limits on the ledger-specific _measure_ (eg size) of a sequence of
-- transactions
--
-- > import           Ouroboros.Consensus.Mempool.TxLimits (TxLimits)
-- > import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits
module Ouroboros.Consensus.Mempool.TxLimits {-# DEPRECATED "Use Ouroboros.Consensus.Mempool or Ouroboros.Consensus.Mempool.Capacity instead "#-} (
    ByteSize (..)
  , TxLimits (..)
  , (<=)
    -- * Restricting more strongly than the ledger's limits
  , Overrides
  , applyOverrides
  , getOverrides
  , mkOverrides
  , noOverridesMeasure
  ) where

import           Prelude hiding ((<=))
import           Ouroboros.Consensus.Mempool.Capacity 

type Overrides blk = TxOverrides blk
