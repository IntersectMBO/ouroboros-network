module Ouroboros.Consensus.Node.LedgerDerivedInfo (
    LedgerDerivedInfo(..)
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime

-- | Information the node requires to run which is dependent on the ledger state
class LedgerDerivedInfo blk where
  -- | The known slot length
  --
  -- TODO: This should take the LedgerState as argument
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1637>
  knownSlotLengths :: BlockConfig blk -> SlotLengths

  -- TODO: This should have a function for epoch lengths
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
