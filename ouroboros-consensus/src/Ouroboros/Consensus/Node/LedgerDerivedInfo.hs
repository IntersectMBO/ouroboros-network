module Ouroboros.Consensus.Node.LedgerDerivedInfo (
    HasHardForkHistory(..)
  , LedgerDerivedInfo(..)
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.HardFork.Abstract

-- | Information the node requires to run which is dependent on the ledger state
class HasHardForkHistory blk => LedgerDerivedInfo blk where
  -- | The known slot length
  --
  -- TODO: This should take the LedgerState as argument
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1637>
  knownSlotLength :: BlockConfig blk -> SlotLength

  -- TODO: This should have a function for epoch lengths
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
