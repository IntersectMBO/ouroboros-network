{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Ouroboros.Consensus.Node.LedgerDerivedInfo (
    HasHardForkHistory(..)
  , LedgerDerivedInfo(..)
  ) where

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.HardFork.History as HardFork
import           Ouroboros.Consensus.Ledger.Abstract

class HasHardForkHistory blk where
  -- | Type level description of the hard fork shape
  --
  -- The exact way in which this will used will be determined once we have the
  -- proper hard fork combinator, but intuitively it would be something like
  -- @'[Byron, Shelley, Goguen]@.
  type family HardForkIndices blk :: [*]

  -- | Static (ledger independent) hard fork shape
  hardForkShape :: BlockConfig blk
                -> HardFork.Shape (HardForkIndices blk)

  -- | Ledger-dependent hard fork transitions
  --
  -- TODO: This should eventually obsolete 'knownSlotLengths'
  -- TODO: This should eventually address #1205
  hardForkTransitions :: LedgerConfig blk
                      -> LedgerState blk
                      -> HardFork.Transitions (HardForkIndices blk)

-- | Information the node requires to run which is dependent on the ledger state
class HasHardForkHistory blk => LedgerDerivedInfo blk where
  -- | The known slot length
  --
  -- TODO: This should take the LedgerState as argument
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1637>
  knownSlotLengths :: BlockConfig blk -> SlotLengths

  -- TODO: This should have a function for epoch lengths
  -- <https://github.com/input-output-hk/ouroboros-network/issues/1205>
