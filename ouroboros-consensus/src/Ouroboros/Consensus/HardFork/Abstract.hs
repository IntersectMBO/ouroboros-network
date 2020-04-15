{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.HardFork.Abstract (
    HasHardForkHistory(..)
  ) where

import           Ouroboros.Consensus.Block
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
