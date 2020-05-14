{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.HardFork.Abstract (
    HasHardForkHistory(..)
  ) where

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
  hardForkShape :: proxy blk
                -> LedgerConfig blk
                -> HardFork.Shape (HardForkIndices blk)

  -- | Ledger-dependent hard fork transitions
  hardForkTransitions :: LedgerConfig blk
                      -> LedgerState blk
                      -> HardFork.Transitions (HardForkIndices blk)
