{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Ouroboros.Consensus.Ticked (
    Ticked(..)
  ) where

import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

{-------------------------------------------------------------------------------
  Ticked state
-------------------------------------------------------------------------------}

-- | " Ticked " piece of state ('LedgerState', 'LedgerView', 'ChainIndepState')
--
-- Ticking refers to the passage of time (the ticking of the clock). When a
-- piece of state is marked as ticked, it means that time-related
-- changes have been applied to ledger state (or forecast).
--
-- Some examples of time related changes:
--
-- * Scheduled delegations might have been applied in Byron
-- * New leader schedule computed for Shelley
-- * Transition from Byron to Shelley activated in the hard fork combinator.
-- * Nonces switched out at the start of a new epoch.
data Ticked l = Ticked {
      -- | The slot number marking the time the state got ticked to
      tickedSlotNo :: !SlotNo

      -- | The ticked state itself
    , tickedState  :: !l
    }
  deriving stock    (Generic, Functor)
  deriving anyclass (NoUnexpectedThunks)
