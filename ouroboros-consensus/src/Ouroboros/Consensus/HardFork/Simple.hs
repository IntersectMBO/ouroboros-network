{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ouroboros.Consensus.HardFork.Simple (TriggerHardFork (..)) where

import           Cardano.Slotting.Slot (EpochNo)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

-- | The trigger condition that will cause the hard fork transition.
data TriggerHardFork =
    -- | Trigger the transition when the on-chain protocol major version (from
    -- the ledger state) reaches this number.
    --
    -- Note: The HFC logic does not require the trigger version for one era to
    -- be the successor of the trigger version for the previous era.
    TriggerHardForkAtVersion !Word16
    -- | For testing only, trigger the transition at a specific hard-coded
    -- epoch, irrespective of the ledger state.
  | TriggerHardForkAtEpoch !EpochNo
    -- | Never trigger a hard fork
  | TriggerHardForkNever
  deriving (Show, Generic, NoThunks)
