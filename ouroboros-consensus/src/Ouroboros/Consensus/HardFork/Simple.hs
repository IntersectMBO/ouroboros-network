{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Ouroboros.Consensus.HardFork.Simple (TriggerHardFork (..)) where

import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Cardano.Slotting.Slot (EpochNo)

-- | The trigger condition that will cause the hard fork transition.
data TriggerHardFork =
    -- | Trigger the transition when the on-chain protocol major version (from
    -- the ledger state) reaches this number.
    TriggerHardForkAtVersion !Word16
    -- | For testing only, trigger the transition at a specific hard-coded
    -- epoch, irrespective of the ledger state.
  | TriggerHardForkAtEpoch !EpochNo
    -- | Never trigger a hard fork
  | TriggerHardForkNever
  deriving (Show, Generic, NoThunks)
