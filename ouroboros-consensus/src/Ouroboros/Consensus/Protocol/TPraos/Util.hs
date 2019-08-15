-- | Assorted utility functions for TPraos integration.
--
-- In particular, various things we need for integration with the `delegation`
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Protocol.TPraos.Util where

import Cardano.Slotting.Slot
import Cardano.Slotting.EpochInfo
import Data.Functor.Identity (Identity(..))

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
isNewEpoch ::
  EpochInfo Identity ->
    -- | Slot we want to check
  SlotNo ->
    -- | Slot we are comparing a new epoch against
  WithOrigin SlotNo ->
  Bool
isNewEpoch ei newSlot referenceWO = let
    reference = fromWithOrigin genesisSlotNo referenceWO
  in runIdentity $ do
    oldEpoch <- epochInfoEpoch ei reference
    newEpoch <- epochInfoEpoch ei newSlot
    pure $ newEpoch > oldEpoch
