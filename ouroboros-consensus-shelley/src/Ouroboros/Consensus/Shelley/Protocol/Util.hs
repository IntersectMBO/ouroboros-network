-- | Assorted utility functions for Shelley protocol integration.
--
-- In particular, various things we need for integration with the @delegation@
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Shelley.Protocol.Util (
    isNewEpoch
  , firstSlotOfEpochOfSlot
  ) where

import           Cardano.Slotting.EpochInfo
import           Data.Functor.Identity (Identity (..))

import           Ouroboros.Consensus.Block

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
isNewEpoch
  :: EpochInfo Identity
  -> SlotNo
      -- ^ Slot we want to check
  -> WithOrigin SlotNo
     -- ^ Slot we are comparing a new epoch against
  -> Bool
isNewEpoch ei newSlot referenceWO = runIdentity $ do
    oldEpoch <- epochInfoEpoch ei reference
    newEpoch <- epochInfoEpoch ei newSlot
    pure $ newEpoch > oldEpoch
  where
    reference = fromWithOrigin genesisSlotNo referenceWO
    -- TODO
    genesisSlotNo = SlotNo 0

-- | Return the first slot in the epoch of the given slot.
firstSlotOfEpochOfSlot ::
     EpochInfo Identity
  -> SlotNo
  -> SlotNo
firstSlotOfEpochOfSlot ei slot = runIdentity $ do
    epoch <- epochInfoEpoch ei slot
    epochInfoFirst ei epoch
