-- | Assorted utility functions for Shelley protocol integration.
--
-- In particular, various things we need for integration with the @delegation@
-- package from cardano-ledger-specs.
module Ouroboros.Consensus.Protocol.Ledger.Util (
    firstSlotOfEpochOfSlot
  , isNewEpoch
  ) where

import           Cardano.Slotting.EpochInfo
import           Data.Functor.Identity (Identity (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.History.Util (addEpochs,
                     countSlots)

-- | Verify whether a slot represents a change to a new epoch with regard to
-- some other slot.
--
-- PRECONDITION: the two slots must be in the same era.
isNewEpoch
  :: EpochInfo Identity
  -> WithOrigin SlotNo
     -- ^ Slot we are comparing a new epoch against
  -> SlotNo
      -- ^ Slot we want to check
  -> Bool
isNewEpoch ei reference newSlot = runIdentity $ do
    oldEpoch  <- case reference of
                   Origin      -> return $ EpochNo 0
                   NotOrigin s -> epochInfoEpoch ei s
    epochSize <- epochInfoSize  ei oldEpoch
    firstSlot <- epochInfoFirst ei oldEpoch

    let epochsAfter = (countSlots newSlot firstSlot) `div` unEpochSize epochSize
        newEpoch    = addEpochs epochsAfter oldEpoch
    -- Note that we don't call:
    -- > epochInfoEpoch ei newSlot
    -- because the 'EpochInfo' might have limited range. The precondition
    -- justifies the calculation that we do here.
    pure $ newEpoch > oldEpoch

-- | Return the first slot in the epoch of the given slot.
firstSlotOfEpochOfSlot ::
     EpochInfo Identity
  -> SlotNo
  -> SlotNo
firstSlotOfEpochOfSlot ei slot = runIdentity $ do
    epoch <- epochInfoEpoch ei slot
    epochInfoFirst ei epoch
