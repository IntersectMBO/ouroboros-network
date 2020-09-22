module Ouroboros.Consensus.HardFork.History.Util (
    -- * Adding and subtracting slots/epochs
    addSlots
  , subSlots
  , addEpochs
  , countSlots
  , countEpochs
  ) where

import           Control.Exception (assert)
import           Data.Word

import           Ouroboros.Consensus.Block

{-------------------------------------------------------------------------------
  Adding and subtracting slots/epochs
-------------------------------------------------------------------------------}

addSlots :: Word64 -> SlotNo -> SlotNo
addSlots n (SlotNo x) = SlotNo (x + n)

subSlots :: Word64 -> SlotNo -> SlotNo
subSlots n (SlotNo x) = assert (x >= n) $ SlotNo (x - n)

addEpochs :: Word64 -> EpochNo -> EpochNo
addEpochs n (EpochNo x) = EpochNo (x + n)

-- | @countSlots to fr@ counts the slots from @fr@ to @to@ (@to >= fr@)
countSlots :: SlotNo -> SlotNo -> Word64
countSlots (SlotNo to) (SlotNo fr) = assert (to >= fr) $ to - fr

-- | @countEpochs to fr@ counts the epochs from @fr@ to @to@ (@to >= fr@)
countEpochs :: EpochNo -> EpochNo -> Word64
countEpochs (EpochNo to) (EpochNo fr) = assert (to >= fr) $ to - fr
