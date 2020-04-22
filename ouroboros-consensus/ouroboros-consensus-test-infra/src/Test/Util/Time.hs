module Test.Util.Time (
    blockUntilSlot
  ) where

import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Util.IOLike

-- | Block until the specified slot
--
-- Returns 'True' immediately if the requested slot is already over, else
-- blocks as requested and then returns 'False'
blockUntilSlot :: IOLike m
               => TestBlockchainTime m
               -> SlotNo
               -> m Bool
blockUntilSlot btime slot = atomically $ do
    now <- testBlockchainTimeSlot btime
    if now > slot then
      return True
    else do
      check $ now == slot
      return False
