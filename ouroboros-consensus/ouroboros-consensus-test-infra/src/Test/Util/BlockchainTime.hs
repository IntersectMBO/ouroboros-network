module Test.Util.BlockchainTime (
    onSlot
  , OnSlotException(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           GHC.Stack

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

onSlot
  :: (HasCallStack, IOLike m)
  => ResourceRegistry m
  -> BlockchainTime m
  -> String
  -> SlotNo  -- ^ Label for the thread
  -> m ()
  -> m ()
onSlot registry btime label slot k = do
    startingSlot <- atomically $ getCurrentSlot btime
    when (startingSlot >= slot) $
      throwM $ OnSlotTooLate slot startingSlot
    void $ onSlotChange registry btime label $ \slot' ->
      when (slot == slot') k

data OnSlotException =
    -- | An action was scheduled via 'onSlot' for a slot in the past.
    -- First slot is requested, second slot is current as of raising.
    OnSlotTooLate SlotNo SlotNo
  deriving (Eq, Show)

instance Exception OnSlotException
