module Test.Util.BlockchainTime (
    onSlot
  , OnSlotException(..)
  ) where

import           Control.Exception (Exception)
import           Control.Monad
import           GHC.Stack

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike

onSlot :: (HasCallStack, IOLike m) => BlockchainTime m -> SlotNo -> m () -> m ()
onSlot btime slot k = do
    startingSlot <- atomically $ getCurrentSlot btime
    when (startingSlot >= slot) $
      throwM $ OnSlotTooLate slot startingSlot
    onSlotChange btime $ \slot' ->
      when (slot == slot') k

data OnSlotException =
    -- | An action was scheduled via 'onSlot' for a slot in the past.
    -- First slot is requested, second slot is current as of raising.
    OnSlotTooLate SlotNo SlotNo
  deriving (Eq, Show)

instance Exception OnSlotException
