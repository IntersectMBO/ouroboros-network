{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes  #-}

module Ouroboros.Consensus.BlockchainTime.API (
    -- * API
    BlockchainTime(..)
  , onSlotChange
    -- * Functionality in terms of the abstract API only
  , blockUntilSlot
  ) where

import           GHC.Stack

import           Control.Monad.Class.MonadSTM

import           Cardano.Prelude (NoUnexpectedThunks, OnlyCheckIsWHNF (..))

import           Ouroboros.Network.Block (SlotNo)

import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Blockchain time
--
-- When we run the blockchain, there is a single, global time. We abstract over
-- this here to allow to query this time (in terms of the current slot), and
-- execute an action each time we advance a slot.
data BlockchainTime m = BlockchainTime {
      -- | Get current slot
      getCurrentSlot :: STM m SlotNo

      -- | Spawn a thread to run an action each time the slot changes
      --
      -- Returns a handle to kill the thread.
      --
      -- The thread will be linked to the registry in which the 'BlockchainTime'
      -- itself was created.
      --
      -- Use sites should call 'onSlotChange' rather than 'onSlotChange_'.
    , onSlotChange_  :: HasCallStack => (SlotNo -> m ()) -> m (m ())
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "BlockchainTime" (BlockchainTime m)

-- | Wrapper around 'onSlotChange_' to ensure 'HasCallStack' constraint
--
-- See documentation of 'onSlotChange_'.
onSlotChange :: HasCallStack
             => BlockchainTime m -> (SlotNo -> m ()) -> m (m ())
onSlotChange = onSlotChange_

{-------------------------------------------------------------------------------
  Functionality in terms of the abstract API only
-------------------------------------------------------------------------------}

-- | Block until the specified slot
--
-- Returns 'True' immediately if the requested slot is already over, else
-- blocks as requested and then returns 'False'
blockUntilSlot :: IOLike m
               => BlockchainTime m
               -> SlotNo
               -> m Bool
blockUntilSlot btime slot = atomically $ do
    now <- getCurrentSlot btime
    if now > slot then
      return True
    else do
      check $ now == slot
      return False
