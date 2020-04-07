{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

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
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (onEachChange)

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
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "BlockchainTime" (BlockchainTime m)

-- | Spawn a thread to run an action each time the slot changes
--
-- Returns a handle to kill the thread.
--
-- The thread will be linked to the registry in which the 'BlockchainTime'
-- itself was created.
onSlotChange :: (IOLike m, HasCallStack)
             => ResourceRegistry m
             -> BlockchainTime m
             -> String            -- ^ Label for the thread
             -> (SlotNo -> m ())  -- ^ Action to execute
             -> m (m ())
onSlotChange registry BlockchainTime{getCurrentSlot} label =
      fmap cancelThread
    . onEachChange registry label id Nothing getCurrentSlot

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
