{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Consensus.BlockchainTime.API (
    BlockchainTime(..)
  , onKnownSlotChange
    -- * Testing
  , settableBlockchainTime
  ) where

import           GHC.Stack

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

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

-- | Spawn a thread to run an action each time the slot changes
--
-- The action will not be called until the current slot becomes known
-- (if the tip of our ledger is too far away from the current wallclock time,
-- we may not know what the current 'SlotId' is).
--
-- Returns a handle to kill the thread.
onKnownSlotChange :: (IOLike m, HasCallStack)
                  => ResourceRegistry m
                  -> BlockchainTime m
                  -> String            -- ^ Label for the thread
                  -> (SlotNo -> m ())  -- ^ Action to execute
                  -> m (m ())
onKnownSlotChange registry BlockchainTime{getCurrentSlot} label =
      fmap cancelThread
    . onEachChange registry label id Nothing getCurrentSlot

{-------------------------------------------------------------------------------
  Test infrastructure

  TODO: these will go after
  <https://github.com/input-output-hk/ouroboros-network/pull/1989>
-------------------------------------------------------------------------------}

-- | The current slot can be changed by modifying the given 'StrictTVar'.
--
-- 'onSlotChange_' is not implemented and will return an 'error'.
settableBlockchainTime :: MonadSTM m => StrictTVar m SlotNo -> BlockchainTime m
settableBlockchainTime varCurSlot = BlockchainTime {
      getCurrentSlot = readTVar varCurSlot
    }
