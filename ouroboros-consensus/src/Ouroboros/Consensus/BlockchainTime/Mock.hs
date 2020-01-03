{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime.Mock (
    -- * Fixed time
    fixedBlockchainTime
    -- * Testing time
  , NumSlots(..)
  , TestClock(..)
  , TestBlockchainTime(..)
  , newTestBlockchainTime
  , countSlotLengthChanges
  ) where

import           Control.Monad
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Fixed time
-------------------------------------------------------------------------------}

-- | A 'BlockchainTime' that is fixed to the given slot.
--
-- 'onSlotChange_' does nothing.
fixedBlockchainTime :: MonadSTM m => SlotNo -> BlockchainTime m
fixedBlockchainTime slot = BlockchainTime {
      getCurrentSlot = return slot
    , onSlotChange_  = const (return (return ()))
    }

{-------------------------------------------------------------------------------
  Testing time
-------------------------------------------------------------------------------}

-- | Number of slots
newtype NumSlots = NumSlots Word64
  deriving (Show)

-- | The current time during a test run.
data TestClock =
    Initializing
    -- ^ This phase has a non-zero but negligible duration.
  | Running !SlotNo
  deriving (Eq, Generic, NoUnexpectedThunks)

data TestBlockchainTime m = TestBlockchainTime
  { testBlockchainTime     :: BlockchainTime m
  , testBlockchainTimeDone :: m ()
    -- ^ Blocks until the end of the final requested slot.
  }

-- | Construct new blockchain time that ticks at the specified slot duration
--
-- NOTE: This is just one way to construct time. We can of course also connect
-- this to the real time (if we are in IO), or indeed to a manual tick
-- (in a demo).
--
-- NOTE: The number of slots is only there to make sure we terminate the
-- thread (otherwise the system will keep waiting).
--
-- NOTE: Any code not passed to 'onSlotChange' may start running \"before\" the
-- first slot @SlotNo 0@, i.e. during 'Initializing'. This is likely only
-- appropriate for initialization code etc. In contrast, the argument to
-- 'onSlotChange' is blocked at least until @SlotNo 0@ begins.
newTestBlockchainTime
    :: forall m. (IOLike m, HasCallStack)
    => ResourceRegistry m
    -> NumSlots           -- ^ Number of slots
    -> SlotLengths        -- ^ Slot duration
    -> m (TestBlockchainTime m)
newTestBlockchainTime registry (NumSlots numSlots) slotLens = do
    slotVar <- newTVarM initVal
    doneVar <- newEmptyMVar ()

    void $ forkLinkedThread registry $ loop slotVar doneVar

    let get :: STM m SlotNo
        get = blockUntilJust $
                (\case
                    Initializing -> Nothing
                    Running slot -> Just slot)
            <$> readTVar slotVar

        btime :: BlockchainTime m
        btime = BlockchainTime {
            getCurrentSlot = get
          , onSlotChange_  = fmap cancelThread .
              onEachChange registry Running (Just initVal) get
          }

    return $ TestBlockchainTime
      { testBlockchainTime = btime
      , testBlockchainTimeDone = readMVar doneVar
      }
  where
    loop :: StrictTVar m TestClock -> StrictMVar m () -> m ()
    loop slotVar doneVar = go slotLens numSlots
      where
        -- count off each requested slot
        go :: SlotLengths -> Word64 -> m ()
        go _  0 = putMVar doneVar () -- signal the end of the final slot
        go ls n = do
            atomically $ modifyTVar slotVar $ Running . \case
              Initializing -> SlotNo 0
              Running slot -> succ slot
            let (SlotLength delay, ls') = tickSlotLengths ls
            threadDelay (nominalDelay delay)
            go ls' (n - 1)

    initVal = Initializing

-- | Number of slot length changes if running for the specified number of slots
countSlotLengthChanges :: NumSlots -> SlotLengths -> Word64
countSlotLengthChanges = \(NumSlots n) -> go n
  where
    go :: Word64 -> SlotLengths -> Word64
    go limit (SlotLengths _ mNext) =
        case mNext of
          Nothing                      -> 0
          Just (SegmentLength n, next) ->
            if limit > n
              then 1 + go (limit - n) next
              else 0
