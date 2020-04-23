{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Time (
    -- * Test blockchain time API
    TestBlockchainTime -- opaque
  , testBlockchainTimeSlot
  , testBlockchainTimeDone
    -- * Construction
  , NumSlots(..)
  , newTestBlockchainTime
    -- * Alternative constructors
  , fixedBlockchainTime
    -- * Derived funtionality
  , testBlockchainTime
  , blockUntilSlot
  , onSlotChange
    -- * Executing an action once
  , OnSlotException(..)
  , onSlot
  ) where

import           Control.Monad
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

{-------------------------------------------------------------------------------
  Test blockchain time
-------------------------------------------------------------------------------}

-- | Test blockchain time
--
-- Unlike the real 'BlockchainTime' API, the 'TestBlockchainTime' API /always/
-- knows what the current 'SlotNo' is.
--
-- In addition, 'TestBlockchainTime' defines a /maximum/ number of slots that
-- the system will run for, and offers an API for waiting until that slot.
--
-- TODO: We will eventually need to generalize the tests also so that they do
-- not rely on knowing the time.
data TestBlockchainTime m = TestBlockchainTime
  { testBlockchainTimeSlot :: STM m SlotNo
  , testBlockchainTimeDone :: m ()
    -- ^ Blocks until the end of the final requested slot.
  }

{-------------------------------------------------------------------------------
  Construction
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
    -> SlotLength         -- ^ Slot duration
    -> m (TestBlockchainTime m)
newTestBlockchainTime registry (NumSlots numSlots) slotLen = do
    slotVar <- newTVarM Initializing
    doneVar <- newEmptyMVar ()

    void $ forkLinkedThread registry "TestBlockchainTime" $ loop slotVar doneVar

    return $ clone slotVar doneVar
  where
    loop :: StrictTVar m TestClock -> StrictMVar m () -> m ()
    loop slotVar doneVar = go numSlots
      where
        -- count off each requested slot
        go :: Word64 -> m ()
        go 0 = putMVar doneVar () -- signal the end of the final slot
        go n = do
            atomically $ modifyTVar slotVar $ Running . \case
              Initializing -> SlotNo 0
              Running slot -> succ slot
            threadDelay (nominalDelay (getSlotLength slotLen))
            go (n - 1)

    clone
      :: StrictTVar m TestClock
      -> StrictMVar m ()
      -> TestBlockchainTime m
    clone slotVar doneVar =
        TestBlockchainTime
          { testBlockchainTimeSlot = get
          , testBlockchainTimeDone = readMVar doneVar
          }
      where
        get :: STM m SlotNo
        get = blockUntilJust $
                (\case
                    Initializing -> Nothing
                    Running slot -> Just slot)
            <$> readTVar slotVar

{-------------------------------------------------------------------------------
  Alternative constructors

  TODO: These should be gone after
  <https://github.com/input-output-hk/ouroboros-network/pull/1989>.
-------------------------------------------------------------------------------}

-- | 'TestBlockchainTime' that is stuck on the given slot
fixedBlockchainTime :: MonadSTM m => SlotNo -> TestBlockchainTime m
fixedBlockchainTime slot = TestBlockchainTime {
      testBlockchainTimeSlot = return slot
    , testBlockchainTimeDone = error "fixedBlockchainTime: never done"
    }

{-------------------------------------------------------------------------------
  Derived functionality
-------------------------------------------------------------------------------}

testBlockchainTime :: TestBlockchainTime m -> BlockchainTime m
testBlockchainTime t = BlockchainTime {
      getCurrentSlot = testBlockchainTimeSlot t
    }

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

-- | Variant on 'onKnownSlotChange'
onSlotChange :: (IOLike m, HasCallStack)
             => ResourceRegistry m
             -> TestBlockchainTime m
             -> String            -- ^ Label for the thread
             -> (SlotNo -> m ())  -- ^ Action to execute
             -> m (m ())
onSlotChange registry btime label =
      fmap cancelThread
    . onEachChange registry label id Nothing (testBlockchainTimeSlot btime)

{-------------------------------------------------------------------------------
  Executing an action once
-------------------------------------------------------------------------------}

-- | Run the specified action once when the specified slot is reached
onSlot :: (HasCallStack, IOLike m)
       => ResourceRegistry m
       -> TestBlockchainTime m
       -> String -- ^ Label for the thread
       -> SlotNo
       -> m ()
       -> m ()
onSlot registry btime label slot k = do
    startingSlot <- atomically $ testBlockchainTimeSlot btime
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
