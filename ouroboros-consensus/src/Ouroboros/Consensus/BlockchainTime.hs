{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.BlockchainTime (
    -- * Abstract definition
    BlockchainTime(..)
  , blockUntilSlot
  , onSlotChange
  , onSlot
    -- * Use in testing
  , NumSlots(..)
  , TestBlockchainTime(..)
  , newTestBlockchainTime
    -- * Real blockchain time
  , realBlockchainTime
    -- * Time to slots and back again
  , SlotLength(..)
  , slotLengthFromMillisec
  , slotLengthToMillisec
  , SystemStart(..)
  , startOfSlot
  , slotAtTime
  , timeUntilNextSlot
  , getCurrentSlotIO
  , waitUntilNextSlotIO
    -- * Re-exports
  , SlotNo (..)
  ) where

import           Control.Exception (Exception (..))
import           Control.Monad
import           Data.Fixed
import           Data.Time
import           Data.Void
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadFork)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Ouroboros.Consensus.Util.MonadSTM.NormalForm

import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Network.Block (SlotNo (..))

{-------------------------------------------------------------------------------
  Abstract definition
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
      -- The thread will be linked to the registry in which the 'BlockchainTime'
      -- itself was created.
      --
      -- Use sites should call 'onSlotChange' rather than 'onSlotChange_'.
    , onSlotChange_  :: HasCallStack => (SlotNo -> m ()) -> m ()

      -- | Spawn a thread to run an action when reaching the specified slot
      --
      -- The thread will be linked to the registry in which the 'BlockchainTime'
      -- itself was created.
      --
      -- Unlike the thread created by 'onSlotChange_', this thread will
      -- terminate as soon as the action does.
      --
      -- Use sites should call 'onSlot' rather than 'onSlot_'.
    , onSlot_        :: HasCallStack => SlotNo -> m () -> m ()
    }

-- | Returns 'True' immediately if the requested slot is already over, else
-- blocks as requested and then returns 'False'
--
blockUntilSlot ::
     MonadSTM m
  => BlockchainTime m
  -> SlotNo
  -> m Bool
blockUntilSlot btime slot = do
    tooLate <- atomically $ do
        now <- getCurrentSlot btime
        if now > slot then pure True else do
            check $ now == slot
            pure False
    pure tooLate

-- | Wrapper around 'onSlotChange_' to ensure 'HasCallStack' constraint
--
-- See documentation of 'onSlotChange_'.
onSlotChange :: HasCallStack => BlockchainTime m -> (SlotNo -> m ()) -> m ()
onSlotChange = onSlotChange_

-- | Wrapper around 'onSlot_' to ensure 'HasCallStack' constraint
onSlot :: HasCallStack => BlockchainTime m -> SlotNo -> m () -> m ()
onSlot = onSlot_

-- | Default implementation of 'onSlot' (used internally only)
defaultOnSlot :: forall m.
                 ( MonadMask  m
                 , MonadFork  m
                 , MonadAsync m
                 , HasCallStack
                 )
              => ResourceRegistry m
              -> STM m SlotNo
              -> SlotNo
              -> m ()
              -> m ()
defaultOnSlot registry getCurrentSlot slot action = do
    startingSlot <- atomically getCurrentSlot
    when (startingSlot >= slot) $
      throwM $ OnSlotTooLate slot startingSlot
    runWhenJust registry waitForSlot (\() -> action)
  where
    waitForSlot :: STM m (Maybe ())
    waitForSlot = do
        currentSlot <- getCurrentSlot
        return $ if currentSlot >= slot
                   then Just ()
                   else Nothing

data OnSlotException =
    -- | An action was scheduled via 'onSlot' for a slot in the past.
    -- First slot is requested, second slot is current as of raising.
    OnSlotTooLate SlotNo SlotNo
  deriving (Eq, Show)

instance Exception OnSlotException

{-------------------------------------------------------------------------------
  Use in testing
-------------------------------------------------------------------------------}

-- | Number of slots
newtype NumSlots = NumSlots Int
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
    :: forall m. (
           MonadAsync m
         , MonadTimer m
         , MonadMask  m
         , MonadFork  m
         , HasCallStack
         )
    => ResourceRegistry m
    -> NumSlots           -- ^ Number of slots
    -> DiffTime           -- ^ Slot duration
    -> m (TestBlockchainTime m)
newTestBlockchainTime registry (NumSlots numSlots) slotLen = do
    slotVar <- newTVarM initVal
    doneVar <- newEmptyTMVarM

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
          , onSlot_        = defaultOnSlot registry get
          , onSlotChange_  = onEachChange registry Running (Just initVal) get
          }

    return $ TestBlockchainTime
      { testBlockchainTime = btime
      , testBlockchainTimeDone = atomically (readTMVar doneVar)
      }
  where
    loop :: StrictTVar m TestClock -> StrictTMVar m () -> m ()
    loop slotVar doneVar = do
        -- count off each requested slot
        replicateM_ numSlots $ do
          atomically $ modifyTVar slotVar $ Running . \case
            Initializing -> SlotNo 0
            Running slot -> succ slot
          threadDelay slotLen
        -- signal the end of the final slot
        atomically $ putTMVar doneVar ()

    initVal = Initializing

{-------------------------------------------------------------------------------
  "Real" blockchain time
-------------------------------------------------------------------------------}

-- | Real blockchain time
--
-- TODO: Right now this requires a single specific slot duration. This is
-- not going to be the case when we move to Praos. We need to think this
-- through carefully.
realBlockchainTime :: ResourceRegistry IO
                   -> SlotLength -> SystemStart
                   -> IO (BlockchainTime IO)
realBlockchainTime registry slotLen start = do
    first <- getCurrentSlotIO slotLen start
    slot  <- newTVarM first
    void $ forkLinkedThread registry $ loop slot
    return BlockchainTime {
        getCurrentSlot = readTVar slot
      , onSlot_        = defaultOnSlot registry (readTVar slot)
      , onSlotChange_  = onEachChange registry id (Just first) (readTVar slot)
      }
  where
    -- In each iteration of the loop, we recompute how long to wait until
    -- the next slot. This minimizes clock skew.
    loop :: StrictTVar IO SlotNo -> IO Void
    loop slot = forever $ do
      next <- waitUntilNextSlotIO slotLen start
      atomically $ writeTVar slot next

{-------------------------------------------------------------------------------
  Time to slots and back again
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: NominalDiffTime }
  deriving (Show)

slotLengthFromMillisec :: Integer -> SlotLength
slotLengthFromMillisec = SlotLength . conv
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Integer -> NominalDiffTime
    conv = (realToFrac :: Pico -> NominalDiffTime)
         . (/ 1000)
         . (fromInteger :: Integer -> Pico)

slotLengthToMillisec :: SlotLength -> Integer
slotLengthToMillisec = conv . getSlotLength
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: NominalDiffTime -> Integer
    conv = truncate
         . (* 1000)
         . (realToFrac :: NominalDiffTime -> Pico)

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: UTCTime }
  deriving (Show)

-- | Compute start of the specified slot
--
-- > slotAtTime (startOfSlot slot) == (slot, 0)
startOfSlot :: SlotLength -> SystemStart -> SlotNo -> UTCTime
startOfSlot (SlotLength d) (SystemStart start) (SlotNo n) =
    addUTCTime (fromIntegral n * d) start

-- | Compute slot at the specified time and how far we are into that slot
--
-- > now - slotLen < startOfSlot (fst (slotAtTime now)) <= now
-- > 0 <= snd (slotAtTime now) < slotLen
slotAtTime :: SlotLength -> SystemStart -> UTCTime -> (SlotNo, NominalDiffTime)
slotAtTime (SlotLength d) (SystemStart start) now =
    conv $ (now `diffUTCTime` start) `divMod'` d
  where
    conv :: (Word64, NominalDiffTime) -> (SlotNo, NominalDiffTime)
    conv (slot, time) = (SlotNo slot, time)

-- | Compute time until the next slot and the number of that next slot
--
-- > 0 < fst (timeUntilNextSlot now) <= slotLen
-- > timeUntilNextSlot (startOfSlot slot) == (slotLen, slot + 1)
-- > slotAtTime (now + fst (timeUntilNextSlot now)) == bimap (+1) (const 0) (slotAtTime now)
-- > fst (slotAtTime (now + fst (timeUntilNextSlot now))) == snd (timeUntilNextSlot now)
timeUntilNextSlot :: SlotLength
                  -> SystemStart
                  -> UTCTime
                  -> (NominalDiffTime, SlotNo)
timeUntilNextSlot slotLen start now =
    (getSlotLength slotLen - timeInCurrentSlot, succ currentSlot)
  where
    (currentSlot, timeInCurrentSlot) = slotAtTime slotLen start now

-- | Get current slot
getCurrentSlotIO :: SlotLength -> SystemStart -> IO SlotNo
getCurrentSlotIO slotLen start =
    fst . slotAtTime slotLen start <$> getCurrentTime

-- | Wait until next slot, and return number of that slot
waitUntilNextSlotIO :: SlotLength -> SystemStart -> IO SlotNo
waitUntilNextSlotIO slotLen start = do
    now <- getCurrentTime
    let (delay, nextSlot) = timeUntilNextSlot slotLen start now
    threadDelay ((realToFrac :: NominalDiffTime -> DiffTime) delay)
    return nextSlot
