{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.BlockchainTime (
    -- * Abstract definition
    BlockchainTime(..)
  , onSlot
    -- * Use in testing
  , NumSlots(..)
  , finalSlot
  , testBlockchainTime
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

import           Control.Monad
import           Data.Fixed
import           Data.Time
import           Data.Word (Word64)

import           Control.Monad (void)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer

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
    , onSlotChange   :: (SlotNo -> m ()) -> m ()
    }

-- | Execute action on specific slot
onSlot :: MonadSTM m => BlockchainTime m -> SlotNo -> m () -> m ()
onSlot BlockchainTime{..} slot act = onSlotChange $ \slot' -> do
    when (slot == slot') act

{-------------------------------------------------------------------------------
  Use in testing
-------------------------------------------------------------------------------}

-- | Number of slots
newtype NumSlots = NumSlots Int
  deriving (Show)

finalSlot :: NumSlots -> SlotNo
finalSlot (NumSlots n) = SlotNo (fromIntegral n)

-- | Construct new blockchain time that ticks at the specified slot duration
--
-- NOTE: This is just one way to construct time. We can of course also connect
-- this to the real time (if we are in IO), or indeed to a manual tick
-- (in a demo).
--
-- NOTE: The number of slots is only there to make sure we terminate the
-- thread (otherwise the system will keep waiting).
testBlockchainTime :: forall m. (MonadSTM m, MonadFork m, MonadTimer m)
                   => NumSlots           -- ^ Number of slots
                   -> DiffTime           -- ^ Slot duration
                   -> m (BlockchainTime m)
testBlockchainTime (NumSlots numSlots) slotLen = do
    slotVar <- atomically $ newTVar firstSlot
    void $ fork $ replicateM_ numSlots $ do
        threadDelay slotLen
        atomically $ modifyTVar slotVar succ
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      , onSlotChange   = onEachChange id firstSlot (readTVar slotVar)
      }
  where
    firstSlot = SlotNo 0

{-------------------------------------------------------------------------------
  "Real" blockchain time
-------------------------------------------------------------------------------}

-- | Real blockchain time
--
-- TODO: Right now this requires a single specific slot duration. This is
-- not going to be the case when we move to Praos. We need to think this
-- through carefully.
realBlockchainTime :: SlotLength -> SystemStart -> IO (BlockchainTime IO)
realBlockchainTime slotLen start = do
    first   <- getCurrentSlotIO slotLen start
    slotVar <- atomically $ newTVar first
    void $ fork $ forever $ do
      -- In each iteration of the loop, we recompute how long to wait until
      -- the next slot. This minimizes clock skew.
      next <- waitUntilNextSlotIO slotLen start
      atomically $ writeTVar slotVar next
    return BlockchainTime {
        getCurrentSlot = readTVar slotVar
      , onSlotChange   = onEachChange id first (readTVar slotVar)
      }

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

