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
    -- * Time utilities
  , FixedUTC(..)
  , fixedFromUTC
  , fixedToUTC
  , getCurrentFixedUTC
  , FixedDiffTime(..)
  , fixedDiffFromNominal
  , fixedDiffToNominal
  , fixedDiffToMicroseconds
  , threadDelayByFixedDiff
  , multFixedDiffTime
  , diffFixedUTC
  , addFixedUTC
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
  , Slot(..)
  ) where

import           Control.Monad
import           Data.Fixed
import           Data.Proxy
import           Data.Time
import           Data.Time.Clock.System

import           Control.Monad (void)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Network.Block (Slot (..))

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
      getCurrentSlot :: Tr m Slot

      -- | Spawn a thread to run an action each time the slot changes
    , onSlotChange   :: (Slot -> m ()) -> m ()
    }

-- | Execute action on specific slot
onSlot :: MonadSTM m => BlockchainTime m -> Slot -> m () -> m ()
onSlot BlockchainTime{..} slot act = onSlotChange $ \slot' -> do
    when (slot == slot') act

{-------------------------------------------------------------------------------
  Use in testing
-------------------------------------------------------------------------------}

-- | Number of slots
newtype NumSlots = NumSlots Int
  deriving (Show)

finalSlot :: NumSlots -> Slot
finalSlot (NumSlots n) = Slot (fromIntegral n)

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
                   -> Duration (Time m)  -- ^ Slot duration
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
    firstSlot :: Slot
    firstSlot = 0

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
  Time utilities

  We avoid using Double here to avoid rounding problems.
-------------------------------------------------------------------------------}

-- | Fixed precision UTC time (resolution: milliseconds)
--
-- > fixedToUTC (fixedFromUTC t) ~= t
-- > fixedFromUTC (fixedToUTC t) == t
newtype FixedUTC = FixedUTC { fixedSecondsSinceEpoch :: Fixed E3 }
  deriving (Eq, Ord)

instance Show FixedUTC where
  show = show . fixedToUTC

fixedFromUTC :: UTCTime -> FixedUTC
fixedFromUTC = FixedUTC . conv . utcToSystemTime
  where
    conv :: SystemTime -> Fixed E3
    conv (MkSystemTime secs nsecs) = MkFixed $
          (fromIntegral secs * 1_000)
        + (fromIntegral nsecs `div` 1_000_000)

fixedToUTC :: FixedUTC -> UTCTime
fixedToUTC = systemToUTCTime . conv . fixedSecondsSinceEpoch
  where
    conv :: Fixed E3 -> SystemTime
    conv (MkFixed millisecs) = MkSystemTime {
          systemSeconds     = fromIntegral $  millisecs `div` 1_000
        , systemNanoseconds = fromIntegral $ (millisecs `mod` 1_000) * 1_000_000
        }

getCurrentFixedUTC :: IO FixedUTC
getCurrentFixedUTC = fixedFromUTC <$> getCurrentTime

-- | Fixed precision time span (resolution: milliseconds)
--
-- > fixedToNominal (fixedFromNominal d) ~= d
-- > fixedFromNominal (fixedToNominal t) == t
newtype FixedDiffTime = FixedDiffTime { fixedDiffTime :: Fixed E3 }
  deriving (Eq, Ord, Num)

instance Show FixedDiffTime where
  show = show . fixedDiffToNominal

fixedDiffFromNominal :: NominalDiffTime -> FixedDiffTime
fixedDiffFromNominal = FixedDiffTime . doubleToFixed round . realToFrac

fixedDiffToNominal :: FixedDiffTime -> NominalDiffTime
fixedDiffToNominal = realToFrac . fixedToDouble . fixedDiffTime

fixedDiffToMicroseconds :: FixedDiffTime -> Int
fixedDiffToMicroseconds (FixedDiffTime d) = round (d * 1_000_000)

threadDelayByFixedDiff :: FixedDiffTime -> IO ()
threadDelayByFixedDiff = threadDelay . fixedDiffToMicroseconds

multFixedDiffTime :: Int -> FixedDiffTime -> FixedDiffTime
multFixedDiffTime n (FixedDiffTime d) = FixedDiffTime (fromIntegral n * d)

diffFixedUTC :: FixedUTC -> FixedUTC -> FixedDiffTime
diffFixedUTC (FixedUTC t) (FixedUTC t') = FixedDiffTime (t - t')

addFixedUTC :: FixedDiffTime -> FixedUTC -> FixedUTC
addFixedUTC (FixedDiffTime d) (FixedUTC t) = FixedUTC (t + d)

{-------------------------------------------------------------------------------
  Time to slots and back again
-------------------------------------------------------------------------------}

-- | Slot length
newtype SlotLength = SlotLength { getSlotLength :: FixedDiffTime }
  deriving (Show)

slotLengthFromMillisec :: Integer -> SlotLength
slotLengthFromMillisec = SlotLength . FixedDiffTime . conv
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Integer -> Fixed E3
    conv = MkFixed

slotLengthToMillisec :: SlotLength -> Integer
slotLengthToMillisec = conv . fixedDiffTime . getSlotLength
  where
    -- Explicit type annotation here means that /if/ we change the precision,
    -- we are forced to reconsider this code.
    conv :: Fixed E3 -> Integer
    conv (MkFixed n) = n

-- | System start
--
-- Slots are counted from the system start.
newtype SystemStart = SystemStart { getSystemStart :: FixedUTC }
  deriving (Show)

-- | Compute start of the specified slot
--
-- > slotAtTime (startOfSlot slot) == (slot, 0)
startOfSlot :: SlotLength -> SystemStart -> Slot -> FixedUTC
startOfSlot (SlotLength d) (SystemStart start) (Slot n) =
    addFixedUTC (multFixedDiffTime (fromIntegral n) d) start

-- | Compute slot at the specified time and how far we are into that slot
--
-- > now - slotLen < startOfSlot (fst (slotAtTime now)) <= now
-- > 0 <= snd (slotAtTime now) < slotLen
slotAtTime :: SlotLength -> SystemStart -> FixedUTC -> (Slot, FixedDiffTime)
slotAtTime (SlotLength d) (SystemStart start) now = conv $
              (fixedDiffTime (now `diffFixedUTC` start))
    `divMod'` (fixedDiffTime d)
  where
    conv :: (Word, Fixed E3) -> (Slot, FixedDiffTime)
    conv (slot, time) = (Slot slot, FixedDiffTime time)

-- | Compute time until the next slot and the number of that next slot
--
-- > 0 < fst (timeUntilNextSlot now) <= slotLen
-- > timeUntilNextSlot (startOfSlot slot) == (slotLen, slot + 1)
-- > slotAtTime (now + fst (timeUntilNextSlot now)) == bimap (+1) (const 0) (slotAtTime now)
-- > fst (slotAtTime (now + fst (timeUntilNextSlot now))) == snd (timeUntilNextSlot now)
timeUntilNextSlot :: SlotLength
                  -> SystemStart
                  -> FixedUTC
                  -> (FixedDiffTime, Slot)
timeUntilNextSlot slotLen start now =
    (getSlotLength slotLen - timeInCurrentSlot, currentSlot + 1)
  where
    (currentSlot, timeInCurrentSlot) = slotAtTime slotLen start now

-- | Get current slot
getCurrentSlotIO :: SlotLength -> SystemStart -> IO Slot
getCurrentSlotIO slotLen start =
    fst . slotAtTime slotLen start <$> getCurrentFixedUTC

-- | Wait until next slot, and return number of that slot
waitUntilNextSlotIO :: SlotLength -> SystemStart -> IO Slot
waitUntilNextSlotIO slotLen start = do
    now <- getCurrentFixedUTC
    let (delay, nextSlot) = timeUntilNextSlot slotLen start now
    threadDelayByFixedDiff delay
    return nextSlot

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Convert 'Double' to fixed precision
--
-- For precision 'E1', we have
--
-- >           1.000 1.010 1.040 1.049 1.050  1.051 1.090 1.100
-- > floor   | 1.0   1.0   1.0   1.0   1.0    1.0   1.0   1.1
-- > round   | 1.0   1.0   1.0   1.0   1.0(*) 1.1   1.1   1.1
-- > ceiling | 1.0   1.1   1.1   1.1   1.1    1.1   1.1   1.1
--
-- (*): See <https://en.wikipedia.org/wiki/IEEE_754#Rounding_rules>
doubleToFixed :: forall a. HasResolution a
              => (Double -> Integer) -- ^ Rounding policy
              -> Double -> Fixed a
doubleToFixed r d = MkFixed $ r (d * fromIntegral (resolution (Proxy @a)))

-- | Convert fixed precision number to 'Double'
fixedToDouble :: HasResolution a => Fixed a -> Double
fixedToDouble = realToFrac
