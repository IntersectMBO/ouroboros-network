{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended for qualified import
--
-- > import Test.Util.OracularClock (OracularClock(..))
-- > import qualified Test.Util.OracularClock as OracularClock
module Test.Util.HardFork.OracularClock (
    EndOfDaysException (..)
  , OracularClock (..)
  , forkEachSlot
  , mkOracularClock
  ) where

import           Control.Monad (void, when)
import           Data.Foldable (toList)
import           Data.Function (fix)
import           Data.Time
import           GHC.Stack

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Ouroboros.Consensus.Block
import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Ouroboros.Consensus.Util.Time (nominalDelay)

import           Test.Util.HardFork.Future (Future, futureSlotLengths,
                     futureSlotToTime, futureTimeToSlot)
import           Test.Util.Slots (NumSlots (..))

-- | A clock that knows the future
--
-- This clock's closure contains a 'BTime.SystemTime', a 'Future', and a
-- 'NumSlots'. Once all 'NumSlots' have passed, the clock is /exhausted/ and
-- all of its methods begin throwing 'EndOfDaysException'.
--
-- Notably, 'waitUntilDone' blocks until the the clock is exhausted; so the
-- continuation of that call should promptly reap other threads using this
-- clock because they will otherwise soon raise 'EndOfDaysException'.
--
-- Note: Though the wallclock-slot correspondence depends on the ledger state,
-- we have designed our ledgers so that all nodes necessarily use the same
-- correspondence in the absence of a Common Prefix violation. This ensures all
-- nodes adopt the same timeline, which must be /the/ 'Future' that this clock
-- anticipates.
data OracularClock m = OracularClock
    { -- | Returns 'True' if the requested slot is already over
      blockUntilSlot :: SlotNo -> m Bool

      -- | The current delay duration until the onset of the next slot
    , delayUntilNextSlot :: m NominalDiffTime

      -- | A mock system time
      --
      -- Note that 'BTime.systemTimeCurrent' eventually raises
      -- 'EndOfDaysException'.
    , finiteSystemTime :: BTime.SystemTime m

      -- | The current slot
    , getCurrentSlot :: m SlotNo

      -- | See 'forkEachSlot'
    , forkEachSlot_ :: HasCallStack
                    => ResourceRegistry m
                    -> String
                    -> (SlotNo -> m ())
                    -> m (m ())

      -- | Block until the clock is exhausted
    , waitUntilDone :: m ()
    }

-- | Forks a thread that executes an action at the onset of each slot
--
-- Returns an action that cancels the thread.
--
-- INVARIANT: In @io-sim@, there is no race:
-- @'finiteSystemTime'.systemTimeCurrent@ and hence 'getCurrentSlot' called
-- from within the given action will always return the correct slot.
--
-- See the discussion of ticker threads in 'getCurrentSlot'.
forkEachSlot :: HasCallStack
             => ResourceRegistry m
             -> OracularClock m
             -> String
             -> (SlotNo -> m ())
             -> m (m ())
forkEachSlot reg clk = forkEachSlot_ clk reg
    -- jumping the hoop so HasCallStack is useful

-- | See 'OracularClock'
--
-- NOTE: Every method relies only on the given 'BTime.SystemTime'. For example,
-- there is no internal ticker thread underlying this 'OracularClock'. This
-- design avoids the risk of certain kinds of races, particularly with respect
-- to
-- 'Ouroboros.Consensus.BlockchainTime.WallClock.HardFork.hardForkBlockchainTime'
-- which also only relies on 'BTime.SystemTime'.
--
-- PREREQUISITE: The only assumption about the given 'BTime.SystemTime' is that
-- its 'BTime.systemCurrentTime' ticks before any 'threadDelay'-ed thread
-- scheduled to wake-up then does so. The 'BTime.defaultSystemTime' in the mock
-- 'IO' monad provided by @io-sim@ satisfies this assumption.
mkOracularClock :: forall m. (IOLike m)
    => BTime.SystemTime m
    -> NumSlots
    -> Future
    -> OracularClock m
mkOracularClock BTime.SystemTime{..} numSlots future = OracularClock
    { blockUntilSlot = \slot -> do
        BTime.RelativeTime now <- finiteSystemTimeCurrent
        let later = futureSlotToTime future slot

        -- refuse to block until @>= endOfDays@
        when (later >= endOfDays) $ do
          threadDelay $ nominalDelay $ endOfDays - now
          exhaustedM

        blockUntilTime now later

    , delayUntilNextSlot = do
        (_slot, leftInSlot, _slotLength) <- getPresent
        pure leftInSlot

    , finiteSystemTime = BTime.SystemTime
        { BTime.systemTimeCurrent = finiteSystemTimeCurrent
        , BTime.systemTimeWait    = systemTimeWait
        }

    , getCurrentSlot = do
        (slot, _leftInSlot, _slotLength) <- getPresent
        pure slot

    , forkEachSlot_ = \rr threadLabel action ->
        fmap cancelThread $
        forkLinkedThread rr threadLabel $
        fix $ \loop -> do
          -- INVARIANT the slot returned here ascends monotonically unless
          -- the underlying 'BTime.SystemTime' jumps backwards
          (slot, leftInSlot, _slotLength) <- getPresent

          let lbl = threadLabel <> " [" <> show slot <> "]"
          -- fork the action, so it can't threadDelay us
          void $ forkLinkedThread rr lbl $ action slot

          threadDelay $ nominalDelay leftInSlot
          loop

    , waitUntilDone = do
        BTime.RelativeTime now <- finiteSystemTimeCurrent
        void $ blockUntilTime now endOfDays

    }
  where
    -- when the clock becomes exhausted
    endOfDays :: NominalDiffTime
    endOfDays =
        (sum . map BTime.getSlotLength) $
        (take (fromIntegral n) . toList) $
        futureSlotLengths future
      where
        NumSlots n = numSlots

    -- what any method called at exactly @endOfDays@ or blocked as of
    -- @endOfDays@ ends up doing at the exact @endOfDays@ moment
    exhaustedM :: forall a. m a
    exhaustedM = do
        -- throw if this thread isn't terminated in time
        threadDelay $ picosecondsToDiffTime 1   -- the smallest possible delay
        throwIO EndOfDaysException

    -- a 'BTime.systemTimeCurrent' that respects @endOfDays@
    finiteSystemTimeCurrent :: m BTime.RelativeTime
    finiteSystemTimeCurrent = do
        t <- systemTimeCurrent

        -- check if clock is exhausted
        let tFinal = BTime.RelativeTime endOfDays
        when (t >  tFinal) $ throwIO EndOfDaysException
        when (t == tFinal) $ exhaustedM

        pure t

    getPresent :: m (SlotNo, NominalDiffTime, BTime.SlotLength)
    getPresent = do
        BTime.RelativeTime now <- finiteSystemTimeCurrent
        pure $ futureTimeToSlot future now

    blockUntilTime :: NominalDiffTime -> NominalDiffTime -> m Bool
    blockUntilTime now later =
        case compare now later of
          LT -> do
              threadDelay $ nominalDelay $ later - now
              pure False
          EQ -> pure False
          GT -> pure True   -- ie " too late "

-----

-- | A thread used an 'OracularClock' well after it was exhausted
--
-- A thread using an exhausted 'OracularClock' first briefly delays, so that
-- finalizers etc have a chance to terminate it. If that tear down isn't prompt
-- enough, the thread then throws this exception, which we don't catch
-- anywhere.
data EndOfDaysException = EndOfDaysException
  deriving (Show)

instance Exception EndOfDaysException
