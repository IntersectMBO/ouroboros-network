{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended for qualified import
--
-- > import Test.Util.OracularClock (OracularClock(..))
-- > import qualified Test.Util.OracularClock as OracularClock
module Test.Util.HardFork.OracularClock (
    OracularClock (..)
  , new
  , onSlotChange
  , withinEachSlot
  , withinTheCurrentSlot
  ) where

import           Control.Monad (replicateM_, void)
import           Data.Foldable (toList)
import           Data.Time
import           GHC.Stack

import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Cardano.Slotting.Slot

import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Ouroboros.Consensus.Util.STM (onEachChange)
import           Ouroboros.Consensus.Util.Time (nominalDelay)

import           Test.Util.HardFork.Future (Future, futureSlotLengths,
                     futureTimeToSlot)
import           Test.Util.Slots (NumSlots (..))
import           Test.Util.Stream

-- | A clock that knows the future
--
-- This clock's closure contains a 'BTime.SystemTime', a 'Future', and a
-- 'NumSlots'. As the system time advances, the clock maintains a notion of the
-- current 'SlotNo'. Once all 'NumSlots' have passed, the clock is /exhausted/
-- and most of its methods block indefinitely from then on.
--
-- Most notably, 'waitUntilDone' instead blocks until the the clock is
-- exhausted; so the continuation of that call should expect other threads
-- using the rest of this clock's methods to be stuck (and eg reap them).
--
-- Note: though the wallclock-slot correspondence depends on the ledger state,
-- we have designed our ledgers so that all nodes necessarily use the same
-- correspondence in the absence of a Common Prefix violation. So this clock
-- can indeed know /the/ future.
data OracularClock m = OracularClock
    { -- | Returns 'True' if the requested slot is already over
      --
      -- Note: blocks indefinitely if clock becomes exhausted before requested
      -- slot.
      blockUntilSlot :: SlotNo -> m Bool

      -- | The current delay duration until the onset of the next slot
      --
      -- Note: blocks indefinitely if the clock is already exhausted.
    , delayUntilNextSlot :: m NominalDiffTime

      -- | A mock system time
      --
      -- Note: this 'BTime.systemTimeCurrent' blocks indefinitely if clock is
      -- already exhausted.
    , finiteSystemTime :: BTime.SystemTime m

      -- | Convert 'Control.Monad.Class.MonadTime.getCurrentTime' to 'SlotNo'
      --
      -- INVARIANT: In @io-sim@, the @getCurrentTime@ use (by default) the
      -- monotonic clock, and it advances /before/ threads correspondingly
      -- wake-up from a @threadDelay@.
      --
      -- So we rely on the (monotonic) clock instead of using a ticker thread
      -- and internal state, since a calling thread and the ticker thread might
      -- wake up simultaneously from a
      -- 'Control.Monad.Class.MonadTimer.threadDelay' call: it's ambiguous
      -- which thread would win the race to respectively read or increment the
      -- internal state.
      --
      -- Note: we do use a ticker thread for 'onSlotChange'.
      --
      -- Note: blocks indefinitely if the clock is already exhausted.
    , getCurrentSlot :: m SlotNo

      -- | See 'onSlotChange'
    , onSlotChange_ :: HasCallStack
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
onSlotChange :: HasCallStack
             => ResourceRegistry m
             -> OracularClock m
             -> String
             -> (SlotNo -> m ())
             -> m (m ())
onSlotChange reg clk = onSlotChange_ clk reg
    -- jumping the hoop so HasCallStack is useful

-- | Perform a " transaction " within the current slot
--
-- The callback should be idempotent.
--
-- We don't export a 'getCurrentSlotSTM' function because that leads to races
-- with 'threadDelay'. This function repeatedly attempts to execute the
-- callback until it is able to do so with 'getCurrentSlot' returning the same
-- provided value before and after.
--
-- Takes the expected current slot, returns the current slot.
withinTheCurrentSlot :: (Monad m)
                     => OracularClock m
                     -> SlotNo
                     -> (SlotNo -> m a)
                     -> m (SlotNo, Maybe a)
withinTheCurrentSlot clock s get = do
    a <- get s
    s' <- getCurrentSlot clock
    pure (s', if s == s' then Just a else Nothing)

-- | Perform a " transaction " once within each slot
--
-- The first callback should be idempotent.
--
-- See 'withinTheCurrentSlot'.
withinEachSlot :: (Monad m, HasCallStack)
               => OracularClock m
               -> SlotNo
               -> (SlotNo -> m a)
               -> (a -> m ())
               -> m ()
withinEachSlot clock s0 get put = do
    (s1, mbA) <- withinTheCurrentSlot clock s0 get
    case mbA of
      Nothing ->
          -- ticked while we were trying, so try again in this new slot
          withinEachSlot clock s1 get put
      Just a  -> do
          put a
          -- we succeed in this slot, so try again in the next
          let s2 = succ s1
          void $ blockUntilSlot clock s2
          withinEachSlot clock s2 get put

-- | See 'OracularClock'
new :: forall m. (IOLike m, HasCallStack)
    => BTime.SystemTime m
    -> ResourceRegistry m
    -> NumSlots
    -> Future
    -> m (OracularClock m)
new systemTime@BTime.SystemTime{..} registry (NumSlots n) future = do
    waitOneSlot <- do
      slotLengthsVar <- uncheckedNewTVarM $ futureSlotLengths future
      pure $ do
        slotLength <- atomically $ do
          x :< xs <- readTVar slotLengthsVar
          x <$ writeTVar slotLengthsVar xs
        threadDelay $ nominalDelay $ BTime.getSlotLength slotLength
    let _ = waitOneSlot :: m ()

    slotVar <- newTVarM 0
    doneVar <- newTVarM False
    void $ forkThread registry "OracularClock.ticker" $ do
      -- The first slot has already begun, so let other threads execute until
      -- the slot ends
      waitOneSlot
      -- And repeat for the @n - 1@ remaining slots
      replicateM_ (fromIntegral n - 1) $ do
        atomically $ modifyTVar slotVar (+ 1)
        waitOneSlot
      -- Set internal flag indicating the clock is exhausted (ie the test is
      -- over)
      atomically $ writeTVar doneVar True

    let readCurrentSlotSTM = do
            -- block forever if clock is exhausted
            readTVar doneVar >>= check . not

            readTVar slotVar

    let finiteSystemTimeCurrent = do
            t <- systemTimeCurrent

            -- block forever if clock is exhausted (this checks the time
            -- directly, without relying on 'doneVar')
            atomically $ do
              let totalDelta =
                      (sum . map BTime.getSlotLength) $
                      (take (fromIntegral n) . toList) $
                      futureSlotLengths future
                  tFinal = BTime.RelativeTime totalDelta
              check $ t < tFinal

            pure t

    let getPresent = do
            BTime.RelativeTime t <- finiteSystemTimeCurrent
            pure $ futureTimeToSlot future t

    pure OracularClock
      { blockUntilSlot = \slot -> atomically $ do
          now <- readCurrentSlotSTM
          case compare now slot of
            LT -> retry
            EQ -> pure False
            GT -> pure True   -- ie " too late "
      , delayUntilNextSlot = do
          (_slot, leftInSlot, _slotLength) <- getPresent
          pure leftInSlot
      , finiteSystemTime = systemTime
          { BTime.systemTimeCurrent = finiteSystemTimeCurrent
          }
      , getCurrentSlot = do
          (slot, _leftInSlot, _slotLength) <- getPresent
          pure slot
      , onSlotChange_ = \rr threadLabel action ->
          cancelThread <$>
            onEachChange
              rr
              threadLabel
              id
              Nothing
              readCurrentSlotSTM
              action
      , waitUntilDone = atomically $ readTVar doneVar >>= check
      }
