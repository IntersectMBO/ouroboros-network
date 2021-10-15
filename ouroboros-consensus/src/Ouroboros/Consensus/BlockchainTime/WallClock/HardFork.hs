{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.HardFork (
    BackoffDelay (..)
  , HardForkBlockchainTimeArgs (..)
  , hardForkBlockchainTime
  ) where

import           Control.Monad
import           Control.Tracer
import           Data.Time (NominalDiffTime)
import           Data.Void
import           GHC.Stack

import           Ouroboros.Consensus.BlockchainTime.API
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
import           Ouroboros.Consensus.HardFork.Abstract
import qualified Ouroboros.Consensus.HardFork.History as HF
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time

-- | A backoff delay
--
-- If the 'horizon' is very far away, the current tip is very far away from the
-- wallclock. However, that probably does not mean we have to wait @now -
-- horizon@ time: we are probably just syncing, and so the tip of the ledger
-- will rapidly move forward. So at most @now - horizon@ could be used as a
-- heuristic for how long to wait. For now we just trace it.
--
-- Instead, we just return a fixed delay of 'backoffDelay'. There is a
-- trade-off between trying to often, incurring computational overhead, and
-- missing the opportunity to produce a block. For mainnet, we anticipate a 60
-- second delay will keep both the computational overhead and the number of
-- slots we might miss reasonably small. We anyway can't guarantee the speed of
-- syncing, so delaying it by a further 60 seconds as needed does not change
-- anything fundamentally.
--
-- (NOTE: We could reduce this delay but Edsko doesn't think it would change
-- very much, and it would increase the frequency of the trace messages and
-- incur computational overhead.)
newtype BackoffDelay = BackoffDelay NominalDiffTime

data HardForkBlockchainTimeArgs m blk = HardForkBlockchainTimeArgs
  { hfbtBackoffDelay   :: m BackoffDelay
    -- ^ See 'BackoffDelay'
  , hfbtGetLedgerState :: STM m (LedgerState blk EmptyMK)
  , hfbtLedgerConfig   :: LedgerConfig blk
  , hfbtRegistry       :: ResourceRegistry m
  , hfbtSystemTime     :: SystemTime m
  , hfbtTracer         :: Tracer m (TraceBlockchainTimeEvent RelativeTime)
  , hfbtMaxClockRewind :: NominalDiffTime
    -- ^ Maximum time the clock can be rewound without throwing a fatal
    -- 'SystemClockMovedBack' exception.
    --
    -- When the slot length is short, e.g., Praos' 1s compared to PBFT's 20s,
    -- the chances of an NTP sync causing the clock to go back to the previous
    -- slot increase.
    --
    -- We allow the system clock to rewind up to 'hfbtMaxClockRewind', tracing a
    -- 'TraceSystemClockMovedBack' message in such cases. Note that the current
    -- slot *never decreases*, we just wait a bit longer in the same slot.
  }

-- | 'BlockchainTime' instance with support for the hard fork history
hardForkBlockchainTime :: forall m blk.
                          ( IOLike m
                          , HasHardForkHistory blk
                          , HasCallStack
                          )
                       => HardForkBlockchainTimeArgs m blk
                       -> m (BlockchainTime m)
hardForkBlockchainTime args = do
    run <- HF.runWithCachedSummary (summarize <$> getLedgerState)
    systemTimeWait

    (firstSlot, now, firstDelay) <- getCurrentSlot' tracer time run backoffDelay
    slotVar <- newTVarIO firstSlot
    void $ forkLinkedThread registry "hardForkBlockchainTime" $
             loop run slotVar firstSlot now firstDelay

    return $ BlockchainTime {
        getCurrentSlot = readTVar slotVar
      }
  where
    HardForkBlockchainTimeArgs
      { hfbtBackoffDelay   = backoffDelay
      , hfbtGetLedgerState = getLedgerState
      , hfbtLedgerConfig   = cfg
      , hfbtRegistry       = registry
      , hfbtSystemTime     = time@SystemTime{..}
      , hfbtTracer         = tracer
      , hfbtMaxClockRewind = maxClockRewind
      } = args

    summarize :: LedgerState blk mk -> HF.Summary (HardForkIndices blk)
    summarize st = hardForkSummary cfg st

    loop :: HF.RunWithCachedSummary xs m
         -> StrictTVar m CurrentSlot
         -> CurrentSlot     -- Previous slot
         -> RelativeTime    -- Current time
         -> NominalDiffTime -- Time to wait until next slot
         -> m Void
    loop run slotVar = go
      where
        go :: CurrentSlot -> RelativeTime -> NominalDiffTime -> m Void
        go prevSlot prevTime delay = do
           threadDelay (nominalDelay delay)
           (newSlot, newTime, newDelay) <- getCurrentSlot' tracer time run backoffDelay
           newSlot' <- checkValidClockChange (prevSlot, prevTime) (newSlot, newTime)
           atomically $ writeTVar slotVar newSlot'
           go newSlot' newTime newDelay

    checkValidClockChange ::
         (CurrentSlot, RelativeTime)
      -> (CurrentSlot, RelativeTime)
      -> m CurrentSlot
    checkValidClockChange (prevSlot, prevTime) (newSlot, newTime) =
        case (prevSlot, newSlot) of
          (CurrentSlotUnknown, CurrentSlot _)
            -- Unknown-to-known typically happens when syncing catches up far
            -- enough that we can now know what the current slot is.
            -> return newSlot
          (CurrentSlot _, CurrentSlotUnknown)
            -- Known-to-unknown can happen when the ledger is no longer being
            -- updated and time marches on past the end of the safe zone.
            -> return newSlot
          (CurrentSlotUnknown, CurrentSlotUnknown)
            -> return newSlot
          (CurrentSlot m, CurrentSlot n)
            -- Normally we expect @n == m + 1@, but if the system is under heavy
            -- load, we might miss a slot.
            | m <  n
            -> return newSlot
            -- We could have @n == m@ or @n < m@ only if the user's system clock
            -- was adjusted (say by an NTP process). We only allow a limited
            -- rewinding of the clock, but never rewind the slot number
            | m >= n
            , prevTime `diffRelTime` newTime <= maxClockRewind
            -> do traceWith tracer $ TraceSystemClockMovedBack prevTime newTime
                  return prevSlot
            | otherwise
            -> throwIO $ SystemClockMovedBack m n

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Get current slot, current time, and the delay until the next slot.
getCurrentSlot' :: forall m xs. IOLike m
                => Tracer m (TraceBlockchainTimeEvent RelativeTime)
                -> SystemTime m
                -> HF.RunWithCachedSummary xs m
                -> m BackoffDelay
                -> m (CurrentSlot, RelativeTime, NominalDiffTime)
getCurrentSlot' tracer SystemTime{..} run getBackoffDelay = do
    now   <- systemTimeCurrent
    mSlot <- atomically $ HF.cachedRunQuery run $ HF.wallclockToSlot now
    case mSlot of
      Left ex -> do
        -- give up for now and backoff; see 'BackoffDelay'
        traceWith tracer $ TraceCurrentSlotUnknown now ex
        BackoffDelay delay <- getBackoffDelay
        return (CurrentSlotUnknown, now, delay)
      Right (slot, _inSlot, timeLeft) -> do
        return (CurrentSlot slot, now, timeLeft)
