{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ouroboros.Consensus.BlockchainTime.WallClock.HardFork (
    hardForkBlockchainTime
  ) where

import           Control.Monad
import           Control.Tracer
import           Data.Time (NominalDiffTime, UTCTime)
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

-- | 'BlockchainTime' instance with support for the hard fork history
hardForkBlockchainTime :: forall m blk.
                          ( IOLike m
                          , HasHardForkHistory blk
                          , HasCallStack
                          )
                       => ResourceRegistry m
                       -> Tracer m TraceBlockchainTimeEvent
                       -> SystemTime m
                       -> LedgerConfig blk
                       -> STM m (LedgerState blk)
                       -> m (BlockchainTime m)
hardForkBlockchainTime registry
                       tracer
                       time@SystemTime{..}
                       cfg
                       getLedgerState = do
    run <- HF.runWithCachedSummary (summarize <$> getLedgerState)
    waitUntilSystemStart tracer time

    (_now, firstSlot, firstDelay) <- getCurrentSlot' tracer time run
    slotVar <- newTVarM firstSlot
    void $ forkLinkedThread registry "hardForkBlockchainTime" $
             loop run slotVar firstSlot firstDelay

    return $ BlockchainTime {
        getCurrentSlot = readTVar slotVar
      }
  where
    summarize :: LedgerState blk -> HF.Summary (HardForkIndices blk)
    summarize st = hardForkSummary systemTimeStart cfg st

    loop :: HF.RunWithCachedSummary xs m
         -> StrictTVar m CurrentSlot
         -> CurrentSlot     -- Previous slot
         -> NominalDiffTime -- Time to wait until next slot
         -> m Void
    loop run slotVar = go
      where
        go :: CurrentSlot -> NominalDiffTime -> m Void
        go prevSlot delay = do
           threadDelay (nominalDelay delay)
           (now, newSlot, newDelay) <- getCurrentSlot' tracer time run
           checkValidClockChange now (prevSlot, newSlot)
           atomically $ writeTVar slotVar newSlot
           go newSlot newDelay

    checkValidClockChange :: UTCTime -> (CurrentSlot, CurrentSlot) -> m ()
    checkValidClockChange now = \case
        (CurrentSlotUnknown, CurrentSlot _) ->
          -- Unknown-to-known typically happens when syncing catches up far
          -- enough that we can now know what the current slot is.
          return ()
        (CurrentSlot _, CurrentSlotUnknown) ->
          -- Known-to-unknown can happen when the ledger is no longer being
          -- updated and time marches on past the end of the safe zone.
          return ()
        (CurrentSlotUnknown, CurrentSlotUnknown) ->
          return ()
        (CurrentSlot m, CurrentSlot n)
          -- Normally we expect @n == m + 1@, but if the system is under heavy
          -- load, we might miss a slot. We could have @n == m@ only if the
          -- user's system clock was adjusted (say by an NTP process).
          | m <  n    -> return ()
          | m == n    -> return ()
          | otherwise -> throwM $ SystemClockMovedBack now m n

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Get current time, current slot, and delay until next slot
getCurrentSlot' :: forall m xs. IOLike m
                => Tracer m TraceBlockchainTimeEvent
                -> SystemTime m
                -> HF.RunWithCachedSummary xs m
                -> m (UTCTime, CurrentSlot, NominalDiffTime)
getCurrentSlot' tracer SystemTime{..} run = do
    now   <- systemTimeCurrent
    mSlot <- atomically $ HF.cachedRunQuery run $ HF.wallclockToSlot now
    case mSlot of
      Left ex -> do
        -- If the 'horizon' is very far away, the current tip is very far away
        -- from the wallclock. However, that probably does not mean we have to
        -- wait @now - horizon@ time: we are probably just syncing, and so the
        -- tip of the ledger will rapidly move forward. So at most @now -
        -- horizon@ could be used as a heuristic for how long to wait. For now
        -- we just trace it.
        --
        -- Instead, we just return a fixed delay of 60 seconds. There is a
        -- trade-off between trying to often, incurring computational overhead,
        -- and missing the opportunity to produce a block. With a 60 second
        -- delay, computational overhead should be minimal, and the number of
        -- slots we might miss is minimal. We anyway can't guarantee the speed
        -- of syncing, so delaying it by a further 60 seconds does not change
        -- anything fundamentally.
        --
        -- (NOTE: We could reduce this delay but I don't think it would change
        -- very much, and it would increase the frequency of the trace messages
        -- and incur computational overhead.)
        traceWith tracer $ TraceCurrentSlotUnknown now ex
        return (now, CurrentSlotUnknown, 60)
      Right (slot, _inSlot, timeLeft) -> do
        return (now, CurrentSlot slot, timeLeft)
