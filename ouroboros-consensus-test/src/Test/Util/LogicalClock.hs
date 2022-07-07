{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Logical time (in terms of abstract " ticks ")
--
-- Intended for qualified import
--
-- > import Test.Util.LogicalClock (LogicalClock)
-- > import qualified Test.Util.LogicalClock as LogicalClock
module Test.Util.LogicalClock (
    -- * API
    LogicalClock (..)
  , NumTicks (..)
  , Tick (..)
    -- * Construction
  , new
  , sufficientTimeFor
    -- * Scheduling actions
  , blockUntilTick
  , onTick
  , tickWatcher
  ) where

import           Control.Monad
import           Data.Time (NominalDiffTime)
import           Data.Word
import           GHC.Stack
import           System.Random (Random)

import qualified Ouroboros.Consensus.BlockchainTime as BTime
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM
import           Ouroboros.Consensus.Util.Time

{-------------------------------------------------------------------------------
  API
-------------------------------------------------------------------------------}

-- | Logical time unit
newtype Tick = Tick { tickToWord64 :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Num, Enum, Random)

-- | Number of ticks the test will run for
newtype NumTicks = NumTicks Word64

-- | Logical clock (in terms of ticks rather than actual 'UTCTime')
data LogicalClock m = LogicalClock {
      -- | Get the current " time "
      getCurrentTick :: STM m Tick

      -- | Wait for the end of time (each clock has a maximum number of ticks)
    , waitUntilDone  :: m ()

      -- | Translate the logical clock to mock 'SystemTime'
    , mockSystemTime :: BTime.SystemTime m
    }

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

new :: IOLike m => ResourceRegistry m -> NumTicks -> m (LogicalClock m)
new registry numTicks = newWithDelay registry numTicks tickDelay

-- | Set 'NumTicks' such that we will have seen all of the specified 'Tick's
sufficientTimeFor :: HasCallStack => [Tick] -> NumTicks
sufficientTimeFor [] = error "sufficientTimeFor: empty list"
sufficientTimeFor ts = NumTicks . maximum . map succ . map tickToWord64 $ ts

-- | Time of a single tick
--
-- The exact value of 'tickDelay' delay is not very important, but the scale
-- should be somewhat on par with the "real time" (for example, the relation
-- between GC delay and slot length, which would in turn depend on some kind of
-- relation between ticks and slots).
tickDelay :: NominalDiffTime
tickDelay = 0.5

{-------------------------------------------------------------------------------
  Scheduling actions
-------------------------------------------------------------------------------}

-- | Execute action on every clock tick
tickWatcher :: LogicalClock m
            -> (Tick -> m ())
            -> Watcher m Tick Tick
tickWatcher clock action =
    Watcher {
        wFingerprint = id
      , wInitial     = Nothing
      , wNotify      = action
      , wReader      = getCurrentTick clock
      }

-- | Execute action once at the specified tick
onTick :: (IOLike m, HasCallStack)
       => ResourceRegistry m
       -> LogicalClock m
       -> String
       -> Tick
       -> m ()
       -> m ()
onTick registry clock threadLabel tick action = do
    void $
      forkLinkedThread
        registry
        threadLabel
        (waitForTick clock tick >> action)

-- | Block until the specified tick
--
-- Returns 'False' if the current tick is later than the requested one, or
-- 'True' if they were equal.
blockUntilTick :: MonadSTM m => LogicalClock m -> Tick -> m Bool
blockUntilTick clock tick = atomically $ do
    now <- getCurrentTick clock
    if now > tick then
      return True
    else do
      when (now < tick) retry
      return False

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | Generalization of 'new' that allows to override the 'tickDelay'
--
-- NOTE: Tests using the logical clock really should not need to know what the
-- tick delay is; that's kind of the point of a /logical/ clock after all.
newWithDelay :: (IOLike m, HasCallStack)
             => ResourceRegistry m
             -> NumTicks
             -> NominalDiffTime
             -> m (LogicalClock m)
newWithDelay registry (NumTicks numTicks) tickLen = do
    current <- newTVarIO 0
    done    <- newEmptyMVar ()
    _thread <- forkThread registry "ticker" $ do
                 -- Tick 0 is the first tick, so increment @numTicks - 1@ times
                 replicateM_ (fromIntegral numTicks - 1) $ do
                   -- Give simulator chance to execute other threads
                   threadDelay (nominalDelay tickLen)
                   atomically $ modifyTVar current (+ 1)

                 -- Give tests that need to do some final processing on the last
                 -- tick a chance to do that before we indicate completion.
                 threadDelay (nominalDelay tickLen)
                 putMVar done ()

    return LogicalClock {
        getCurrentTick = Tick <$> readTVar current
      , waitUntilDone  = readMVar done
      , mockSystemTime = BTime.SystemTime {
            BTime.systemTimeCurrent = do
              tick <- atomically $ readTVar current
              return $ BTime.RelativeTime $ fromIntegral tick * tickLen
          , BTime.systemTimeWait =
              return ()
          }
      }

-- | Wait for the specified tick (blocking the current thread)
waitForTick :: IOLike m => LogicalClock m -> Tick -> m ()
waitForTick clock tick = do
    start <- atomically $ getCurrentTick clock
    when (start >= tick) $
      throwIO $ WaitForTickTooLate {
          tickRequest = tick
        , tickCurrent = start
        }

    atomically $ do
      now <- getCurrentTick clock
      check (now >= tick)

-- | Thrown by 'waitForTick' (and hence 'onTick')
data WaitForTickException =
    WaitForTickTooLate {
        -- | The time the action should have run at
        tickRequest :: Tick

        -- | The time when 'onTick' was called
      , tickCurrent :: Tick
      }
  deriving (Eq, Show)

instance Exception WaitForTickException
