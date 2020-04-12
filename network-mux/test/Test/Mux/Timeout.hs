{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Mux.Timeout ( tests ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.IOSim

import           Network.Mux.Timeout (TimeoutFn,
                                      withTimeoutSerialNative,
                                      withTimeoutSerialAlternative)
import           Network.Mux.Time (microsecondsToDiffTime)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
    testGroup "Mux.Timeout"

    [ testGroup "single timeout"
      [ testProperty "withTimeoutSerialNative (Sim)" $
          prop_timeout_sim withTimeoutSerialNative

      , testProperty "withTimeoutSerialAlternative (Sim)" $
          prop_timeout_sim withTimeoutSerialAlternative

      , testProperty "withTimeoutSerialNative (IO)" $
          prop_timeout_io withTimeoutSerialNative

      , testProperty "withTimeoutSerialAlternative (IO)" $
          prop_timeout_io withTimeoutSerialAlternative
      ]

    , testGroup "series of timeouts"
      [ testProperty "withTimeoutSerialNative (Sim)" $
          prop_timeouts_sim withTimeoutSerialNative

      , testProperty "withTimeoutSerialAlternative (Sim)" $
          prop_timeouts_sim withTimeoutSerialAlternative

        -- Note that these IO tests can fail if run concurrently with CPU-bound
        -- threads. See the comments on schedulingToleranceIO.
      , testProperty "withTimeoutSerialNative (IO)" $
          mapSize (min 50) $
          prop_timeouts_io withTimeoutSerialNative

      , testProperty "withTimeoutSerialAlternative (IO)" $
          mapSize (min 50) $
          prop_timeouts_io withTimeoutSerialAlternative
      ]
    ]


type TimeoutConstraints m =
      ( MonadAsync m
      , MonadFork  m
      , MonadTime  m
      , MonadTimer m
      , MonadMask  m
      , MonadThrow (STM m)
      )

type WithTimeout m = forall b. TimeoutConstraints m
                  => (TimeoutFn m -> m b) -> m b

data SchedulingTolerance =
     SchedulingTolerance {
       negativeSchedulingTolerance :: DiffTime,
       positiveSchedulingTolerance :: DiffTime
     }


--
-- Properties
--

-- | Run an action under a timeout and check the outcome. The action runs for
-- 'ActionTime', and the timeout is set to the 'TimeoutTime'.
--
-- The guarantee that a timeout provides is that the action is not interrupted
-- too early. There is /no guarantee/ of being interrupted promptly after the
-- timeout time. The guarantee is only that it will /eventually/ be interrupted
-- after the timeout time. What we check here is that it gets interrupted
-- within a configurable scheduling tolerance. For example GHC's interval timer
-- for pre-emptive context switching is 20 milliseconds.
--
-- The valid outcomes are:
--
-- * If the timeout expired, the action time should have been the same or
--   longer than the timeout time.
--
-- * If the timeout did not expire, the action time should be less than the
--   timeout time plus a scheduling tolerance.
--
runSingleTimeoutExperiment
    :: (TimeoutConstraints m, MonadSTM m)
    => WithTimeout m
    -> SchedulingTolerance
    -> TimeoutTime
    -> ActionTime
    -> m Property
runSingleTimeoutExperiment withTimeout schedulingTolerance
                           timeoutTime actionTime = do

    before <- getMonotonicTime
    result <- withTimeout $ \timeout ->
                -- Allow the action to run for timeoutTime
                timeout timeoutTime $
                  -- Simulate an action that takes actionTime
                  threadDelay actionTime
    after  <- getMonotonicTime
    let actualTime = diffTime after before
    return $ experimentResult schedulingTolerance
                              timeoutTime actionTime
                              actualTime result

experimentResult :: SchedulingTolerance
                 -> TimeoutTime
                 -> ActionTime
                 -> DiffTime
                 -> Maybe ()
                 -> Property
experimentResult SchedulingTolerance {
                   negativeSchedulingTolerance,
                   positiveSchedulingTolerance
                 }
                 timeoutTime actionTime
                 actualTime result =
    sanityCheck .&&. timeoutCheck
  where
    expectedTime = min timeoutTime actionTime

    -- The overall actualTime should be in a range from the expected time
    -- to the expected time plus scheduling tolerance. In a perfect
    -- simulator the scheduling tolerance can be 0 so we hae to make the
    -- bounds inclusive.
    sanityCheck  = counterexample "sanity check failed" $
                   counterexample ("actualTime: " ++ show actualTime) $
                   actualTime >= expectedTime - negativeSchedulingTolerance
                && actualTime <= expectedTime + positiveSchedulingTolerance

    timeoutCheck =
      case result of
        -- The timeout /did/ go off, the action was interrupted.
        -- That is ok if the action time was the same or longer than the
        -- timeout. Whereas if the action time is less than the timeout
        -- then we violated the basic timeout property of not interrupting
        -- too early.
        Nothing ->
          counterexample "timeout fired" $
          actionTime >= timeoutTime - negativeSchedulingTolerance

        -- The timeout /did not/ go off, the action completed.
        -- That is ok if the action time is the same or less than the
        -- timeout time plus the scheduling tolerance.
        Just () ->
          counterexample "timeout did not fire" $
          actionTime <= timeoutTime + positiveSchedulingTolerance


prop_timeout_sim :: (forall s. WithTimeout (SimM s))
                 -> TimeoutTime -> ActionTime -> Property
prop_timeout_sim withTimeout timeoutTime actionTime =
    either (const (property False)) id $ runSim $
      runSingleTimeoutExperiment
        withTimeout schedulingToleranceSim
        timeoutTime actionTime


-- In the simulator the timeouts should be perfect.
schedulingToleranceSim :: SchedulingTolerance
schedulingToleranceSim =
    SchedulingTolerance {
      negativeSchedulingTolerance = 0.0,
      positiveSchedulingTolerance = 0.0
    }


prop_timeout_io :: WithTimeout IO -> TimeoutTime -> ActionTime -> Property
prop_timeout_io withTimeout timeoutTime actionTime =
    ioProperty $
      runSingleTimeoutExperiment
        withTimeout schedulingToleranceIO
        timeoutTime actionTime

-- | GHC's timer for pre-emptive context switching is 20 milliseconds.
-- We allow for 200 milliseconds tolerance after a timer fires. Even this
-- is sometimes not enough if there are lots of concurrent CPU-bound threads.
--
-- It is also imperfect in the negative direction and can wake up a few
-- microseconds too early. We allow 100 microseconds tolerance.
--
schedulingToleranceIO :: SchedulingTolerance
schedulingToleranceIO =
    SchedulingTolerance {
      negativeSchedulingTolerance = 0.001,
      positiveSchedulingTolerance = 1.0
    }


-- | Like runSingleTimeoutExperiment, but run a series of timeouts under the
-- 'withTimeout' action.
--
runSeriesOfTimeoutsExperiment
    :: (TimeoutConstraints m, MonadSTM m)
    => WithTimeout m
    -> SchedulingTolerance
    -> [(TimeoutTime, ActionTime)]
    -> m [Property]
runSeriesOfTimeoutsExperiment withTimeout schedulingTolerance times =
    withTimeout $ \timeout ->
      sequence [ singleExperiment timeout timeoutTime actionTime
               | (timeoutTime, actionTime) <- times ]
  where
    -- This is much the same as runSingleTimeoutExperiment above, but now
    -- within the scope of a single withTimeout. See the comments above
    -- for an explanation of the logic here.
    singleExperiment timeout timeoutTime actionTime = do
      before <- getMonotonicTime
      result <- timeout timeoutTime $ threadDelay actionTime
      after  <- getMonotonicTime
      let actualTime = diffTime after before
      return $ experimentResult schedulingTolerance
                                timeoutTime actionTime
                                actualTime result


prop_timeouts_sim :: (forall s. WithTimeout (SimM s))
                  -> [(TimeoutTime, ActionTime)] -> Property
prop_timeouts_sim withTimeout times =
    case runSim $ runSeriesOfTimeoutsExperiment
                    withTimeout schedulingToleranceSim times of
      Left err -> counterexample (show err) False
      Right rs -> conjoin rs


prop_timeouts_io :: WithTimeout IO
                 -> [(TimeoutTime, ActionTime)] -> Property
prop_timeouts_io withTimeout times =
    ioProperty $ do
      rs <- runSeriesOfTimeoutsExperiment
              withTimeout schedulingToleranceIO times
      return $ conjoin rs


--
-- Arbitrary instances
--


type TimeoutTime = DiffTime
type ActionTime  = DiffTime

instance Arbitrary DiffTime where
    arbitrary = millisecondsToDiffTime <$>
                frequency
                  [ (4, choose (0,  5))
                  , (1, choose (5, 10))
                  ]
      where
        millisecondsToDiffTime = microsecondsToDiffTime . (* 1000)

    shrink = map (fromRational . getNonNegative)
           . shrink
           . NonNegative
           . toRational

