{-# LANGUAGE CPP              #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Mux.Timeout (tests) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer hiding (timeout)
import           Control.Monad.IOSim

import           Network.Mux.Time (microsecondsToDiffTime)
import           Network.Mux.Timeout (TimeoutFn, withTimeoutSerialAlternative,
                     withTimeoutSerialNative)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
    testGroup "Mux.Timeout"

    [ testGroup "Sim"
      [ testProperty "prop_timeout_sim withTimeoutSerialNative" $
          prop_timeout_sim withTimeoutSerialNative

      , testProperty "prop_timeout_sim withTimeoutSerialAlternative" $
          prop_timeout_sim withTimeoutSerialAlternative

      , testProperty "prop_timeouts_sim withTimeoutSerialNative" $
          prop_timeouts_sim withTimeoutSerialNative

      , testProperty "prop_timeouts_sim withTimeoutSerialAlternative" $
          prop_timeouts_sim withTimeoutSerialAlternative
      ]

    , testGroup "IO"
      [ testProperty "prop_timeout_io withTimeoutSerialNative" $
          prop_timeout_io withTimeoutSerialNative

      , testProperty "prop_timeout_io withTimeoutSerialAlternative" $
          prop_timeout_io withTimeoutSerialAlternative

#if !defined(mingw32_HOST_OS)
        -- Note that these IO tests can fail if run concurrently with CPU-bound
        -- threads. See the comments on schedulingToleranceIO.
        --
        -- TODO: issue #2612
{-
      , testProperty "prop_timeouts_io withTimeoutSerialNative" $
          mapSize (min 50) $
          prop_timeouts_io withTimeoutSerialNative
-}
      , testProperty "prop_timeouts_io withTimeoutSerialAlternative" $
          mapSize (min 50) $
          prop_timeouts_io withTimeoutSerialAlternative
#endif
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
       positiveSchedulingTolerance :: DiffTime,
       -- number of cases that can be outside of `negativeSchedulingTolerance`
       -- and `positiveSchedulingTolerance`.
       maxFailures                 :: Int
     }


--
-- Properties
--

data WithSanityCheck prop
  = WithSanityCheck        prop

  -- | The first one represents the property without sanity check, the other one
  -- sanity check (which failed). It is kept to keep its `counterexample`s.
  | WithSanityCheckFailure prop prop
  deriving (Functor)

ignoreSanityCheck :: WithSanityCheck prop -> prop
ignoreSanityCheck (WithSanityCheck    prop)       = prop
ignoreSanityCheck (WithSanityCheckFailure prop _) = prop

withSanityCheck :: WithSanityCheck Property -> Property
withSanityCheck (WithSanityCheck        prop)             = prop
withSanityCheck (WithSanityCheckFailure prop sanityCheck) = prop .&&. sanityCheck

isSanityCheckIgnored :: WithSanityCheck prop -> Bool
isSanityCheckIgnored WithSanityCheck{}         = False
isSanityCheckIgnored WithSanityCheckFailure {} = True


-- | Run an action under a timeout and check the outcome. The action runs for
-- at least 'ActionDuration', and the timeout is set to the 'TimeoutDuration'.
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
singleTimeoutExperiment
    :: TimeoutConstraints m
    => TimeoutFn m
    -> SchedulingTolerance
    -> TimeoutDuration
    -> ActionDuration
    -> m (WithSanityCheck Property)
singleTimeoutExperiment timeout schedulingTolerance
                        intendedTimeoutDuration
                        intendedActionDuration = do

    before <- getMonotonicTime

              -- Allow the action to run for intendedTimeoutDuration
    result <- timeout intendedTimeoutDuration $ do

                  -- Simulate an action that should take intendedActionDuration
                  threadDelay intendedActionDuration

                  -- but we also measure the actual duration
                  getMonotonicTime

    after  <- getMonotonicTime

    return $ experimentResult schedulingTolerance
                              intendedTimeoutDuration
                              intendedActionDuration
                              before after result

experimentResult :: SchedulingTolerance
                 -> TimeoutDuration
                 -> ActionDuration
                 -> Time
                 -> Time
                 -> Maybe Time
                 -> WithSanityCheck Property
experimentResult SchedulingTolerance {
                   negativeSchedulingTolerance,
                   positiveSchedulingTolerance
                 }
                 intendedTimeoutDuration
                 intendedActionDuration
                 before after result =
    counterexamples
      [ "intendedTimeoutDuration: " ++ show intendedTimeoutDuration
      , "intendedActionDuration:  " ++ show intendedActionDuration
      , "intendedOverallDuration: " ++ show intendedOverallDuration
      , "actualOverallDuration:   " ++ show actualOverallDuration
      ] <$>
      if ignoredSanityCheck
        then WithSanityCheckFailure timeoutCheck sanityCheck
        else WithSanityCheck $ sanityCheck .&&. timeoutCheck
  where
    actualOverallDuration   = diffTime after before
    intendedOverallDuration = min intendedTimeoutDuration intendedActionDuration

    -- The actual overall duration should be in a range from the intended
    -- duration to the expected duration plus scheduling tolerance. In a perfect
    -- simulator the scheduling tolerance can be 0 so we have to make the
    -- bounds inclusive.
    sanityCheck    =
         counterexamples
           [ "negativeSchedulingTolerance: " ++ show negativeSchedulingTolerance
           , "violation of timer sanity property:\n" ++
             "  actualOverallDuration >= intendedOverallDuration" ++
             "                         - negativeSchedulingTolerance"
           ] sanityCheckLow
       .&&.
         counterexamples
           [ "positiveSchedulingTolerance: " ++ show positiveSchedulingTolerance
           , "violation of timer sanity property:\n" ++
             "  actualOverallDuration <= intendedOverallDuration\n" ++
             "                         + positiveSchedulingTolerance"
           ]
           sanityCheckHigh

    ignoredSanityCheck =
         actualOverallDuration < intendedOverallDuration
                               - negativeSchedulingTolerance
      || actualOverallDuration > intendedOverallDuration
                               + 100 * positiveSchedulingTolerance

    sanityCheckLow =
      actualOverallDuration >= intendedOverallDuration
                             - negativeSchedulingTolerance

    sanityCheckHigh =
      actualOverallDuration <= intendedOverallDuration
                             + positiveSchedulingTolerance

    timeoutCheck =
      case result of
        -- The timeout /did/ go off, the action was interrupted.
        --
        -- How do we know if this is ok, given that both the action and the
        -- timeout could be late due to scheduling and other busy threads?
        -- We do not know the actual action time because of course it got
        -- interrupted. We only know the intended action time, intended timeout
        -- time and actual overall time.
        --
        -- One might think that if the action was really short and the timeout
        -- rather longer, but the timeout went off anyway, that this should be
        -- a violation. But that's hard to ensure because the action itself can
        -- take longer than expected due to thread scheduling issues.
        --
        -- The only thing we really need to check here is that we have not
        -- violated the basic timeout property of not interrupting too early.
        -- So that means the actual overall time should be at least the length
        -- of the intended timeout.
        --
        Nothing ->
          counterexamples
            [ "timeout fired (but should not have)"
            , "violation of timeout property:\n" ++
              "  actualOverallDuration <= intendedTimeoutDuration\n" ++
              "                         - negativeSchedulingTolerance"
            ] $
          actualOverallDuration >= intendedTimeoutDuration
                                 - negativeSchedulingTolerance

        -- The timeout /did not/ go off, the action completed. This means
        -- we do know how long the action actually took.
        --
        -- This outcome is certainly ok if the actual action duration is the
        -- same or less than the intended timeout duration. The timeout can
        -- also be late due to thread scheduling issues, so this outcome is
        -- also ok if actual action duration is the same or less than the
        -- intended timeout duration plus the scheduling tolerance.
        Just afterAction ->
          let actualActionDuration = diffTime afterAction before in
          counterexamples
            [ "actualActionDuration:  " ++ show actualActionDuration
            , "timeout did not fire (but should not have)"
            , "violation of timeout property:\n" ++
              "  actualActionDuration <= intendedTimeoutDuration\n" ++
              "                        + positiveSchedulingTolerance"
            ] $
          actualActionDuration <= intendedTimeoutDuration
                                + positiveSchedulingTolerance


-- | Run a 'singleTimeoutExperiment' under a single 'withTimeout' scope.
--
prop_timeout
    :: TimeoutConstraints m
    => WithTimeout m
    -> SchedulingTolerance
    -> TimeoutDuration
    -> ActionDuration
    -> m (WithSanityCheck Property)
prop_timeout withTimeout
             schedulingTolerance
             intendedTimeoutDuration
             intendedActionDuration =
    withTimeout $ \timeout ->
      singleTimeoutExperiment
        timeout
        schedulingTolerance
        intendedTimeoutDuration
        intendedActionDuration


-- | Like 'prop_timeout', but run a series of timeouts under the same
-- 'withTimeout' scope.
--
prop_timeouts
    :: TimeoutConstraints m
    => WithTimeout m
    -> SchedulingTolerance
    -> [(TimeoutDuration, ActionDuration)]
    -> m Property
prop_timeouts withTimeout schedulingTolerance@SchedulingTolerance { maxFailures } times =
    withTimeout $ \timeout ->
      conjoin' <$>
      sequence
        [ (fmap (counterexample ("failure on timeout test #" ++ show n)))
          <$>
          singleTimeoutExperiment
            timeout
            schedulingTolerance
            intendedTimeoutDuration
            intendedActionDuration
        | ((intendedTimeoutDuration,
            intendedActionDuration), n) <- zip times [1 :: Int ..] ]
  where
    conjoin' :: [WithSanityCheck Property] -> Property
    conjoin' props =
           conjoin (ignoreSanityCheck `map` props)
      .&&. let numFailures = length (filter isSanityCheckIgnored props)
           in counterexample
               ("too many failures: " ++ show numFailures ++ " ≰ " ++ show maxFailures)
               (numFailures <= maxFailures)



--
-- Tests in Sim
--

-- In the simulator the timeouts should be perfect.
schedulingToleranceSim :: SchedulingTolerance
schedulingToleranceSim =
    SchedulingTolerance {
      negativeSchedulingTolerance = 0.0,
      positiveSchedulingTolerance = 0.0,
      maxFailures = 0
    }


prop_timeout_sim :: (forall s. WithTimeout (IOSim s))
                 -> TimeoutDuration -> ActionDuration -> Property
prop_timeout_sim withTimeout timeoutDuration actionDuration =
    either (\err -> counterexample (show err) False) id $ runSim $
      withSanityCheck <$>
        prop_timeout
          withTimeout schedulingToleranceSim
          timeoutDuration actionDuration

prop_timeouts_sim :: (forall s. WithTimeout (IOSim s))
                  -> [(TimeoutDuration, ActionDuration)] -> Property
prop_timeouts_sim withTimeout times =
    either (\err -> counterexample (show err) False) id $ runSim $
      prop_timeouts
        withTimeout schedulingToleranceSim
        times


--
-- Tests in IO
--

-- | GHC's timer for pre-emptive context switching is 20 milliseconds.
-- We allow for 100 milliseconds tolerance after a timer fires. Even this
-- is sometimes not enough if there are lots of concurrent CPU-bound threads.
--
-- It is also imperfect in the negative direction and can wake up a few
-- microseconds too early. We allow 100 microseconds tolerance.
--
schedulingToleranceIO :: SchedulingTolerance
schedulingToleranceIO =
    SchedulingTolerance {
      negativeSchedulingTolerance = 0.001,
      positiveSchedulingTolerance = 0.3,
      maxFailures                 = 5
    }


prop_timeout_io :: WithTimeout IO -> TimeoutDuration -> ActionDuration -> Property
prop_timeout_io withTimeout timeoutDuration actionDuration =
    ioProperty $
      withSanityCheck <$>
        prop_timeout
          withTimeout schedulingToleranceIO
          timeoutDuration actionDuration

prop_timeouts_io :: WithTimeout IO
                 -> [(TimeoutDuration, ActionDuration)] -> Property
prop_timeouts_io withTimeout times =
    ioProperty $
      prop_timeouts
        withTimeout schedulingToleranceIO
        times


--
-- Arbitrary instances
--


type TimeoutDuration = DiffTime
type ActionDuration  = DiffTime

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

counterexamples :: Testable t => [String] -> t -> Property
counterexamples []     p = property p
counterexamples (c:cs) p = counterexample c (counterexamples cs p)

