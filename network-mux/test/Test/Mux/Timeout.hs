{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Test.Mux.Timeout (tests) where

import Control.Concurrent.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI hiding (timeout)
import Control.Monad.Class.MonadUnique (MonadUnique)
import Control.Monad.IOSim

import Network.Mux.Time (microsecondsToDiffTime)
import Network.Mux.Timeout (TimeoutFn, withTimeoutSerialAlternative,
           withTimeoutSerialNative)

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


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

      , testProperty "prop_caught_action_exception_sim withTimeoutSerialNative" $
          prop_caught_action_exception_sim withTimeoutSerialNative

      , testProperty "prop_caught_action_exception_sim withTimeoutSerialAlternative" $
          prop_caught_action_exception_sim withTimeoutSerialAlternative

      , testProperty "prop_caught_action_exception_reuse_sim withTimeoutSerialNative" $
          prop_caught_action_exception_reuse_sim withTimeoutSerialNative

      , testProperty "prop_caught_action_exception_reuse_sim withTimeoutSerialAlternative" $
          prop_caught_action_exception_reuse_sim withTimeoutSerialAlternative

      , testProperty "prop_caught_async_exception_sim withTimeoutSerialNative" $
          prop_caught_async_exception_sim withTimeoutSerialNative

      , testProperty "prop_caught_async_exception_sim withTimeoutSerialAlternative" $
          prop_caught_async_exception_sim withTimeoutSerialAlternative

      , testProperty "prop_outer_timeout_sim withTimeoutSerialNative" $
          prop_outer_timeout_sim withTimeoutSerialNative

      , testProperty "prop_outer_timeout_sim withTimeoutSerialAlternative" $
          prop_outer_timeout_sim withTimeoutSerialAlternative

      , testProperty "prop_inner_timeout_sim withTimeoutSerialNative" $
          prop_inner_timeout_sim withTimeoutSerialNative

      , testProperty "prop_inner_timeout_sim withTimeoutSerialAlternative" $
          prop_inner_timeout_sim withTimeoutSerialAlternative

        -- The native timeout in io-sim-1.8.0.1 delivers a stale exception
        -- at the deadline; its Sim test is omitted until that is fixed.
      , testProperty "prop_exception_at_deadline_sim withTimeoutSerialAlternative" $
          prop_exception_at_deadline_sim withTimeoutSerialAlternative
      ]

    , testGroup "SimPOR"
      [ testProperty "prop_caught_action_exception_simpor withTimeoutSerialNative" $
          prop_caught_action_exception_simpor withTimeoutSerialNative

      , testProperty "prop_caught_action_exception_simpor withTimeoutSerialAlternative" $
          prop_caught_action_exception_simpor withTimeoutSerialAlternative

      , testProperty "prop_caught_action_exception_reuse_simpor withTimeoutSerialNative" $
          prop_caught_action_exception_reuse_simpor withTimeoutSerialNative

      , testProperty "prop_caught_action_exception_reuse_simpor withTimeoutSerialAlternative" $
          prop_caught_action_exception_reuse_simpor withTimeoutSerialAlternative

      , testProperty "prop_caught_async_exception_simpor withTimeoutSerialNative" $
          prop_caught_async_exception_simpor withTimeoutSerialNative

      , testProperty "prop_caught_async_exception_simpor withTimeoutSerialAlternative" $
          prop_caught_async_exception_simpor withTimeoutSerialAlternative

      , testProperty "prop_outer_timeout_simpor withTimeoutSerialNative" $
          prop_outer_timeout_simpor withTimeoutSerialNative

      , testProperty "prop_outer_timeout_simpor withTimeoutSerialAlternative" $
          prop_outer_timeout_simpor withTimeoutSerialAlternative

      , testProperty "prop_inner_timeout_simpor withTimeoutSerialNative" $
          prop_inner_timeout_simpor withTimeoutSerialNative

      , testProperty "prop_inner_timeout_simpor withTimeoutSerialAlternative" $
          prop_inner_timeout_simpor withTimeoutSerialAlternative

      , testProperty "prop_exception_at_deadline_simpor withTimeoutSerialNative" $
          prop_exception_at_deadline_simpor withTimeoutSerialNative

      , testProperty "prop_exception_at_deadline_simpor withTimeoutSerialAlternative" $
          prop_exception_at_deadline_simpor withTimeoutSerialAlternative
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

      , testProperty "prop_caught_action_exception_io withTimeoutSerialNative" $
          prop_caught_action_exception_io withTimeoutSerialNative

      , testProperty "prop_caught_action_exception_io withTimeoutSerialAlternative" $
          prop_caught_action_exception_io withTimeoutSerialAlternative

      , testProperty "prop_caught_async_exception_io withTimeoutSerialNative" $
          prop_caught_async_exception_io withTimeoutSerialNative

      , testProperty "prop_caught_async_exception_io withTimeoutSerialAlternative" $
          prop_caught_async_exception_io withTimeoutSerialAlternative

      , testProperty "prop_outer_timeout_io withTimeoutSerialNative" $
          prop_outer_timeout_io withTimeoutSerialNative

      , testProperty "prop_outer_timeout_io withTimeoutSerialAlternative" $
          prop_outer_timeout_io withTimeoutSerialAlternative

      , testProperty "prop_inner_timeout_io withTimeoutSerialNative" $
          prop_inner_timeout_io withTimeoutSerialNative

      , testProperty "prop_inner_timeout_io withTimeoutSerialAlternative" $
          prop_inner_timeout_io withTimeoutSerialAlternative

      , testProperty "prop_exception_at_deadline_io withTimeoutSerialNative" $
          prop_exception_at_deadline_io withTimeoutSerialNative

      , testProperty "prop_exception_at_deadline_io withTimeoutSerialAlternative" $
          prop_exception_at_deadline_io withTimeoutSerialAlternative

      , testProperty "prop_exception_during_delivery_io withTimeoutSerialNative" $
          prop_exception_during_delivery_io withTimeoutSerialNative

      , testProperty "prop_exception_during_delivery_io withTimeoutSerialAlternative" $
          prop_exception_during_delivery_io withTimeoutSerialAlternative
      ]
    ]


data ActionException = ActionException
  deriving (Eq, Show)

instance Exception ActionException


-- Check exception cleanup independently of timing tolerances.
-- Keeping the scope alive after an action throws makes a stale timeout visible.
caughtActionException :: forall m. TimeoutConstraints m => WithTimeout m -> m Property
caughtActionException withTimeout =
    withTimeout $ \timeout -> do
      result <- try $ timeout 0.05 $ do
        threadDelay 0.005
        throwIO ActionException :: m ()
      threadDelay 0.1
      next <- timeout 0.01 $ atomically retry
      return $ conjoin [result === Left ActionException, next === (Nothing :: Maybe ())]


-- Reuse the timeout immediately after an action throws. The next action spans
-- the old deadline, so a stale timeout must not interrupt it.
caughtActionExceptionReuse :: forall m. TimeoutConstraints m => WithTimeout m -> m Property
caughtActionExceptionReuse withTimeout =
    withTimeout $ \timeout -> do
      result <- try $ timeout 0.05 $ do
        threadDelay 0.005
        throwIO ActionException :: m ()
      next <- timeout 0.1 $ threadDelay 0.075
      return $ conjoin [result === Left ActionException, next === Just ()]


caughtAsyncException :: forall m. TimeoutConstraints m => WithTimeout m -> m Property
caughtAsyncException withTimeout =
    withTimeout $ \timeout -> do
      tid <- myThreadId
      started <- newEmptyTMVarIO
      withAsync (do
        atomically $ takeTMVar started
        threadDelay 0.005
        throwTo tid ActionException) $ \_ -> do
          result <- try $ timeout 0.05 $ do
            atomically $ putTMVar started ()
            atomically retry :: m ()
          threadDelay 0.1
          return $ result === Left ActionException


-- Independent scopes must handle only their own timeout exceptions.
nestedTimeouts :: TimeoutConstraints m
               => WithTimeout m
               -> DiffTime
               -- ^ outer delay
               -> DiffTime
               -- ^ inner delay
               -> Maybe (Maybe ())
               -- ^ expected result
               -> m Property
nestedTimeouts withTimeout outerDelay innerDelay expected =
    withTimeout $ \outer ->
      withTimeout $ \inner -> do
        result <- outer outerDelay $ inner innerDelay $ atomically retry
        return $ result === expected


-- The action can throw just as the monitor starts sending its exception.
-- Either result is valid, but delivery must be cancelled before leaving timeout.
exceptionAtDeadline :: forall m. TimeoutConstraints m => WithTimeout m -> m Property
exceptionAtDeadline withTimeout =
    withTimeout $ \timeout -> do
      result <- try $ timeout 1 $ do
        threadDelay 1
        throwIO ActionException :: m ()
      threadDelay 2
      return $ case result of
        Left ActionException -> property True
        Right Nothing        -> property True
        Right (Just ())      -> counterexample "action returned" False


-- The monitor starts delivery while the action cannot receive the exception.
-- Cleanup must interrupt the blocked throwTo when the action throws instead.
-- This runs in IO: both native and alternative timeouts deliver a stale
-- exception in IOSim and IOSimPOR with io-sim-1.8.0.1.
exceptionDuringDelivery :: forall m. TimeoutConstraints m => WithTimeout m -> m Property
exceptionDuringDelivery withTimeout =
    withTimeout $ \timeout -> do
      result <- try $ timeout 0.01 $ uninterruptibleMask_ $ do
        threadDelay 0.05
        throwIO ActionException :: m ()
      threadDelay 0.02
      return $ result === Left ActionException


type TimeoutConstraints m =
      ( MonadAsync m
      , MonadDelay m
      , MonadFork  m
      , MonadTime  m
      , MonadTimer m
      , MonadUnique m
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
-- Tests in IOSim
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


prop_caught_action_exception_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_action_exception_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      caughtActionException withTimeout


prop_caught_action_exception_reuse_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_action_exception_reuse_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      caughtActionExceptionReuse withTimeout


prop_caught_async_exception_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_async_exception_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      caughtAsyncException withTimeout


prop_outer_timeout_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_outer_timeout_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      nestedTimeouts withTimeout 0.01 0.1 Nothing


prop_inner_timeout_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_inner_timeout_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      nestedTimeouts withTimeout 0.1 0.01 (Just Nothing)


prop_exception_at_deadline_sim :: (forall s. WithTimeout (IOSim s)) -> Property
prop_exception_at_deadline_sim withTimeout =
    once $ either (\err -> counterexample (show err) False) id $ runSim $
      exceptionAtDeadline withTimeout


--
-- Tests in IOSimPOR
--

prop_caught_action_exception_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_action_exception_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      caughtActionException withTimeout) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


prop_caught_action_exception_reuse_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_action_exception_reuse_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      caughtActionExceptionReuse withTimeout) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


prop_caught_async_exception_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_caught_async_exception_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      caughtAsyncException withTimeout) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


prop_outer_timeout_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_outer_timeout_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      nestedTimeouts withTimeout 0.01 0.1 Nothing) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


prop_inner_timeout_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_inner_timeout_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      nestedTimeouts withTimeout 0.1 0.01 (Just Nothing)) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


prop_exception_at_deadline_simpor :: (forall s. WithTimeout (IOSim s)) -> Property
prop_exception_at_deadline_simpor withTimeout =
    once $ exploreSimTrace (withScheduleBound 100) (do
      exploreRaces
      exceptionAtDeadline withTimeout) $ \_ trace ->
        either (\err -> counterexample (show err) False) id $
          traceResult False trace


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


prop_caught_action_exception_io :: WithTimeout IO -> Property
prop_caught_action_exception_io withTimeout =
    once $ ioProperty $ caughtActionException withTimeout


prop_caught_async_exception_io :: WithTimeout IO -> Property
prop_caught_async_exception_io withTimeout =
    once $ ioProperty $ caughtAsyncException withTimeout


prop_outer_timeout_io :: WithTimeout IO -> Property
prop_outer_timeout_io withTimeout =
    once $ ioProperty $ nestedTimeouts withTimeout 0.01 0.1 Nothing


prop_inner_timeout_io :: WithTimeout IO -> Property
prop_inner_timeout_io withTimeout =
    once $ ioProperty $ nestedTimeouts withTimeout 0.1 0.01 (Just Nothing)


prop_exception_at_deadline_io :: WithTimeout IO -> Property
prop_exception_at_deadline_io withTimeout =
    once $ ioProperty $ exceptionAtDeadline withTimeout


prop_exception_during_delivery_io :: WithTimeout IO -> Property
prop_exception_during_delivery_io withTimeout =
    once $ ioProperty $ exceptionDuringDelivery withTimeout


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

