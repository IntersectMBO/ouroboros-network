{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Test.Mux.Timeout ( tests ) where

import           Control.Monad (foldM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Concurrent (yield)

import           Data.Maybe (fromMaybe)
import           Data.Monoid (All (..), Sum (..))
import           Data.Functor (($>))

import           Network.Mux.Timeout (TimeoutFn,
                                      withTimeoutSerialNative,
                                      withTimeoutSerialAlternative)

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
    testGroup "Mux.Timeout"
    [ testGroup "single timeout"
      [ testProperty "timeout (Sim)"
          (withDiffTime $ withDiffTime . (prop_timeout_sim withTimeoutSerialNative))
      , testProperty "withTimeoutSerialAlternative (Sim)"
          (withDiffTime $ withDiffTime . (prop_timeout_sim withTimeoutSerialAlternative))
      , testProperty "timeout (IO)"
          (withDiffTime $ withDiffTime . (prop_timeout_io withTimeoutSerialNative))
      , testProperty "withTimeoutSerial (IO)"
          (withDiffTime $ withDiffTime . (prop_timeout_io withTimeoutSerialAlternative))
      ]
    , testGroup "series of timeouts"
      [ testProperty "timeout (Sim)"
          (withDiffTimes 100 10 (prop_timeouts_sim withTimeoutSerialNative))

--     Test failure: issue #1943
--    , testProperty "withTimeoutSerialAlternative (Sim)"
--        (withDiffTimes 100 10 (prop_timeouts_sim withTimeoutSerialAlternative))

      , testProperty "timeout (IO)"
          (withDiffTimes 25 0.25 (prop_timeouts_io withTimeoutSerialNative))
      , testProperty "withTimeoutSerialAlternative (IO)"
          (withDiffTimes 25  0.25 (prop_timeouts_io withTimeoutSerialAlternative))
      ]
    ]


type WithTimeout m b
    = ( MonadAsync m
      , MonadFork  m
      , MonadTime  m
      , MonadTimer m
      , MonadMask  m
      , MonadThrow (STM m)
      )
    => (TimeoutFn m -> m b) -> m b


--
-- Properties
--

-- | Run timeout `threadDealy`; it either errors or returns @True@ if
runSingleTimeoutExperiment
    :: ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadSTM   m
       , MonadTime  m
       , MonadTimer m
       , MonadThrow m
       , MonadThrow (STM m)
       )
    => WithTimeout m (Maybe Bool)
    -> DiffTime
    -> DiffTime
    -> m (Maybe Bool)
runSingleTimeoutExperiment withTimeoutImpl timeoutDiffTime delay =
    withTimeoutImpl $ \timeoutM ->
      timeoutM timeoutDiffTime (threadDelay delay $> (timeoutDiffTime >= delay))


prop_timeout_sim :: (forall s. WithTimeout (SimM s) (Maybe Bool)) ->  DiffTime -> DiffTime -> Bool
prop_timeout_sim withTimeoutImpl timeoutDiffTime delay =
        case runSim $ runSingleTimeoutExperiment withTimeoutImpl timeoutDiffTime delay of
          Left {}             -> False
          Right (Just result) -> result
          Right Nothing       -> timeoutDiffTime <= delay


prop_timeout_io :: WithTimeout IO (Maybe Bool) -> DiffTime -> DiffTime -> Property
prop_timeout_io withTimeoutImpl timeoutDiffTime delay =
    ioProperty $ do
      r <- runSingleTimeoutExperiment withTimeoutImpl timeoutDiffTime delay
      -- in `IO`, unlike in `SimM` we cannot guarantee that even if
      -- `timeoutDiffTime < delay` the timeout fires before delay.
      -- We can get away here, only because we generate `Difftime`s which are
      -- multiple of `0.001`s, which gives @ghc@ enough time to actually fire
      -- the timeout.
      pure $ fromMaybe (timeoutDiffTime <= delay) r


runSeriesOfTimeoutsExperiment
    :: ( MonadAsync m
       , MonadFork  m
       , MonadMask  m
       , MonadSTM   m
       , MonadTime  m
       , MonadTimer m
       , MonadThrow m
       , MonadThrow (STM m)
       )
    => WithTimeout m [Bool]
    -> m ()
    -> [(DiffTime, DiffTime)]
    -> m [Bool]
runSeriesOfTimeoutsExperiment withTimeoutImpl yield_ as =
    withTimeoutImpl $ \timeoutM ->
      reverse <$>
        foldM
          (\xs x -> do
            x' <- uncurry (singleExp timeoutM) x
            yield_
            pure (x' : xs))
          [] as
  where
    singleExp timeoutM timeoutDiffTime delay = do
      r <- timeoutM timeoutDiffTime (threadDelay delay $> (timeoutDiffTime >= delay))
      pure $ fromMaybe True r


prop_timeouts_sim :: (forall s. WithTimeout (SimM s) [Bool]) -> [(DiffTime, DiffTime)] -> Property
prop_timeouts_sim withTimeoutImpl as =
    case runSim $ runSeriesOfTimeoutsExperiment withTimeoutImpl (pure ()) as of
      Left err -> counterexample (show err) False
      Right r  -> counterexample (show r) $ all id r


-- NOTE: for `withTimeoutSerialAlternative`: without passing `yield` or
-- `threadDelay 1`, this test fails! issue#1994
prop_timeouts_io :: WithTimeout IO [Bool] -> [(DiffTime, DiffTime)] -> Property
prop_timeouts_io withTimeoutImpl as =
    ioProperty $ do
      -- the comment in `prop_timeout_io` applies here as well
      rs <- runSeriesOfTimeoutsExperiment withTimeoutImpl yield as
      let rs' = zip rs as
          -- test result, and number of cases in which we had to ignore the
          testResult :: Bool; s :: Int;
          (All testResult, Sum s) =
            foldMap (\(r, (timeoutDiffTime, delay)) ->
                      if timeoutDiffTime <= delay
                        then (All True, Sum (if r then 0 else 1))
                        else (All r, Sum 0)) rs'
      pure $ label ("number of ignored cases: " ++ show s) testResult


-- quickcheck test failure: issue #1943
-- tr :: Trace
-- tr = runSimTrace runSeriesOfTimeoutsExperiment withTimeoutSerialAlternative [(0.005,0), (0.001, 0.005)]


--
-- generators & shrinkers / no arbitrary type classes :)
--

-- run tests feeding it with data generated using `genDiffTime` and shrinked
-- with `shrinkDiffTime`
--
-- NOTE: in `Aribrary`less approach we need to write combinators for running
-- tests.
withDiffTime :: Testable prop => (DiffTime -> prop) -> Property
withDiffTime = forAllShrink (genDiffTime 100) shrinkDiffTime

withDiffTimes :: Testable prop
              => Int      -- max list size
              -> DiffTime -- max delay
              -> ([(DiffTime, DiffTime)] -> prop) -> Property
withDiffTimes size maxDelay = forAllShrink g s
  where
    g :: Gen [(DiffTime, DiffTime)]
    g = do
      k <- choose (0, size)
      suchThat (vectorOf k $ ((,) <$> (genDiffTime 10) <*> (genDiffTime 10)))
               (\as -> sum (map fst as) <= maxDelay)

    s :: [(DiffTime, DiffTime)] -> [[(DiffTime, DiffTime)]]
    s = shrinkList $ \(a, b) ->
      [ (a', b)
      | a' <- shrinkDiffTime a
      ] ++
      [ (a, b')
      | b' <- shrinkDiffTime b
      ]

genDiffTime :: Int -- max number of milliseconds
            -> Gen DiffTime
genDiffTime size = fromRational <$> gen
  where
    -- generatare rationals in the range `0` up to `0.1` with granurality `0.001`.
    gen :: Gen Rational
    gen =
      ((/1000) . fromIntegral)
        <$> frequency
          [ (4, choose (0 , size `div` 2))
          , (1, choose (size `div` 2, size))
          ]

shrinkDiffTime :: DiffTime -> [DiffTime]
shrinkDiffTime =
    map (fromRational . getNonNegative)
  . shrink
  . NonNegative
  . toRational
