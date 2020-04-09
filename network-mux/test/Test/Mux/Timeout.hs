{-# LANGUAGE RankNTypes #-}

module Test.Mux.Timeout ( tests ) where

import           Control.Monad (foldM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim

import           Data.Maybe (fromMaybe)
import           Data.Functor (($>))

-- TODO: import 'withTimeoutSerial' from `Netowork.Mux.Timeout`
-- import           Network.Mux.Timeout

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
    testGroup "Mux.Timeout"
    [ testProperty "timeout (Sim)"  (withDiffTime $ withDiffTime . prop_timeout_sim)
    , testProperty "timeout (IO)"   (withDiffTime $ withDiffTime . prop_timeout_io)
    , testProperty "timeouts (Sim)" (withDiffTimes 100 10   prop_timeouts_sim)
    , testProperty "timeouts (IO)"  (withDiffTimes 25  0.25 prop_timeouts_io)
    ]


-- TODO: change for the one from `Network.Mux.Timeout`
withTimeoutSerial :: MonadTimer m => ((forall a. DiffTime -> m a -> m (Maybe a)) -> m b) -> m b
withTimeoutSerial f = f timeout


--
-- generators & shrinkers / no arbitrary type classes :)
--

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

--
-- Properties
--


-- | Run timeout `threadDealy`; it either errors or returns @True@ if 
runSingleTimeoutExperiment
    :: ( MonadAsync m
       , MonadFork  m
       , MonadTimer m
       , MonadThrow m
       )
    => DiffTime
    -> DiffTime
    -> m (Maybe Bool)
runSingleTimeoutExperiment timeoutDiffTime delay =
    withTimeoutSerial $ \timeoutM ->
      timeoutM timeoutDiffTime (threadDelay delay $> (timeoutDiffTime >= delay))


prop_timeout_sim :: DiffTime -> DiffTime -> Bool
prop_timeout_sim timeoutDiffTime delay =
        case runSim $ runSingleTimeoutExperiment timeoutDiffTime delay of
          Left {}                   -> False
          Right (Just result)       -> result
          Right Nothing             -> timeoutDiffTime <= delay


prop_timeout_io :: DiffTime -> DiffTime -> Property
prop_timeout_io timeoutDiffTime delay =
    ioProperty $ do
      r <- runSingleTimeoutExperiment timeoutDiffTime delay
      -- in `IO`, unlike in `SimM` we cannot guarantee that even if
      -- `timeoutDiffTime < delay` the timeout fires before delay.
      -- We can get away here, only because we generate `Difftime`s which are
      -- multiple of `0.001`s, which gives @ghc@ enough time to actually fire
      -- the timeout.
      pure $ fromMaybe (timeoutDiffTime <= delay) r


runSeriesOfTimeoutsExperiment
    :: ( MonadAsync m
       , MonadFork  m
       , MonadTimer m
       , MonadThrow m
       )
    => [(DiffTime, DiffTime)]
    -> m Bool
runSeriesOfTimeoutsExperiment as =
    withTimeoutSerial $ \timeoutM ->
      foldM
        (\r x -> (r &&) <$> uncurry (singleExp timeoutM) x)
        True as
  where
    singleExp timeoutM timeoutDiffTime delay = do
      r <- timeoutM timeoutDiffTime (threadDelay delay $> (timeoutDiffTime >= delay))
      pure $ fromMaybe (timeoutDiffTime <= delay) r

prop_timeouts_sim :: [(DiffTime, DiffTime)] -> Bool
prop_timeouts_sim as =
    case runSim $ runSeriesOfTimeoutsExperiment as of
      Left {} -> False
      Right r -> r


prop_timeouts_io :: [(DiffTime, DiffTime)] -> Property
prop_timeouts_io as =
    ioProperty $
      -- the comment in `prop_timeout_io` applies here as well
      runSeriesOfTimeoutsExperiment as
