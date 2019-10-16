{-# LANGUAGE FlexibleContexts #-}

module ChannelTests (tests) where

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime
import Control.Monad.IOSim

import Network.TypedProtocol.Channel

import Data.Time.Clock (diffTimeToPicoseconds, picosecondsToDiffTime)
import System.Random

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests =
  testGroup "Channel"
  [ testProperty "createConnectedDelayChannels" prop_createConnectedDelayChannels
  ]


prop_createConnectedDelayChannels
  :: Positive Int
  -> (Positive Int, Positive Int, Positive Int)
  -> Int
  -> [Int] -> Property
prop_createConnectedDelayChannels
    (Positive maxsize) (Positive g_ms, Positive s_ms, Positive v_ms)
    seed xs =

      expectedDelayChannelTimes maxsize (g, s, v) prng xs
  === actualDelayChannelTimes   maxsize (g, s, v) prng xs
  where
    prng = mkStdGen seed

    g = millisecondsToDiffTime g_ms
    s = millisecondsToDiffTime s_ms
    v = millisecondsToDiffTime v_ms

    millisecondsToDiffTime :: Int -> DiffTime
    millisecondsToDiffTime = (/1e3) . fromIntegral

actualDelayChannelTimes
  :: RandomGen g
  => Int
  -> (DiffTime, DiffTime, DiffTime)
  -> g
  -> [b]
  -> [((VTime, b), (VTime, b))]
actualDelayChannelTimes maxsize gsv prng xs0 =
    runSimOrThrow $ do
      (chanA, chanB) <- createConnectedBufferedChannels
                          (const 1) maxsize gsv prng

      sa <- async (sender chanA xs0)
      ra <- async (receiver chanB [] (length xs0))
      uncurry zip <$> waitBoth sa ra
  where
    sender chan xs =
      sequence
        [ do send chan x
             now <- getMonotonicTime
             return (now,x)
        | x <- xs ]

    receiver _    xs 0 = return (reverse xs)
    receiver chan xs n = do
      Just x <- recv chan
      now <- getMonotonicTime
      receiver chan ((now,x):xs) (n-1)


expectedDelayChannelTimes
  :: RandomGen g
  => Int
  -> (DiffTime, DiffTime, DiffTime)
  -> g
  -> [b]
  -> [((VTime, b), (VTime, b))]
expectedDelayChannelTimes maxsize (g, s, v) prng0 xs0 =
    let (prngA, _prngB) = split prng0
        vsamples :: [DiffTime]
        vsamples = map picosecondsToDiffTime
                       (randomRs (0, diffTimeToPicoseconds v) prngA)
     in go (VTime 0) (VTime 0) (queue []) vsamples xs0
  where

    go :: VTime -> VTime -> Q VTime -> [DiffTime] -> [b] -> [((VTime, b), (VTime, b))]
    go _ _ _ _ [] = []

    go now maxarrive arrivals vsamples (x:xs)
      | queuelen arrivals == maxsize
      , Just (arrival, arrivals') <- dequeue arrivals
      = to (max now arrival) maxarrive arrivals' vsamples x xs

      | otherwise
      = to now maxarrive arrivals vsamples x xs

    to now maxarrive arrivals (vsample:vsamples) x xs =
        ((depart, x), (arrive, x))
      : go depart arrive (enqueue arrivals arrive) vsamples xs
      where
        depart = s `addTime` now
        arrive = max maxarrive ((g + vsample) `addTime` depart)
        -- cannot have the next message arrive before the last previous arrival

    to _ _ _ [] _ _ = error "expectedDelayChannelTimes: randomRs is infinite"



----------------
-- Queue
--

data Q a = Q [a] [a]

queue :: [a] -> Q a
queue xs = Q xs []

enqueue :: Q a -> a -> Q a
enqueue (Q front  back) x = Q front (x : back)

dequeue :: Q a -> Maybe (a, Q a)
dequeue (Q (x:xs) back)    = Just (x, Q xs back)
dequeue (Q []     back)    = case reverse back of
                               x:xs -> Just (x, Q xs [])
                               []   -> Nothing

queuelen :: Q a -> Int
queuelen (Q front back) = length front + length back

