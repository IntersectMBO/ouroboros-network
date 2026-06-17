{-# LANGUAGE CPP #-}

module Main (main) where

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import Data.Function (on)
import Data.List (sortBy)
import Data.Monoid (Sum (..))
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, fromGregorian)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Data.Window.Count qualified as C
import Data.Window.Timed qualified as T


-- Use Int rather than Double for arithmetic invariants — the
-- finger-tree-cached sum and the naive list sum can disagree by
-- floating-point ULPs even though both are "correct".
type CountW = C.Window (Sum Int) (C.SumSample Int)
type TimedW = T.TimedWindow UTCTime (Sum Int) (T.SumSample Int)


newtype Cap = Cap Int deriving Show
instance Arbitrary Cap where
  arbitrary = Cap <$> choose (1, 32)

newtype Dur = Dur NominalDiffTime deriving Show
instance Arbitrary Dur where
  arbitrary = Dur . fromInteger <$> choose (1, 60)


-- Count ---------------------------------------------------------------------

-- insertMany agrees with repeated insert (orientation invariant).
prop_insertMany_eq_foldl :: Cap -> [Int] -> Property
prop_insertMany_eq_foldl (Cap n) xs =
  let w0 = C.empty n :: CountW
      w1 = foldl' (flip C.insert) w0 xs
      w2 = C.insertMany xs w0
  in  (C.toNewestFirst w1 :: [Int]) === C.toNewestFirst w2

-- size after inserting xs into a fresh window is min n (length xs).
prop_size_bounded :: Cap -> [Int] -> Property
prop_size_bounded (Cap n) xs =
  let w = C.insertMany xs (C.empty n :: CountW)
  in  C.size w === min n (length xs)

-- windowSum matches the live elements' sum.
prop_sum_consistent :: Cap -> [Int] -> Property
prop_sum_consistent (Cap n) xs =
  let w        = C.insertMany xs (C.empty n :: CountW)
      live     = C.toNewestFirst w :: [Int]
      expected = if null live then Nothing else Just (sum live)
  in  C.windowSum w === expected

-- fromFoldable (oldest-first input) gives the same window as the
-- equivalent sequence of inserts (regression for bug #1 from review).
prop_fromFoldable_orientation :: Cap -> [Int] -> Property
prop_fromFoldable_orientation (Cap n) xs =
  let w1 = C.fromFoldable n xs :: CountW
      w2 = foldl' (flip C.insert) (C.empty n :: CountW) xs
  in  (C.toNewestFirst w1 :: [Int]) === C.toNewestFirst w2

-- fromListN (newest-first input) is the reverse-input mirror of insert.
prop_fromListN_orientation :: Cap -> [Int] -> Property
prop_fromListN_orientation (Cap n) xs =
  let w1 = C.fromListN n (reverse xs) :: CountW
      w2 = foldl' (flip C.insert) (C.empty n :: CountW) xs
  in  (C.toNewestFirst w1 :: [Int]) === C.toNewestFirst w2

-- evictOldestN reduces the size as expected.
prop_evictOldestN :: Cap -> [Int] -> NonNegative Int -> Property
prop_evictOldestN (Cap n) xs (NonNegative k) =
  let w  = C.insertMany xs (C.empty n :: CountW)
      w' = C.evictOldestN k w
  in  C.size w' === max 0 (C.size w - k)


-- Timed ---------------------------------------------------------------------

-- evictBefore retains only samples with timestamp >= cutoff. Input is
-- sorted to honour the non-decreasing-timestamp invariant of insertMany.
prop_evictBefore :: Dur -> [(NonNegative Int, Int)] -> Int -> Property
prop_evictBefore (Dur d) tvs offset =
  let base   = UTCTime (fromGregorian 2024 1 1) 0
      sorted = sortBy (compare `on` fst) tvs
      pairs  = [ (fromIntegral s `addUTCTime` base, v)
               | (NonNegative s, v) <- sorted ]
      w      = T.insertMany pairs (T.empty d :: TimedW)
      cutoff = fromIntegral offset `addUTCTime` base
      w'     = T.evictBefore cutoff w
      kept   = T.toTimedNewestFirst w' :: [(UTCTime, Int)]
  in  conjoin [ counterexample (show t) (t >= cutoff) | (t, _) <- kept ]

-- windowDuration on a freshly-built window from a non-decreasing
-- timestamp list equals (newest - oldest), within the configured cap.
prop_windowDuration :: Dur -> [NonNegative Int] -> Property
prop_windowDuration (Dur d) ts0 =
  let base   = UTCTime (fromGregorian 2024 1 1) 0
      sorted = sortBy compare (map getNonNegative ts0)
      pairs  = [ (fromIntegral s `addUTCTime` base, 0 :: Int) | s <- sorted ]
      w      = T.insertMany pairs (T.empty d :: TimedW)
      live   = T.toTimedNewestFirst w :: [(UTCTime, Int)]
  in  case live of
        []   -> T.windowDuration w === Nothing
        x:xs ->
          let newest = fst x
              oldest = fst $ foldl (\_ x' -> x') x xs
              dur    = newest `diffUTCTime` oldest
          in  T.windowDuration w === Just dur
  where
    diffUTCTime a b = realToFrac (utcTimeDiff a b) :: NominalDiffTime
    utcTimeDiff a b =
      let toS (UTCTime _ d') = realToFrac d' :: Double
      in  toS a - toS b


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "window-stats"
    [ testGroup "Count"
        [ testProperty "insertMany ≡ foldl' insert" prop_insertMany_eq_foldl
        , testProperty "size = min cap (length xs)" prop_size_bounded
        , testProperty "windowSum = sum live"       prop_sum_consistent
        , testProperty "fromFoldable orientation"   prop_fromFoldable_orientation
        , testProperty "fromListN orientation"      prop_fromListN_orientation
        , testProperty "evictOldestN reduces size"  prop_evictOldestN
        ]
    , testGroup "Timed"
        [ testProperty "evictBefore keeps t >= cutoff" prop_evictBefore
        , testProperty "windowDuration = newest - oldest" prop_windowDuration
        ]
    ]
