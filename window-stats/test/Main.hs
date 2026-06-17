{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}

module Main (main) where

#if !MIN_VERSION_base(4,20,0)
import Data.Foldable
#endif
import Data.List (sort, sortBy, tails)
import Data.Maybe (isNothing)
import Data.Monoid (Sum (..))
import Data.Ord (comparing)
import Data.Time (NominalDiffTime, UTCTime (..), addUTCTime, diffUTCTime,
                  fromGregorian)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Data.Window.Count qualified as C
import Data.Window.DigestTimeBatched qualified as DTB
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
-- equivalent sequence of inserts.
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
      sorted = sortBy (comparing fst) tvs
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
  let sorted = sort (map getNonNegative ts0)
      pairs  = [ (mkT (fromIntegral s), 0 :: Int) | s <- sorted ]
      w      = T.insertMany pairs (T.empty d :: TimedW)
      live   = T.toTimedNewestFirst w :: [(UTCTime, Int)]
  in  case live of
        []   -> T.windowDuration w === Nothing
        x:xs ->
          let newest = fst x
              oldest = fst $ foldl' (\_ x' -> x') x xs
              dur    = newest `diffUTC` oldest
          in  T.windowDuration w === Just dur
  where
    diffUTC a b = realToFrac (utcTimeDiff a b) :: NominalDiffTime
    utcTimeDiff a b =
      let toS (UTCTime _ d') = realToFrac d' :: Double
      in  toS a - toS b


-- DigestTimeBatched ---------------------------------------------------------

type Comp = 100
type DigestW = DTB.TimedDigestWindow UTCTime Comp

digestBase :: UTCTime
digestBase = UTCTime (fromGregorian 2024 1 1) 0

mkT :: Double -> UTCTime
mkT s = realToFrac s `addUTCTime` digestBase

-- Sorted timestamps uniformly within [0, spanSec], sample values in
-- [-100, 100].
genTimedSamplesIn
  :: Double            -- ^ maximum offset in seconds
  -> Gen [(UTCTime, Double)]
genTimedSamplesIn spanSec = do
  n       <- choose (10, 200)
  offsets <- sort <$> vectorOf n (choose (0, spanSec))
  values  <- vectorOf n (choose (-100, 100))
  return
    [ (mkT t, v) | (t, v) <- zip offsets values ]

-- Same, but all sample values in [0, 100] for the accuracy tests.
genPositiveSamplesIn
  :: Double
  -> Gen [(UTCTime, Double)]
genPositiveSamplesIn spanSec = do
  n       <- choose (100, 500)
  offsets <- sort <$> vectorOf n (choose (0, spanSec))
  values  <- vectorOf n (choose (0, 100))
  return
    [ (mkT t, v) | (t, v) <- zip offsets values ]

buildDigestW :: NominalDiffTime -> Int -> [(UTCTime, Double)] -> DigestW
buildDigestW bucketDur retention =
  foldl' (flip DTB.insert) (DTB.empty bucketDur retention)


-- Bug #1 (direct regression): a hand-picked sequence that spans two
-- bucket periods with multiple samples in the second period. Under
-- correct behaviour the open bucket accumulates the last three samples;
-- windowDuration reports 7. Under the bug, obFinish stays at 1 and
-- windowDuration reports 1.
--
unit_dtb_openBucketResetsOnSeal :: Property
unit_dtb_openBucketResetsOnSeal = once $
    let samples = [ (mkT 0, 0), (mkT 1, 1) -- bucket [0, 2)
                  , (mkT 2, 2), (mkT 3, 3) -- bucket [2, 4)
                  , (mkT 5, 5)             -- evicts first bucket
                  ]
        w = buildDigestW 2 2 samples
    in      DTB.sampleCount w    === 3
       .&&. DTB.windowDuration w === Just (4 :: NominalDiffTime)


-- Empty window semantics.
unit_dtb_emptySemantics :: Property
unit_dtb_emptySemantics = once $
    let w = DTB.empty (1 :: NominalDiffTime) 60 :: DigestW
    in     DTB.sampleCount w    === 0
      .&&. counterexample "windowDigest"   (isNothing (DTB.windowDigest   w))
      .&&. DTB.windowDuration w === Nothing
      .&&. DTB.windowMedian w   === Nothing
      .&&. DTB.windowQuantile 0.99 w === Nothing


-- reset returns an empty window.
prop_dtb_resetEmpties :: Property
prop_dtb_resetEmpties =
  forAll (genTimedSamplesIn 100.0) $ \samples ->
    let w  = buildDigestW 1 60 samples
        w' = DTB.reset w
    in     DTB.sampleCount w'  === 0
      .&&. counterexample "windowDigest after reset"
             (isNothing (DTB.windowDigest w'))


-- sampleCount counts every inserted sample when nothing has been
-- evicted (all samples fit within the window duration).
prop_dtb_sampleCountConservation :: Property
prop_dtb_sampleCountConservation =
  forAll (genTimedSamplesIn 3.0) $ \samples ->
    -- bucket_dur = 1s, retention = 5 → window = 5s ; all samples ≤ 3s
    let w = buildDigestW 1.0 5 samples
    in DTB.sampleCount w === length samples


-- A quantile query lies within [min, max] of the input samples that
-- are still in the window.
prop_dtb_quantileInRange :: Property
prop_dtb_quantileInRange =
  forAll (genTimedSamplesIn 3.0) $ \samples ->
    not (null samples) ==>
      let w      = buildDigestW 1.0 5 samples
          values = map snd samples
          lo     = minimum values
          hi     = maximum values
      in case DTB.windowMedian w of
           Nothing -> counterexample "no median for non-empty window" False
           Just m  -> counterexample
                        (show m ++ " outside [" ++ show lo ++ ", " ++ show hi ++ "]")
                        (m >= lo && m <= hi)


-- The batched digest's median is within a tolerance of the exact
-- sample median, for reasonably-sized uniform inputs that all fit in
-- the window.
prop_dtb_quantileAccuracy :: Property
prop_dtb_quantileAccuracy =
  forAll (genPositiveSamplesIn 30.0) $ \samples ->
    let w = buildDigestW 1.0 60 samples
        values = map snd samples
        n = length values
        sorted = sort values
        exactMedian = if odd n
                      then sorted !! (n `div` 2)
                      else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2
    in case DTB.windowMedian w of
         Nothing -> counterexample "no median for non-empty window" False
         Just m  -> counterexample
                      ("batched " ++ show m ++ " vs exact " ++ show exactMedian)
                      (abs (m - exactMedian) <= 5.0)   -- generous


-- Eviction correctness: after inserting samples spread over
-- [0, 6 × windowDur], the sample count should be at most the count of
-- samples whose timestamps fall within the last windowDur (plus two
-- buckets' worth of slack).
prop_dtb_evictionRetainsOnlyRecent :: Property
prop_dtb_evictionRetainsOnlyRecent =
  forAll gen $ \samples ->
    not (null samples) ==>
      let bucketDur   = 1.0 :: NominalDiffTime
          retention   = 5
          windowDur   = fromIntegral retention * bucketDur
          w           = buildDigestW bucketDur retention samples
          lastT       = maximum (map fst samples)
          -- Two bucket durations of slack: eviction only runs on seal
          -- events, so the last seal may have fired up to bucketDur before
          -- lastT, and a retained bucket's oldest sample can be up to
          -- bucketDur older than its sbFinish.
          cutoff      = negate (windowDur + 2 * bucketDur) `addUTCTime` lastT
          liveCount   = length (filter (\(t, _) -> t >= cutoff) samples)
      in counterexample (show (DTB.sampleCount w, liveCount))
           (DTB.sampleCount w <= liveCount)
  where
    gen = do
      n       <- choose (50, 400)
      offsets <- sort <$> vectorOf n (choose (0, 30))  -- 30s span, window 5s
      values  <- vectorOf n (choose (-100, 100))
      return [ (mkT t, v) | (t, v) <- zip offsets values ]


-- windowDuration reports a value bounded below by the actual sample
-- duration (newest - oldest) and above by that duration plus one
-- bucket period, per its haddock. The upper slack is the range of
-- possible obStart positions for the leftmost sealed bucket (whose
-- obStart is not tracked; the implementation approximates it as
-- sbFinish - bucketDur). Samples span less than the window so no
-- eviction narrows the "actual" duration.
prop_dtb_windowDurationApproximate :: Property
prop_dtb_windowDurationApproximate =
  forAll (genTimedSamplesIn 30.0) $ \samples ->
    not (null samples) ==>
      let bucketDur = 1.0 :: NominalDiffTime
          retention = 60          -- window = 60s > spanSec = 30s
          w         = buildDigestW bucketDur retention samples
          times     = map fst samples
          actual    = maximum times `diffUTCTime` minimum times
      in case DTB.windowDuration w of
           Nothing -> counterexample "unexpected Nothing for non-empty window" False
           Just d  ->
             counterexample
               (show d ++ " outside [" ++ show actual ++
                ", " ++ show (actual + bucketDur) ++ "]")
               (d >= actual .&&. d <= actual + bucketDur)
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
    , testGroup "DigestTimeBatched"
        [ testProperty "open bucket resets on seal (bug #1 regression)"
                       unit_dtb_openBucketResetsOnSeal
        , testProperty "empty window has trivial statistics"
                       unit_dtb_emptySemantics
        , testProperty "reset empties the window"
                       prop_dtb_resetEmpties
        , testProperty "sampleCount conserves inserts (no eviction)"
                       prop_dtb_sampleCountConservation
        , testProperty "median lies in [min, max] of samples"
                       prop_dtb_quantileInRange
        , testProperty "median accuracy vs exact"
                       prop_dtb_quantileAccuracy
        , testProperty "eviction retains only recent samples"
                       prop_dtb_evictionRetainsOnlyRecent
        , testProperty "windowDuration is bounded [actual, actual + bucketDur]"
                       prop_dtb_windowDurationApproximate
        ]
    ]
