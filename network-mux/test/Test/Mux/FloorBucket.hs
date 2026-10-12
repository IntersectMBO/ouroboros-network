{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | The floor's guarantee at the bucket, in IOSim: bearers are threads asking
-- the budget for batches; the oracle is the simulated clock and arithmetic
-- on the parameters, never the bucket.
--
module Test.Mux.FloorBucket (tests) where

import Control.Applicative ((<|>))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, forever, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadThrow (MonadMask, mask_)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim
import Control.Monad.IOSim qualified as IOSim

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (foldl', sortOn, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Word (Word8)

import Network.Mux.Counters qualified as Counters
import Network.Mux.Egress.Bucket (GrantSource (..))
import Network.Mux.Egress.Bucket qualified as Bucket
import Network.Mux.Egress.Burst qualified as Burst
import Network.Mux.Egress.Floor (FloorClass, FloorState, floorClass)
import Network.Mux.Types (MiniProtocolDir, MiniProtocolNum)
import NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..), noThunks,
           unsafeNoThunks)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.Read (readMaybe)

tests :: TestTree
tests =
  testGroup "Network.Mux.Egress.Bucket floor"
    [ testProperty "the budget stays the ceiling"          prop_ceiling
    , testProperty "only credited tiers use the floor"    prop_exclusion
    , testProperty "each class gets its share in bytes"   prop_class_share
    , testProperty "turns within a class differ by one"   prop_class_turns
    , testProperty "every floor wait is bounded"          prop_bounded_wait
    , testProperty "same tier: nobody starves under rotation" prop_same_tier
    , testProperty "an idle floor changes nothing"        prop_idle_floor
    , testProperty "counters match the grants"            prop_accounting
    , testProperty "the counters hold no thunks"          prop_stats_no_thunks
    , testProperty "in IO, the state holds no thunks"     prop_state_no_thunks_io
    , testProperty "the floor follows the budget's rate"  prop_floor_follows_rate
    , testProperty "the rest tier takes turns"            prop_rest_turns
    , testProperty "a rest bearer waits at most a round"  prop_rest_wait
    , testProperty "a credited tier is served head first" prop_credited_head_first
    , testProperty "a queued bearer follows its rule"      prop_rule_followed_while_queued
    , testProperty "no timer outlives its request"           prop_timers_follow_requests
    , testProperty "a shared credit is spent once, by whole batches" prop_shared_credit_spent_once
    , testProperty "the fast path charges the rank its rule answers now" prop_fast_path_charges_current_rank
    , testProperty "the bursts are the fold of the grants" prop_bursts_match_grants
    , testProperty "rest bearers alone open no burst"      prop_rest_alone_no_burst
    , testProperty "a cancellation after the grant leaves the counts whole" prop_cancel_after_grant
    , testProperty "the drain sweeps the burst in progress" prop_drain_sweeps
    ]

--
-- Setup
--

-- | A bearer: its tier, the batch it asks for, and when it starts. Every
-- bearer is backlogged: it asks again the moment it is granted.
data FBearer = FBearer {
    fbTier  :: !Word8,
    fbBatch :: !Int,
    fbStart :: !DiffTime
  }
  deriving Show

data FloorRun = FloorRun {
    frRate     :: !Double,      -- ^ budget, bytes/s
    frCapacity :: !Int,         -- ^ budget and floor capacity
    frPercent  :: !Int,         -- ^ the floor's share, percent
    frDuration :: !DiffTime,    -- ^ how long bearers run after the last start
    frRotation :: !(Maybe Bucket.Rotation),
    frBearers  :: ![FBearer]
  }
  deriving Show

floorRate :: FloorRun -> Double
floorRate FloorRun { frRate, frPercent } = frRate * fromIntegral frPercent / 100

lastStart :: FloorRun -> DiffTime
lastStart FloorRun { frBearers } = maximum (0 : map fbStart frBearers)

endTime :: FloorRun -> Time
endTime fr = Time (lastStart fr + frDuration fr)

maxBatch :: FloorRun -> Int
maxBatch FloorRun { frBearers } = maximum (1 : map fbBatch frBearers)

-- | Batches relative to the capacity: control, a chunk, bulk.
genBatch :: Int -> Gen Int
genBatch cap = frequency [ (4, choose (256, max 256 (cap `div` 32)))
                         , (2, choose (256, max 256 (cap `div` 4)))
                         , (1, choose (256, cap)) ]

-- | Strictly increasing start offsets: no two bearers start at once.
genStarts :: Int -> Gen [DiffTime]
genStarts n = do
  gaps <- vectorOf n (choose (1, 50 :: Int))
  return (map (\g -> fromIntegral g / 1000) (scanl1 (+) gaps))

-- | Saturators: bearers each asking for a whole capacity at a time, so one
-- grant empties the budget and the next saturator is always queued; the
-- budget's queue is then never empty. Half the runs they are local roots and
-- no credited bearer reaches the budget; in the rest they are of a credited
-- tier the run allows, so the budget's head is often a floor member too.
genSaturators :: Int -> Int -> Gen [Word8 -> DiffTime -> FBearer]
genSaturators cap n = vectorOf n $ return (\tier start -> FBearer tier cap start)

genRun :: Gen (Word8 -> Bool) -> Gen FloorRun
genRun tierOK = do
  rate    <- choose (1e5, 2e6)
  cap     <- elements [4096, 16384, 65536, 131072]
  percent <- choose (5, 30)
  dur     <- fromIntegral <$> choose (1, 4 :: Int)
  nSat    <- choose (2, 4)
  nCred   <- choose (1, 8)
  ok      <- tierOK
  satTier <- case filter ok [1, 2, 3] of
                  []       -> pure 0
                  credTier -> frequency [ (1, pure 0), (1, elements credTier) ]
  sats    <- genSaturators cap nSat
  creds   <- vectorOf nCred ((,) <$> (suchThat (choose (1, 5)) ok) <*> genBatch cap)
  starts  <- genStarts (nSat + nCred)
  let bearers = zipWith ($) (map ($ satTier) sats ++ [ \s -> FBearer t b s | (t, b) <- creds ]) starts
  return FloorRun { frRate = rate, frCapacity = cap, frPercent = percent, frDuration = dur,
                    frRotation = Nothing, frBearers = bearers }

-- | Shrinking keeps at least two bearers asking for a whole capacity, so the
-- budget stays busy and the laws stated for a saturated budget keep their
-- premise.
instance Arbitrary FloorRun where
  arbitrary = genRun (pure (const True))
  shrink fr@FloorRun { frBearers, frDuration } =
       [ fr { frBearers = bs }
       | bs <- shrinkList (const []) frBearers
       , length bs >= 2, length (filter ((== frCapacity fr) . fbBatch) bs) >= 2 ]
    ++ [ fr { frDuration = d } | d <- [1, 2], d < frDuration ]
    ++ [ fr { frBearers = map (\b -> b { fbBatch = 256 }) frBearers }
       | any ((> 256) . fbBatch) frBearers ]

-- | One grant seen by the harness.
data Grant = Grant {
    gBearer :: !Int,
    gTier   :: !Word8,
    gAsked  :: !Time,
    gAt     :: !Time,
    gBytes  :: !Int,
    gSource :: !GrantSource
  }
  deriving Show

-- | Run the bearers against a budget with the floor attached (or not), until
-- 'endTime'; returns the grants and the budget's counters.
runFloor :: Bool -> FloorRun -> ([Grant], Bucket.BucketStats)
runFloor withFloor fr = let (gs, st, _) = runFloor' withFloor fr in (gs, st)

-- | The same, also returning the requests still waiting at 'endTime', each
-- with the instant it was made: a bearer starved for the whole run shows up
-- here and nowhere else.
runFloor' :: Bool -> FloorRun -> ([Grant], Bucket.BucketStats, [(Int, Time)])
runFloor' withFloor fr = runSimOrThrow (runFloorM Nothing withFloor fr)

-- | 'runFloor'' in any monad, with times relative to the run's start. Given
-- @inspect@, it is handed the budget and the bearers' handles halfway through
-- the run, at its end before the bearers are cancelled, and again after;
-- without, the run is exactly the IOSim one.
runFloorM :: forall m. (MonadAsync m, MonadDelay m, MonadMask m, MonadTimer m)
          => Maybe (Bucket.Bucket m -> [Bucket.BucketHandle m] -> m ())
          -> Bool -> FloorRun -> m ([Grant], Bucket.BucketStats, [(Int, Time)])
runFloorM inspect_m withFloor fr@FloorRun { frRate, frCapacity, frPercent, frRotation, frBearers } = do
    t0 <- getMonotonicTime
    let end = (endTime fr `diffTime` Time 0) `addTime` t0
    budget <- Bucket.newBucket frRate frCapacity frRotation
    if withFloor && frPercent > 0
       then Bucket.attachFloor budget (fromIntegral frPercent / 100)
       else return ()
    grants  <- newTVarIO []
    pending <- newTVarIO Map.empty
    hsas <- forM (zip [0 ..] frBearers) $ \(i, FBearer { fbTier, fbBatch, fbStart }) -> do
      h <- Bucket.registerBearer budget
      atomically $ Bucket.setRank h (Bucket.Rank fbTier)
      fmap ((,) h) $ async $ do
        labelThisThread (bearerLabel i)
        threadDelay fbStart
        -- masked: a grant is recorded before a cancellation can land, so the
        -- harness sees every grant the bucket counted; the wait itself is
        -- blocking STM and stays interruptible
        let loop = do
              asked <- getMonotonicTime
              atomically $ modifyTVar pending (Map.insert i asked)
              mask_ $ do
                src <- Bucket.awaitGrantWaited Nothing 0 h fbBatch
                at <- getMonotonicTime
                atomically $ do
                  modifyTVar grants (Grant i fbTier asked at fbBatch src :)
                  modifyTVar pending (Map.delete i)
              loop
        loop
    let (hs, as) = unzip hsas
    case inspect_m of
         Nothing -> return ()
         Just inspect -> do
           threadDelay ((end `diffTime` t0) / 2)
           inspect budget hs
    now <- getMonotonicTime
    threadDelay (end `diffTime` now)
    waiting <- Map.toList <$> readTVarIO pending
    mapM_ (\inspect -> inspect budget hs) inspect_m
    mapM_ cancel as
    gs <- reverse <$> readTVarIO grants
    (st, _, _, _) <- atomically $ Bucket.bucketSnapshot budget end
    mapM_ (\inspect -> inspect budget hs) inspect_m
    return (gs, st, waiting)

credited :: Word8 -> Bool
credited t = t >= 1 && t <= 3

classOf :: Word8 -> Maybe FloorClass
classOf = floorClass

-- | Grants after every bearer has started and before the run ends: the
-- steady state the laws are stated for. At the final instant the harness
-- cancels the bearers one by one, and a credited bearer left alone at the
-- head for that instant takes from the budget; that is the harness, not the
-- scheduler.
steady :: FloorRun -> [Grant] -> [Grant]
steady fr = filter (\g -> gAt g >= Time (lastStart fr) && gAt g < endTime fr)

window :: FloorRun -> DiffTime
window = frDuration

--
-- Laws
--

-- | Everything granted, by any lane, is within the budget's rate times the
-- elapsed time, one capacity to start full, and one batch the floor may be
-- in debt by.
prop_ceiling :: FloorRun -> Property
prop_ceiling fr =
  let (gs, _) = runFloor True fr
      granted = sum (map gBytes gs)
      elapsed = realToFrac (endTime fr `diffTime` Time 0) :: Double
      bound   = frRate fr * elapsed + fromIntegral (frCapacity fr) + fromIntegral (maxBatch fr)
  in counterexample ("granted " ++ show granted ++ " bound " ++ show bound)
   $ fromIntegral granted <= bound

prop_exclusion :: FloorRun -> Property
prop_exclusion fr =
  let (gs, _) = runFloor True fr
      wrong = [ g | g <- gs, gSource g == FromFloor, not (credited (gTier g)) ]
  in counterexample (show (take 3 wrong)) $ null wrong

-- | Each credited class present receives at least its share of the floor
-- over the window, in bytes, by whichever lane served it, less two batches
-- and one capacity. The floor is a lower bound on service: when the budget
-- has slack the cascade serves the class instead, and more.
prop_class_share :: FloorRun -> Property
prop_class_share fr =
  let (gs0, _) = runFloor True fr
      gs = steady fr gs0
      classes = Map.fromListWith (+)
                  [ (c, gBytes g) | g <- gs, Just c <- [classOf (gTier g)] ]
      present = Map.keys (Map.fromList [ (c, ()) | b <- frBearers fr, Just c <- [classOf (fbTier b)] ])
      k = length present
      share = floorRate fr * realToFrac (window fr) / fromIntegral (max 1 k)
      slack = fromIntegral (2 * maxBatch fr + frCapacity fr) :: Double
      bound = share - slack
  in k > 0 ==>
     counterexample (show (Map.toList classes) ++ " each >= " ++ show bound)
   $ all (\c -> fromIntegral (Map.findWithDefault 0 c classes) >= bound) present

-- | Within a class, backlogged bearers get floor turns in rotation: their
-- counts over the window differ by at most one. Stated for a saturated
-- budget: a run in which the cascade reached a credited bearer is discarded,
-- since such a grant puts that bearer at the back of its class out of turn.
prop_class_turns :: FloorRun -> Property
prop_class_turns fr =
  let (gs0, _) = runFloor True fr
      gs = steady fr gs0
      reached = or [ credited (gTier g) | g <- gs, gSource g /= FromFloor ]
      perBearer = Map.fromListWith (+) [ (gBearer g, 1 :: Int) | g <- gs, gSource g == FromFloor ]
      byClass = Map.fromListWith (++)
                  [ (c, [Map.findWithDefault 0 i perBearer])
                  | (i, b) <- zip [0 ..] (frBearers fr), Just c <- [classOf (fbTier b)] ]
      spread ns = maximum ns - minimum ns
  in classify reached "the cascade reached a credited bearer" $
     not reached ==>
     counterexample (show (Map.toList byClass))
   $ all (\ns -> spread ns <= 1) (Map.elems byClass)

-- | Every wait of a credited bearer ends within the round the floor needs
-- to reach it: the bearers ahead of it in its class a batch each and its own
-- batch; as many bytes again for each other class, plus two batches, since
-- fair queuing keeps two backlogged classes within a batch of each; and the
-- floor's capacity to refill, all at the floor's rate.
prop_bounded_wait :: FloorRun -> Property
prop_bounded_wait fr =
  let (gs0, _) = runFloor True fr
      gs = [ g | g <- steady fr gs0, credited (gTier g) ]
      classes = Map.fromListWith (+) [ (c, 1 :: Int) | b <- frBearers fr, Just c <- [classOf (fbTier b)] ]
      k = Map.size classes
      bound g = let nc = Map.findWithDefault 1 (maybe (error "class") id (classOf (gTier g))) classes
                    own   = (nc - 1) * maxBatch fr + gBytes g
                    bytes = k * own + 2 * (k - 1) * maxBatch fr + frCapacity fr
                in realToFrac (fromIntegral bytes / floorRate fr :: Double) + 0.001 :: DiffTime
      late = [ (g, gAt g `diffTime` gAsked g, bound g) | g <- gs, gAt g `diffTime` gAsked g > bound g ]
  in counterexample (show (take 2 late)) $ null late

-- | Fifty partners, bulk batches, rotation longer than the run: the cascade
-- serves them in rotation order and the floor must still give each a share.
prop_same_tier :: Property
prop_same_tier =
  forAllShrink gen (\fr -> [ fr { frBearers = take n (frBearers fr) } | n <- [8, 4], n < length (frBearers fr) ]) $ \fr ->
    let (gs0, _) = runFloor True fr
        gs = steady fr gs0
        perBearer = Map.fromListWith (+) [ (gBearer g, gBytes g) | g <- gs ]
        n = length (frBearers fr)
        bound = floorRate fr * realToFrac (window fr) / fromIntegral n
              - fromIntegral (maxBatch fr + frCapacity fr)
        starved = [ (i, Map.findWithDefault 0 i perBearer) | i <- [0 .. n - 1]
                  , fromIntegral (Map.findWithDefault 0 i perBearer) < bound ]
    in counterexample ("bound " ++ show bound ++ " starved " ++ show starved) $ null starved
  where
    gen = do
      fr <- genRun (pure (const True))
      n  <- choose (8, 50)
      starts <- genStarts n
      seed <- arbitrary
      let cap = frCapacity fr
          bearers = [ FBearer 1 cap s | s <- starts ]
      return fr { frBearers = bearers
                , frRotation = Just (Bucket.Rotation seed (10 * frDuration fr + 100) 4) }

-- | Bulk bearers and a few small senders, all in one tier, rotation longer
-- than the run, floor off: what the order within the tier does on its own.
genOneTier :: Word8 -> Gen FloorRun
genOneTier tier = do
  fr     <- genRun (pure (const True))
  nBulk  <- choose (2, 12)
  nSmall <- choose (1, 3)
  let cap = frCapacity fr
  bulk   <- vectorOf nBulk  (choose (cap `div` 4, cap))
  small  <- vectorOf nSmall (choose (256, max 256 (cap `div` 32)))
  starts <- genStarts (nBulk + nSmall)
  seed   <- arbitrary
  return fr { frBearers  = zipWith (FBearer tier) (bulk ++ small) starts
            , frPercent  = 0
            , frRotation = Just (Bucket.Rotation seed (10 * frDuration fr + 100) 4) }

shrinkOneTier :: FloorRun -> [FloorRun]
shrinkOneTier fr@FloorRun { frBearers, frDuration } =
     [ fr { frBearers = bs } | bs <- shrinkList (const []) frBearers, length bs >= 2 ]
  ++ [ fr { frDuration = d } | d <- [1, 2], d < frDuration ]

-- | In the rest tier backlogged bearers take turns: over the steady window
-- their grant counts differ by at most one, bulk and small alike.
prop_rest_turns :: Property
prop_rest_turns =
  forAllShrink (genOneTier 4) shrinkOneTier $ \fr ->
    let (gs0, _) = runFloor False fr
        gs = steady fr gs0
        counts = [ length [ () | g <- gs, gBearer g == i ] | i <- [0 .. length (frBearers fr) - 1] ]
    in classify (length gs > 2 * length (frBearers fr)) "several rounds" $
       counterexample (show counts)
     $ maximum counts - minimum counts <= 1

-- | A rest bearer never waits longer than one round: a batch for every other
-- bearer, its own, and a capacity to refill, at the budget's rate. Judged on
-- every grant and on every request still waiting when the run ends, so a
-- bearer that is never served fails it rather than escaping it.
prop_rest_wait :: Property
prop_rest_wait =
  forAllShrink (genOneTier 4) shrinkOneTier $ \fr ->
    let (gs0, _, waiting) = runFloor' False fr
        gs = steady fr gs0
        batches = sum (map fbBatch (frBearers fr))
        bound b = realToFrac (fromIntegral (batches + b + frCapacity fr) / frRate fr :: Double)
                  + 0.001 :: DiffTime
        late = [ (Left (gBearer g), gAt g `diffTime` gAsked g) | g <- gs, gAt g `diffTime` gAsked g > bound (gBytes g) ]
            ++ [ (Right i, w) | (i, asked) <- waiting, asked >= Time (lastStart fr)
               , let w = endTime fr `diffTime` asked, w > bound (fbBatch (frBearers fr !! i)) ]
    in classify (not (null waiting)) "requests still waiting at the end" $
       counterexample (show (take 2 late)) $ null late

-- | Below the threshold the dealt place decides and a backlogged head keeps
-- the tier: in a credited tier under a rotation longer than the run, with
-- the floor off, exactly one bearer is served in the steady window.
prop_credited_head_first :: Property
prop_credited_head_first =
  forAllShrink (genOneTier 2) shrinkOneTier $ \fr ->
    let (gs0, _) = runFloor False fr
        served = Map.keys (Map.fromList [ (gBearer g, ()) | g <- steady fr gs0 ])
    in counterexample (show served) $ length served === 1

-- | With nobody credited, the run with the floor is the run without it.
prop_idle_floor :: Property
prop_idle_floor =
  forAll (genRun (pure (\t -> t == 0 || t >= 4))) $ \fr ->
    let strip (gs, _) = [ (gBearer g, gAt g, gBytes g) | g <- gs ]
    in strip (runFloor True fr) === strip (runFloor False fr)

-- | A bearer's rule changes while it waits. Two tier-0 saturators keep the
-- budget busy, so a bearer at rest never reaches it. Forward: a bearer queues
-- at rest, its rule turns credited at @rcAt@, and the floor must serve it
-- within a refresh of that. Backward: a credited bearer takes one batch from
-- the floor and queues for the next while the floor refills, slowly enough
-- that the refresh comes first; its rule turns rest at @rcAt@, and within a
-- refresh it has left the floor's queue and waits at rest; cancelled then,
-- it leaves the budget's queue under the keys it holds now, not the ones it
-- queued with.
data RuleChange = RuleChange {
    rcRate    :: !Double,
    rcCap     :: !Int,
    rcPercent :: !Int,
    rcTier    :: !Word8,     -- ^ the credited tier
    rcAt      :: !DiffTime,  -- ^ when the rule changes, after the bearer queued
    rcToRest  :: !Bool       -- ^ credited to rest, else rest to credited
  }
  deriving Show

instance Arbitrary RuleChange where
  arbitrary = do
    rcCap     <- elements [16384, 65536, 131072]
    rcPercent <- choose (5, 30)
    rcTier    <- choose (1, 3)
    rcToRest  <- arbitrary
    -- backward, the floor must not refill the bearer's next batch before the
    -- refresh after the change sees it: a refill time past a refresh and
    -- the change early in it
    (rcRate, rcAt) <-
      if rcToRest
         then do
           refill <- choose (realToFrac Bucket.tierRefresh + 1, 40 :: Double)
           at'    <- choose (0.1, 2 :: Double)
           return (fromIntegral rcCap / (refill * fromIntegral rcPercent / 100), realToFrac at')
         else (,) <$> choose (1e5, 1e6) <*> (realToFrac <$> choose (0.1, 3 :: Double))
    return RuleChange { rcRate, rcCap, rcPercent, rcTier, rcAt, rcToRest }
  shrink rc@RuleChange { rcTier, rcPercent } =
       [ rc { rcTier = 1 }     | rcTier /= 1 ]
    ++ [ rc { rcPercent = 10 } | rcPercent /= 10 ]

prop_rule_followed_while_queued :: RuleChange -> Property
prop_rule_followed_while_queued rc@RuleChange { rcRate, rcCap, rcPercent, rcTier, rcAt, rcToRest } =
    counterexample (show rc) $
    classify rcToRest "credited to rest" $
      runSimOrThrow $ do
        budget <- Bucket.newBucket rcRate rcCap Nothing
        Bucket.attachFloor budget (fromIntegral rcPercent / 100)
        -- the saturators, a whole capacity at a time, forever
        sats <- forM [0, 0.001] $ \d -> do
          h <- Bucket.registerBearer budget
          atomically $ Bucket.setRank h (Bucket.Rank 0)
          async $ threadDelay d >> forever (Bucket.awaitGrant h rcCap)
        -- the bearer whose rule will change
        ruleVar <- newTVarIO (Bucket.Rank (if rcToRest then rcTier else 4))
        h <- Bucket.registerBearer budget
        atomically $ Bucket.setRankSource h (\_ -> readTVar ruleVar)
        threadDelay 0.01
        t0 <- getMonotonicTime
        victim <- async $ do
          when rcToRest $ Bucket.awaitGrant h rcCap      -- the floor's full bucket, at once
          Bucket.awaitGrant h (if rcToRest then rcCap else need)
          getMonotonicTime
        threadDelay rcAt
        atomically $ writeTVar ruleVar (Bucket.Rank (if rcToRest then 4 else rcTier))
        r <- if rcToRest
                then do
                  -- within a refresh: out of the floor's queue, waiting at rest
                  threadDelay (Bucket.tierRefresh + 0.1)
                  still <- poll victim
                  fp <- atomically $ Bucket.floorPending budget
                  (_, _, _, byTier) <- getMonotonicTime >>= atomically . Bucket.bucketSnapshot budget
                  -- cancelled, it leaves the budget's queue under its current keys
                  cancel victim
                  fp' <- atomically $ Bucket.floorPending budget
                  (_, _, queued', _) <- getMonotonicTime >>= atomically . Bucket.bucketSnapshot budget
                  return $ counterexample (show (still, fp, Map.toList byTier, fp', queued'))
                         $ isNothing still .&&. fp === 0 .&&. Map.lookup 4 byTier === Just 1
                           .&&. fp' === 0 .&&. queued' === 2
                else do
                  -- within a refresh and the floor's own time for the batch
                  let bound = rcAt + Bucket.tierRefresh
                            + realToFrac (fromIntegral need / (rcRate * fromIntegral rcPercent / 100) :: Double)
                            + 0.1
                  served <- timeout bound (wait victim)
                  return $ counterexample ("served " ++ show (fmap (`diffTime` t0) served) ++ " bound " ++ show bound)
                         $ maybe False (\at -> at `diffTime` t0 <= bound) served
        mapM_ cancel sats
        return r
  where
    need = max 256 (rcCap `div` 4)

-- | So the no-thunks properties here and in "Test.Mux" can inspect a
-- bucket's counters, and in IO its whole state.
instance NoThunks Bucket.BucketStats
instance NoThunks Bucket.TierGrants
instance NoThunks (Bucket.Bucket IO)
instance NoThunks (Bucket.Floor IO)
instance NoThunks (Bucket.BucketHandle IO)
instance NoThunks Bucket.BurstState
instance NoThunks Bucket.Detector
instance NoThunks Bucket.Open
instance NoThunks Bucket.Burst
instance NoThunks Bucket.Rotation
instance NoThunks Bucket.Ticket
instance NoThunks Bucket.TierAsked
instance NoThunks FloorState
instance NoThunks FloorClass

-- | And the mux counters, in IO.
instance NoThunks (Counters.MuxCounters IO)
instance NoThunks (Counters.MuxByteCells IO)
instance (NoThunks a, NoThunks b) => NoThunks (Counters.Sides a b)
instance NoThunks Counters.EgressCounts
instance NoThunks Counters.IngressCounts
instance NoThunks Counters.SchedulingCounts
instance NoThunks Counters.TierCounts
deriving via OnlyCheckWhnfNamed "MiniProtocolNum" MiniProtocolNum
  instance NoThunks MiniProtocolNum
deriving via OnlyCheckWhnfNamed "MiniProtocolDir" MiniProtocolDir
  instance NoThunks MiniProtocolDir

-- | A strict TVar in IO is a GHC TVar, whose contents nothunks reads.
instance NoThunks a => NoThunks (StrictTVar IO a) where
  showTypeOf _ = "StrictTVar IO"
  wNoThunks ctxt = wNoThunks ctxt . toLazyTVar

-- | With a floor attached, the budget's counters hold no thunks either.
prop_stats_no_thunks :: FloorRun -> Property
prop_stats_no_thunks fr = case runFloor True fr of
  (_, !st) -> counterexample (show (unsafeNoThunks st)) (isNothing (unsafeNoThunks st))

-- | In IO, where a TVar's contents can be inspected: halfway through a run
-- and after it, the budget, its floor and the bearers' handles hold no thunks.
-- Wall-clock time, so the runs are short; leaving state behind does not need
-- a long one.
prop_state_no_thunks_io :: FloorRun -> Property
prop_state_no_thunks_io fr0 = withNumTests 20 $ ioProperty $ do
    found <- newIORef []
    _ <- runFloorM (Just $ \budget hs -> do
                     -- the list is the harness's, built lazily; the handles in it are not
                     _ <- evaluate (foldr seq () hs)
                     r <- (<|>) <$> noThunks ["budget"] budget <*> noThunks ["handles"] hs
                     mapM_ (\info -> modifyIORef' found (info :)) r)
                   True fr
    infos <- readIORef found
    return $ counterexample (show infos) (null infos)
  where
    fr = fr0 { frDuration = min 0.3 (frDuration fr0)
             , frBearers  = [ b { fbStart = min 0.1 (fbStart b) } | b <- frBearers fr0 ] }

-- | A run, with the bursts the budget closed over it, drained at the end
-- before the bearers are cancelled, the one in progress closed as it
-- stands: the cancellation instant interleaves leaves and grants in the
-- scheduler's order, which decides what a lapsed burst held open by the
-- queue counts, so it is kept out.
runFloorBursts :: FloorRun -> ([Grant], [(Int, Time)], [Bucket.Burst])
runFloorBursts fr = runSimOrThrow $ do
    calls  <- newTVarIO (0 :: Int)
    bursts <- newTVarIO []
    let drain budget _ = do
          n <- atomically $ stateTVar calls (\c -> (c, c + 1))
          -- the second call is the one at the end, before the cancellation
          when (n == 1) $ do
            now <- getMonotonicTime
            atomically (Bucket.drainBurstsAt budget now >>= writeTVar bursts)
    (gs, _, waiting) <- runFloorM (Just drain) True fr
    (,,) gs waiting <$> readTVarIO bursts

-- | What the budget holds at the end of a run is the fold of the grants the
-- harness saw before it over the detector's own rules, each queued request
-- of a credited bearer counted in the credited queue from its request to
-- its grant, or to the end if it is still waiting then. The peak queue is
-- compared within one: two queue changes in the same simulated instant are
-- ordered by the scheduler, not by the clock, and a request granted the
-- instant it queued is a fast-path grant to the harness.
prop_bursts_match_grants :: FloorRun -> Property
prop_bursts_match_grants fr =
    classify (length got > 1) "more than one burst" $
    classify (any ((> 0) . Bucket.bFiller) got) "filler counted" $
    counterexample (show (got, want)) $
         counterexample "records" (map strip got === map strip want)
    .&&. counterexample "peaks" (and (zipWith (\a b -> abs (Bucket.bPeakQueued a - Bucket.bPeakQueued b) <= 1) got want))
  where
    (gs, waiting, got) = runFloorBursts fr
    gap  = Burst.gapFor (frRate fr) (frCapacity fr)
    end  = endTime fr
    strip b = b { Bucket.bPeakQueued = 0 }
    -- the events the bucket told the detector before the end, as (instant,
    -- order, event); a request still waiting at the end stays queued
    before = [ g | g <- gs, gAt g < end ]
    events = sortOn (\(t, o, _) -> (t, o)) $
         [ (gAt g, 1 :: Int, Left g) | g <- before ]
      ++ concat [ [ (gAsked g, 2, Right 1), (gAt g, 0, Right (-1)) ]
                | g <- before, gAsked g < gAt g, Burst.creditedTier (gTier g) ]
      ++ [ (asked, 2, Right 1)
         | (i, asked) <- waiting, Burst.creditedTier (fbTier (frBearers fr !! i)) ]
    step (q, d) (t, _, ev) = case ev of
      Left g  -> (q, Burst.burstGrant gap t (gTier g) (fromIntegral (gBearer g)) (gBytes g)
                                      (gAt g `diffTime` gAsked g) 0 d)
      Right k -> let q' = q + k in (q', Burst.burstQueued gap t q' d)
    want = fst $ Burst.takeBursts $ Burst.closeOpen $ Burst.burstSweep gap end
                                  $ snd $ foldl' step (0, Burst.emptyDetector) events

-- | A bearer catching up at rest rank, or several: no burst, no busy period.
prop_rest_alone_no_burst :: Property
prop_rest_alone_no_burst =
  forAllShrink (genOneTier 4) shrinkOneTier $ \fr ->
    let (gs, _, got) = runFloorBursts fr
        (_, st) = runFloor False fr
    in classify (length gs > 10) "many grants" $
       counterexample (show (got, Bucket.bsBursts st, Bucket.bsBusyTime st)) $
         got === [] .&&. Bucket.bsBursts st === 0 .&&. Bucket.bsBusyTime st === 0

-- | One credited grant, then silence: the counters' drain returns its burst
-- once the gap has passed, and nothing before.
prop_drain_sweeps :: Property
prop_drain_sweeps =
  forAllShrink gen (const []) $ \(rate, cap, delay) ->
    let gap = Burst.gapFor rate cap
        (early, late) = runSimOrThrow $ do
          budget <- Bucket.newBucket rate cap Nothing
          h <- Bucket.registerBearer budget
          atomically $ Bucket.setRank h (Bucket.Rank 2)
          Bucket.awaitGrant h cap
          t0 <- getMonotonicTime
          e <- atomically $ Bucket.takeBurstsAt budget (delay `addTime` t0)
          l <- atomically $ Bucket.takeBurstsAt budget ((gap + 1) `addTime` t0)
          return (e, l)
    in classify (delay > gap) "drained after the gap" $
       counterexample (show (gap, delay, early, late)) $
         if delay > gap
            then length early === 1 .&&. late === []
            else early === [] .&&. length late === 1
  where
    -- the delay straddles the gap, which depends on the rate and capacity
    gen = do
      rate <- choose (1e5, 2e6)
      cap  <- elements [16384, 65536, 131072]
      f    <- choose (0.05, 3 :: Double)
      return (rate, cap, realToFrac f * Burst.gapFor rate cap)

-- | A bearer cancelled in the instant its queued grant commits: the
-- exception lands after the grant's transaction and before the mask is
-- back, when the request is already out of the queue, and the credited
-- queue count must come out right. The canceller waits for the queue to
-- empty, so its throw always follows the commit; the bearer's thread dies
-- holding its grant, which is the caller's problem, not the bucket's.
-- Explored with IOSimPOR, whose default schedule lands the throw in the
-- window. A rank-0 bearer empties the bucket, a rank-2 bearer queues for a
-- second and is cancelled as it is granted, a third then queues for a
-- second. Busy time is the seconds a credited bearer was queued, and the
-- one burst closes after the gap only if the count is back at zero.
prop_cancel_after_grant :: Property
prop_cancel_after_grant =
  IOSim.exploreSimTrace id sim $ \_ trace ->
    case IOSim.traceResult False trace of
      Left e -> counterexample (show e) False
      Right (survived, busy, bursts) ->
        classify survived "the bearer returned before the throw" $
        counterexample (show (survived, busy, bursts)) $
          busy === 2 .&&. bursts === 1
  where
    sim :: forall s. IOSim s (Bool, DiffTime, Int)
    sim = do
      exploreRaces
      budget <- Bucket.newBucket 4096 4096 Nothing
      let bearer tier = do
            h <- Bucket.registerBearer budget
            atomically $ Bucket.setRank h (Bucket.Rank tier)
            return h
          queued = do
            (_, _, n, _) <- Bucket.bucketSnapshot budget (Time 0)
            return n
      a <- bearer 0
      b <- bearer 2
      d <- bearer 2
      Bucket.awaitGrant a 4096                 -- the bucket is empty for a second
      bt <- async (Bucket.awaitGrant b 4096)   -- queued until the tokens are back
      canceller <- async $ do
        atomically (queued >>= check . (== 1))  -- queued
        atomically (queued >>= check . (== 0))  -- granted: the transaction committed
        cancel bt
      wait canceller
      survived <- either (const False) (const True) <$> waitCatch bt
      Bucket.awaitGrant d 4096
      now <- getMonotonicTime
      (st, _, _, _) <- atomically $ Bucket.bucketSnapshot budget now
      bursts <- atomically $ Bucket.takeBurstsAt budget (3 `addTime` now)
      return (survived, Bucket.bsBusyTime st, length bursts)

-- | Connections sharing one allowance, as a big-ledger pool's relays share
-- the pool's bucket, all queued behind a saturating local root with the
-- floor on. Their rule answers the pool tier while the shared credit is
-- positive and the rest once it is spent; each grant charges the credit in
-- the transaction that grants, while it is positive. The credit is spent by
-- whole batches: exactly as many grants are charged as it takes to empty
-- it, and the floor, which serves only credited tiers, serves none of them
-- beyond those: a request queued at the pool tier is moved to the rest
-- before it is served, not a refresh later.
data SharedCase = SharedCase {
    scRate   :: Double,
    scCap    :: Int,
    scPool   :: Int,      -- ^ connections sharing the allowance
    scBatch  :: Int,
    scCredit :: Int
  }
  deriving Show

instance Arbitrary SharedCase where
  arbitrary = do
    rate  <- choose (1e5, 1e6)
    cap   <- elements [16384, 65536, 131072]
    k     <- choose (2, 12)
    batch <- choose (256, cap `div` 2)
    c     <- choose (1, 4 * batch)
    return (SharedCase rate cap k batch c)
  shrink sc@SharedCase { scPool, scCredit } =
       [ sc { scPool = k } | k <- shrink scPool, k >= 2 ]
    ++ [ sc { scCredit = c } | c <- shrink scCredit, c >= 1 ]

prop_shared_credit_spent_once :: SharedCase -> Property
prop_shared_credit_spent_once sc@SharedCase { scPool, scBatch, scCredit } =
    counterexample (show (charges, floorGrants, poolGrants)) $
    classify (needed < scPool) "fewer batches paid for than connections queued" $
    cover 80 (floorGrants > 0) "the floor served the pool" $
         counterexample "charges while in credit" (paid === min needed poolGrants)
    .&&. counterexample "floor grants beyond those" (floorGrants <= paid)
    .&&. counterexample "every grant charged once" (length charges === poolGrants)
  where
    needed = (scCredit + scBatch - 1) `div` scBatch
    (charges, floorGrants, poolGrants) = runShared sc
    paid = length (filter (> 0) charges)

-- | A partner's grants with nobody else asking: every grant takes the fast
-- path. Its rule answers the partner tier while its own credit lasts, its
-- pool's tier while the pool's lasts, and the rest after; each grant charges
-- the bucket that gave the rank the rule last answered. Asked at every
-- charging grant, the rule moves on as each credit runs out: the own credit
-- pays for as many whole batches as it takes to spend it, then the pool's,
-- then nothing. A rank cached from an earlier grant would keep charging the
-- spent own credit, which charges nothing, and leave the pool's untouched.
data FastCase = FastCase { fcBatch :: Int, fcOwn :: Int, fcPool :: Int }
  deriving Show

instance Arbitrary FastCase where
  arbitrary = do
    b <- choose (256, 4096)
    FastCase b <$> choose (1, 5 * b) <*> choose (1, 5 * b)
  shrink fc@FastCase { fcOwn, fcPool } =
       [ fc { fcOwn = o } | o <- shrink fcOwn, o >= 1 ]
    ++ [ fc { fcPool = q } | q <- shrink fcPool, q >= 1 ]

prop_fast_path_charges_current_rank :: FastCase -> Property
prop_fast_path_charges_current_rank fc@FastCase { fcBatch, fcOwn, fcPool } =
    counterexample (show fc) $
      runSimOrThrow run === expected
  where
    batches c = (c + fcBatch - 1) `div` fcBatch
    grants    = batches fcOwn + batches fcPool + 3
    expected  = replicate (batches fcOwn) "own" ++ replicate (batches fcPool) "pool"
             ++ replicate 3 "nothing"

    run :: IOSim s [String]
    run = do
      budget <- Bucket.newBucket 1e12 1000000000 Nothing
      own    <- newTVarIO fcOwn
      pool   <- newTVarIO fcPool
      last_  <- newTVarIO (4 :: Word8)
      log_   <- newTVarIO []
      h <- Bucket.registerBearer budget
      let rule _ = do
            o <- readTVar own
            q <- readTVar pool
            let tier | o > 0     = 1
                     | q > 0     = 2
                     | otherwise = 4
            writeTVar last_ tier
            return (Bucket.Rank tier)
          spend var name = do
            c <- readTVar var
            if c > 0 then writeTVar var (c - fcBatch) >> modifyTVar log_ (name :)
                     else modifyTVar log_ ("nothing" :)
          charge _ = do
            t <- readTVar last_
            case t of
                 1 -> spend own "own"
                 2 -> spend pool "pool"
                 _ -> modifyTVar log_ ("nothing" :)
      atomically $ Bucket.setRankSource h rule
      forM_ [1 .. grants] $ \_ -> Bucket.awaitGrantCharged Nothing 0 charge h fcBatch
      reverse <$> readTVarIO log_

-- | The run: the credit before each charge, in order, and the pool
-- connections' floor grants and all their grants.
runShared :: SharedCase -> ([Int], Int, Int)
runShared SharedCase { scRate, scCap, scPool, scBatch, scCredit } = runSimOrThrow $ do
    budget <- Bucket.newBucket scRate scCap Nothing
    Bucket.attachFloor budget 0.1
    credit  <- newTVarIO scCredit
    charges <- newTVarIO []
    grants  <- newTVarIO []
    let end = 2 :: DiffTime
    -- two local roots asking a capacity at a time keep the budget's queue
    -- full, so every pool request queues
    roots <- forM [0, 0.001 :: DiffTime] $ \start -> do
      h <- Bucket.registerBearer budget
      atomically $ Bucket.setRank h (Bucket.Rank 0)
      async $ threadDelay start >> forever (Bucket.awaitGrant h scCap)
    let rule _ = do
          c <- readTVar credit
          return (Bucket.Rank (if c > 0 then 2 else 4))
        charge _ = do
          c <- readTVar credit
          modifyTVar charges (c :)
          when (c > 0) $ writeTVar credit (c - scBatch)
    pool <- forM [1 .. scPool] $ \i -> do
      h <- Bucket.registerBearer budget
      atomically $ Bucket.setRankSource h rule
      async $ do
        threadDelay (0.01 + fromIntegral i / 1000)
        forever $ mask_ $ do
          src <- Bucket.awaitGrantCharged Nothing 0 charge h scBatch
          atomically $ modifyTVar grants (src :)
    threadDelay end
    -- read before cancelling, so no grant is half recorded
    (cs, gs) <- atomically $ (,) <$> readTVar charges <*> readTVar grants
    mapM_ cancel (roots ++ pool)
    return (reverse cs, length (filter (== FromFloor) gs), length gs)

-- | The label of a bearer's thread in 'runFloorM', and the bearer it names.
bearerLabel :: Int -> String
bearerLabel i = "bearer " ++ show i

bearerOf :: String -> Maybe Int
bearerOf lbl = stripPrefix "bearer " lbl >>= readMaybe

-- | No timer outlives the request it was set for. A queued request sets a
-- refresh timer, and one more for each sleep until the bytes are there; when
-- the request is granted or cancelled, every timer it set has fired or is
-- cancelled in that instant, so what the runtime holds for the queue is the
-- queue, not the recent turnover. Read off the IOSim trace: a timer belongs
-- to a request its bearer had under way when it was created, and must have
-- fired or been cancelled by the instant that request was granted, or the
-- run's end that cancelled it.
prop_timers_follow_requests :: FloorRun -> Property
prop_timers_follow_requests fr =
  case traceResult False trace of
    Left e -> counterexample (show e) False
    Right (grants, _, waiting) ->
      let windows = Map.fromListWith (++) $
               [ (gBearer g, [(gAsked g, gAt g)]) | g <- grants ]
            ++ [ (i, [(asked, endTime fr)]) | (i, asked) <- waiting ]
          -- the requests under way at @c@: the fast path grants in the
          -- instant it asks, so many share an instant with the one queued
          requestEnd i c = case [ d | (a, d) <- Map.findWithDefault [] i windows, a <= c, c <= d ] of
                                [] -> Nothing
                                ds -> Just (maximum ds)
          outlives (i, tmid, c) = case (requestEnd i c, Map.lookup tmid ends) of
                                       (Just d, Just (_, e)) -> e > d
                                       _                     -> True
          bad = filter outlives created
      in cover 90 (not (null created)) "a timer set" $
         cover 70 (any ((== Just True) . fmap fst . (`Map.lookup` ends) . (\(_, t, _) -> t)) created)
                  "a timer cancelled" $
         counterexample (show (take 5 bad)) (null bad)
  where
    trace  = runSimTrace (runFloorM Nothing True fr)
    events = traceEvents trace
    created = [ (i, tmid, t) | (t, _, Just lbl, EventTimerCreated tmid _ _) <- events
                             , Just i <- [bearerOf lbl] ]
    -- when each timer ended, and whether by cancellation
    ends = Map.fromListWith (\a b -> if snd a <= snd b then a else b)
             [ (tmid, (cancelled, t)) | (t, _, _, e) <- events, Just (tmid, cancelled) <- [endOf e] ]
    endOf (EventTimerCancelled tmid) = Just (tmid, True)
    endOf (EventTimerFired tmid)     = Just (tmid, False)
    endOf _                          = Nothing

prop_accounting :: FloorRun -> Property
prop_accounting fr =
  let (gs, st) = runFloor True fr
      observed = Map.fromListWith (+) [ (gTier g, fromIntegral (gBytes g)) | g <- gs, gSource g == FromFloor ]
      counted  = Map.map Bucket.tgBytes (Bucket.bsFloorTiers st)
      -- every grant, the floor's included, leaves its bearer's last at its tier
      served   = Map.fromListWith max [ ((gTier g, fromIntegral (gBearer g)), gAt g) | g <- gs ]
  in counterexample (show (observed, counted)) (observed === counted)
     .&&. counterexample "served" (served === Bucket.bsServed st)

-- | After 'setBucketRate' the floor hands out its share of the new rate: a
-- credited bearer served only by the floor, behind two saturators, receives
-- share times the new rate over the second phase, within two batches and a
-- capacity, and the old rate's share over the first.
prop_floor_follows_rate :: Property
prop_floor_follows_rate =
  forAllShrink gen (const []) $ \(rate, cap, pct, factor, batch) ->
    let share = fromIntegral pct / 100 :: Double
        phase = 2 :: DiffTime
        slack = fromIntegral (2 * batch + cap) :: Double
        (b1, b2) = runSimOrThrow $ do
          budget <- Bucket.newBucket rate cap Nothing
          Bucket.attachFloor budget share
          bytes <- newTVarIO (0 :: Int)
          let bearer tier need = do
                h <- Bucket.registerBearer budget
                atomically $ Bucket.setRank h (Bucket.Rank tier)
                async $ let loop = do
                              src <- Bucket.awaitGrantWaited Nothing 0 h need
                              when' (src == FromFloor) $ atomically $ modifyTVar bytes (+ need)
                              loop
                        in loop
          as <- sequence [bearer 0 cap, bearer 0 cap, bearer 2 batch]
          threadDelay phase
          b1' <- atomically $ swapTVar bytes 0
          t0 <- getMonotonicTime
          atomically $ Bucket.setBucketRate budget t0 (rate * factor)
          threadDelay phase
          b2' <- readTVarIO bytes
          mapM_ cancel as
          return (b1', b2')
        expect r = share * r * realToFrac phase
    in counterexample (show (b1, expect rate, b2, expect (rate * factor)))
     $ abs (fromIntegral b1 - expect rate) <= slack
       .&&. abs (fromIntegral b2 - expect (rate * factor)) <= slack
  where
    gen = do
      rate   <- choose (2e5, 2e6)
      cap    <- elements [16384, 65536]
      pct    <- choose (5, 30 :: Int)
      factor <- elements [0.25, 0.5, 2, 4]
      batch  <- choose (256, cap `div` 16)
      return (rate, cap, pct, factor, batch)
    when' c a = if c then a else return ()
