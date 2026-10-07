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
import Control.Monad (forM)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow (MonadMask, mask_)
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IOSim

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Word (Word8)

import Network.Mux.Counters qualified as Counters
import Network.Mux.Egress.Bucket (GrantSource (..))
import Network.Mux.Egress.Bucket qualified as Bucket
import Network.Mux.Egress.Floor (FloorClass, FloorState, floorClass)
import Network.Mux.Types (MiniProtocolDir, MiniProtocolNum)
import NoThunks.Class (NoThunks (..), OnlyCheckWhnfNamed (..), noThunks,
           unsafeNoThunks)

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

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
-- the run and again after it; without, the run is exactly the IOSim one.
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

-- | So the no-thunks properties here and in "Test.Mux" can inspect a
-- bucket's counters, and in IO its whole state.
instance NoThunks Bucket.BucketStats
instance NoThunks Bucket.TierGrants
instance NoThunks (Bucket.Bucket IO)
instance NoThunks (Bucket.Floor IO)
instance NoThunks (Bucket.BucketHandle IO)
instance NoThunks Bucket.BurstState
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
