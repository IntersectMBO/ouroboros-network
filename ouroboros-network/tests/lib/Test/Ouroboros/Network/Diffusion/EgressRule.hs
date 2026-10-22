{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}

module Test.Ouroboros.Network.Diffusion.EgressRule (tests) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM, forM_, forever, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadTime.SI (Time (..), diffTime, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (runSimOrThrow)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Word (Word16)
import Network.Mux (MiniProtocolNum (..), Rank (..))
import Network.Mux.Egress.Bucket qualified as Bucket
import Network.Mux.Egress.Floor (FloorClass (..), floorClass)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Ouroboros.Network.Diffusion.PoolAllowances

import Test.Ouroboros.Network.Diffusion.PoolAllowances (freshAt, fullCharged,
           relaysOf)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Diffusion.EgressRule"
    [ testProperty "a connection's rule and sink follow the decision table" prop_rule
    , testProperty "a stranger's lock is wall-clock time, whatever the reading does" prop_lock_is_time
    , testProperty "the allowance is read at every use, so a smaller one caps a charge at once" prop_allowance_follows_reading
    , testProperty "the floor's classes and the turns threshold follow the rule's tiers" prop_floorClass_follows_rank
    , testProperty "a relay queued dry is served once its credit refills" prop_refill_reaches_queued
    ]

-- | What happens to a connection, at a time in seconds: its rank is asked,
-- its scheduled bytes on a protocol are reported, or the pool list is
-- rebuilt under it. A partner's own bucket starts under the same lock as a
-- stranger's, so a partner asked early falls through to what it otherwise
-- is, and one asked after the lock holds the partner tier until it has
-- spent two refills.
data Op = Ask Double
        | Charge Double Word16 Int
        | Rebuild Double
  deriving Show

data Case = Case {
    cStanding  :: Standing,
    cPools     :: [Int],       -- ^ the matched buckets' charged totals; none for a stranger
    cUncharged :: [Word16],
    cLock      :: Double,      -- ^ the stranger lock, in seconds of the clock
    cOps       :: [Op]
  }
  deriving Show

freshMax :: Int
freshMax = 100_000

allowance :: Allowance
allowance = Allowance freshMax

-- | A kilobyte of fresh bytes a second.
clock :: Time -> Fresh
clock = freshAt 1000 (Time 0)

at :: Double -> Time
at t = Time (realToFrac t)

instance Arbitrary Case where
  arbitrary = do
    cStanding  <- frequency [ (1, pure LocalRoot), (1, pure Partner), (4, pure Other) ]
    k          <- frequency [ (1, pure 0), (2, choose (1, 3)) ]
    cPools     <- vectorOf k (choose (- cap, 3 * cap))
    cUncharged <- sublistOf [2, 3, 8, 18]
    cLock      <- frequency [ (1, pure 0), (3, choose (0, 100)) ]
    n          <- choose (0, 60)
    deltas     <- vectorOf n (choose (0, 5 :: Double))
    cOps       <- forM (scanl1 (+) deltas) $ \t -> frequency
                    [ (3, pure (Ask t))
                    , (3, Charge t <$> elements [2, 3, 4, 8, 10, 18, 19] <*> choose (1, 2 * freshMax))
                    , (1, pure (Rebuild t)) ]
    return Case { cStanding, cPools, cUncharged, cLock, cOps }
    where cap = capacity allowance
  shrink c@Case { cStanding, cPools, cUncharged, cLock, cOps } =
       [ c { cOps = ops }         | ops <- shrinkList (const []) cOps ]
    ++ [ c { cPools = ps }        | ps  <- shrinkList (const []) cPools ]
    ++ [ c { cUncharged = us }    | us  <- shrinkList (const []) cUncharged ]
    ++ [ c { cStanding = Other }  | cStanding /= Other ]
    ++ [ c { cLock = 0 }          | cLock /= 0 ]

-- | The table, stepped by hand with the ranks written out, so the rule's own
-- 'rankFor' is judged rather than echoed: ranks at every ask and the pool
-- buckets at the end. The sink charges whichever bucket the last answer
-- came from: a partner's own at tier 1, the pool's at tier 2, the own at
-- tier 3, nothing at rest; before any answer, as a non-partner.
model :: Case -> ([Rank], [Charged])
model Case { cStanding, cPools, cUncharged, cLock, cOps } =
    go (map Charged cPools) Nothing False cOps
  where
    matched = not (null cPools)
    -- the own bucket: locked until the lock has passed, then empty at the
    -- reading of its first use after that. A use is a rank asked for anyone
    -- but a local root, or a charge that goes to the own bucket: a partner's,
    -- or one without a pool, on a charged protocol.
    openAt t own = case own of
      Just _ -> own
      Nothing | t >= cLock -> Just (emptyCharged (clock (at t)))
              | otherwise  -> Nothing
    go pools _ _ [] = ([], pools)
    go pools own0 asPartner (op : ops) =
      let own = case op of
                  Ask t | cStanding /= LocalRoot -> openAt t own0
                  Charge t num _ | num `notElem` cUncharged, asPartner || not matched -> openAt t own0
                  _ -> own0
      in case op of
           Ask t ->
             let fresh      = clock (at t)
                 ownCredit  = maybe False ((> 0) . credit allowance fresh) own
                 poolCredit = any ((> 0) . credit allowance fresh) pools
                 other | matched    = if poolCredit then Rank 2 else Rank 4
                       | ownCredit  = Rank 3
                       | otherwise  = Rank 4
                 rank = case cStanding of
                             LocalRoot -> Rank 0
                             Partner | ownCredit  -> Rank 1
                                     | otherwise  -> other
                             Other -> other
                 (ranks, final) = go pools own (rank == Rank 1) ops
             in (rank : ranks, final)
           Charge t num n
             | num `elem` cUncharged -> go pools own asPartner ops
             | asPartner -> go pools (charge allowance (clock (at t)) n <$> own) asPartner ops
             | matched   -> go (map (charge allowance (clock (at t)) n) pools) own asPartner ops
             | otherwise -> go pools (charge allowance (clock (at t)) n <$> own) asPartner ops
           Rebuild t ->
             go (map (const (emptyCharged (clock (at t)))) pools) own asPartner ops

-- | The same, through 'mkEgressRule' and the buckets in IOSim: the reading
-- is the clock's at each op's time, and a rebuild installs a list of new
-- pools, so its buckets start empty.
run :: Case -> ([Rank], [Charged])
run Case { cStanding, cPools, cUncharged, cLock, cOps } = runSimOrThrow $ do
    reading <- newTVarIO (Fresh 0)
    pa <- newPoolAllowances (return allowance) (readTVar reading) (realToFrac cLock)
    let key   = "peer" :: String
        k     = length cPools
        index = if k > 0 then Map.singleton key [0 .. k - 1] else Map.empty
    _ <- atomically $ rebuild pa (relaysOf 0 k) index
    buckets0 <- readTVarIO (paBuckets pa)
    forM_ (zip [0 ..] cPools) $ \(i, c) ->
      atomically $ writeTVar (snd (buckets0 IntMap.! i)) (Charged c)
    generation <- newTVarIO (0 :: Int)
    (rule, sink) <- mkEgressRule pa (map MiniProtocolNum cUncharged) key
                                 (const (return cStanding)) (at 0)
    let tick t = writeTVar reading (clock (at t))
    ranks <- fmap concat . forM cOps $ \op ->
      case op of
           Ask t          -> (: []) <$> atomically (tick t >> rule (at t))
           Charge t num n -> [] <$ atomically (tick t >> sink (at t) (MiniProtocolNum num) n)
           Rebuild t      -> [] <$ atomically (do tick t
                                                  g <- stateTVar generation (\g -> (g + 1, g + 1))
                                                  _ <- rebuild pa (relaysOf g k) index
                                                  return ())
    buckets <- readTVarIO (paBuckets pa)
    final   <- mapM (readTVarIO . snd) (IntMap.elems buckets)
    return (ranks, final)

prop_rule :: Case -> Property
prop_rule c@Case { cStanding, cPools, cLock, cOps } =
    classify (cStanding == Other) "one of the rest"
  . classify (cStanding == Partner) "a partner"
  . classify (cStanding == Partner && Rank 1 `elem` fst got && any (/= Rank 1) (fst got))
             "a partner both at its tier and fallen through"
  . classify (cStanding == Partner && not (null cPools) && Rank 2 `elem` fst got)
             "a partner served as its pool's relay"
  . classify (not (null cPools)) "a pool relay"
  . classify lockedStranger "a locked stranger"
  . classify (lockedStranger && any askedAfter cOps) "a stranger asked after its lock"
  . classify (any isRebuild cOps) "the list is rebuilt under it"
  . classify (Rank 4 `elem` fst got) "goes to rest at some point"
  . classify (Rank 2 `elem` fst got) "served as a pool relay at some point"
  . classify (Rank 3 `elem` fst got) "served as a stranger at some point"
  $ got === model c
  where
    got = run c
    lockedStranger = cStanding == Other && null cPools && cLock > 0
    askedAfter (Ask t) = t > cLock
    askedAfter _       = False
    isRebuild Rebuild {} = True
    isRebuild _          = False

-- | A pool relay queues for a batch while its allowance is spent, behind
-- two local roots that keep the budget busy, so at rest it never reaches
-- the budget. Its credit refills with the reading alone, no write to any
-- variable of its own; the same request, never resubmitted, must be served
-- once the rule answers the pool's tier, within a refresh, the floor's time
-- for the batch and a sample. Over budgets, batches, floors, allowances and
-- refill rates: the floor is the relay's only way while the roots keep the
-- budget, and the refresh the only way a queued request learns its tier.
data Refill = Refill {
    rfRate  :: !Double,   -- ^ the budget, bytes/s
    rfCap   :: !Int,      -- ^ its capacity, and the roots' batch
    rfFloor :: !Int,      -- ^ the floor, percent
    rfBatch :: !Int,      -- ^ the relay's batch
    rfFresh :: !Double,   -- ^ fresh bytes a second
    rfMax   :: !Int       -- ^ the allowance: the most one announcement carries
  }
  deriving Show

-- | Budgets and capacities such that the roots take at most a hundred
-- batches a second, and refill rates such that the credit is back within
-- twenty seconds: a few thousand grants a run.
instance Arbitrary Refill where
  arbitrary = do
    rfRate  <- choose (1e3, 1e5)
    rfCap   <- elements [1000, 10000]
    rfFloor <- choose (5, 30)
    rfBatch <- choose (1, rfCap)
    rfFresh <- choose (100, 1e4)
    rfMax   <- choose (10, 1000)
    return Refill { .. }
  shrink rf@Refill { rfCap, rfBatch, rfMax } =
       [ rf { rfBatch = b }          | b <- shrink rfBatch, b >= 1 ]
    ++ [ rf { rfCap = c, rfBatch = min rfBatch c } | c <- [1000], c < rfCap ]
    ++ [ rf { rfMax = m }            | m <- shrink rfMax, m >= 10 ]

prop_refill_reaches_queued :: Refill -> Property
prop_refill_reaches_queued Refill { rfRate, rfCap, rfFloor, rfBatch, rfFresh, rfMax } =
    counterexample (show (entry, firstCredited, served, bound)) $
         counterexample "not at rest while spent" (entry === Rank 4)
    .&&. counterexample "the credit never came back" (property (isJust firstCredited))
    .&&. case (served, firstCredited) of
           (Just servedAt, Just c) ->
             counterexample "served late" (property (servedAt `diffTime` c <= bound))
           _ -> counterexample "never served" (property False)
  where
    floorRate = rfRate * fromIntegral rfFloor / 100
    tick      = 0.01
    samplePeriod = 0.1
    -- a refresh; the floor's time for the batch, after a capacity to
    -- refill; a sample's lateness in noticing the credit; a tick
    bound = Bucket.tierRefresh
          + realToFrac (fromIntegral (rfBatch + rfCap) / floorRate :: Double)
          + samplePeriod + tick
    -- the credit is back once the reading has grown by two allowances
    refill = realToFrac (2 * fromIntegral rfMax / rfFresh :: Double)
    (entry, firstCredited, served) = runSimOrThrow $ do
      budget <- Bucket.newBucket rfRate rfCap (Just (Bucket.Rotation 42 599 4))
      Bucket.attachFloor budget (fromIntegral rfFloor / 100)
      reading <- newTVarIO (Fresh 0)
      t0 <- getMonotonicTime
      clockThread <- async $ forever $ do
        threadDelay tick
        now <- getMonotonicTime
        atomically $ writeTVar reading (freshAt rfFresh t0 now)
      pa <- newPoolAllowances (return (Allowance rfMax)) (readTVar reading) 1
      _ <- atomically $ rebuild pa (relaysOf 0 1) (Map.singleton ("relay" :: String) [0])
      (rule, sink) <- mkEgressRule pa [] "relay" (const (return Other)) t0
      roots <- forM [1 .. 2 :: Int] $ \_ -> do
        h <- Bucket.registerBearer budget
        atomically $ Bucket.setRank h (Rank 0)
        async $ forever $ Bucket.awaitGrant h rfCap
      threadDelay 0.01
      h <- Bucket.registerBearer budget
      now <- getMonotonicTime
      entered <- atomically $ do
        -- spent: whatever the first ticks granted is charged away
        sink now (MiniProtocolNum 2) (capacity (Allowance rfMax))
        Bucket.setRankSource h rule
        rule now
      victim <- async $ Bucket.awaitGrant h rfBatch >> getMonotonicTime
      -- when the rule first answers the pool's tier again, sampled
      credited <- newTVarIO Nothing
      sampler <- async $ forever $ do
        t <- getMonotonicTime
        r <- atomically (rule t)
        when (r /= Rank 4) $ atomically $ modifyTVar credited (maybe (Just t) Just)
        threadDelay samplePeriod
      outcome <- timeout (refill + 3 * bound + 10) (wait victim)
      creditedAt <- readTVarIO credited
      mapM_ cancel (clockThread : sampler : roots)
      return (entered, creditedAt, outcome)

-- | What gives the floor's tier numbers their meaning is 'rankFor'. Over
-- every standing and credit state: a local root has no class, a partner
-- with its own credit is class 0, a pool relay with pool credit class 1, a
-- stranger with its own credit class 2, and a peer without the credit its
-- tier needs has none; and exactly the peers without credit are at or above
-- 'restTier', where the order within a tier is turns. A partner without
-- its own credit is classed as the peer it otherwise is. A renumbering of
-- the tiers on either side fails here.
prop_floorClass_follows_rank :: Property
prop_floorClass_follows_rank = once $ conjoin
  [ counterexample (show (standing, own, pools))
  $ floorClass tier === expected
    .&&. (tier >= restTier) === (expected == Nothing && standing /= LocalRoot)
  | standing <- [LocalRoot, Partner, Other]
  , own      <- [False, True]
  , pools    <- [Nothing, Just False, Just True]
  , let Rank tier = rankFor standing own pools
        other = case pools of
                     Just True        -> Just (FloorClass 1)
                     Just False       -> Nothing
                     Nothing | own    -> Just (FloorClass 2)
                             | otherwise -> Nothing
        expected = case standing of
                        LocalRoot      -> Nothing
                        Partner | own  -> Just (FloorClass 0)
                                | otherwise -> other
                        Other          -> other
  ]

-- | The requirement behind the lock: a stranger's own credit is zero for the
-- lock's length of wall-clock time after its connection, however fast the
-- reading moves; at the lock the bucket opens empty; after it the bucket
-- fills at the fresh rate, so once the reading has moved by two fresh bytes
-- the stranger is served at its own tier. Judged through the rule's answers,
-- for a connection with no pool and no partner standing: rank 4 while dry,
-- rank 3 with credit.
prop_lock_is_time :: Positive Double -> NonNegative Double -> Double -> Positive Double -> Property
prop_lock_is_time (Positive rate) (NonNegative lock) before (Positive delay) =
    counterexample ("early " ++ show early ++ ", at the lock " ++ show opening ++ ", later " ++ show later) $
      conjoin
        [ counterexample "credit before the lock" (lock == 0 .||. early === Rank 4)
        , counterexample "not empty at the lock" (opening === Rank 4)
        , counterexample "no credit after the lock" (rate * delay < 2 .||. later === Rank 3)
        ]
  where
    reading = freshAt rate (Time 0)
    -- some point before the lock (the lock itself when there is none)
    tEarly = lock * (before - fromIntegral (floor before :: Int)) * 0.999
    (early, opening, later) = runSimOrThrow $ do
      readingVar <- newTVarIO (Fresh 0)
      pa <- newPoolAllowances (return allowance) (readTVar readingVar) (realToFrac lock)
      (rule, _) <- mkEgressRule pa [] ("peer" :: String) (const (return Other)) (at 0)
      -- the rule asked at a time, with the clock's reading then
      let ask t = atomically (writeTVar readingVar (reading (at t)) >> rule (at t))
      e <- ask tEarly
      o <- ask lock
      l <- ask (lock + delay)
      return (e, o, l)

-- | The requirement behind reading the allowance at every use: when the chain's
-- maxima shrink, a bucket's credit is capped at the new capacity from the next
-- use on. A pool relay with a full bucket under a large allowance; the
-- allowance is switched to a small one; a charge of at least the small
-- capacity drains the bucket, so the next ask leaves the pool's tier, whereas
-- under the large allowance the same charge would not have.
prop_allowance_follows_reading :: Positive Int -> Positive Int -> Positive Int -> Property
prop_allowance_follows_reading (Positive big) (Positive small0) (Positive extra) =
    counterexample ("after the switch and a charge of " ++ show n ++ ": " ++ show (afterSwitch, stillBig)) $
      conjoin
        [ counterexample "the charge did not drain the bucket at the small allowance" (afterSwitch === Rank 4)
        , counterexample "the same charge drained it at the large allowance" (stillBig === Rank 2)
        ]
  where
    small = min small0 big
    large = Allowance (big + small)          -- strictly larger capacity than the small one
    tiny  = Allowance small
    -- at least the small capacity, short of the large one: drains the one, not the other
    n     = capacity tiny + extra `mod` (capacity large - capacity tiny)
    key = "relay" :: String
    (afterSwitch, stillBig) = runSimOrThrow $ do
      alVar <- newTVarIO large
      pa <- newPoolAllowances (readTVar alVar) (return (Fresh 0)) 0
      _ <- atomically $ rebuild pa (relaysOf 0 1) (Map.singleton key [0])
      buckets <- readTVarIO (paBuckets pa)
      -- the bucket full under the large allowance
      atomically $ writeTVar (snd (buckets IntMap.! 0)) (fullCharged large (Fresh 0))
      (rule, sink) <- mkEgressRule pa [] key (const (return Other)) (at 0)
      r0 <- atomically (rule (at 0))
      -- switch, charge, ask
      a <- atomically $ do
        writeTVar alVar tiny
        sink (at 0) (MiniProtocolNum 2) n
        rule (at 0)
      -- the same from a full bucket under the large allowance alone
      pa' <- newPoolAllowances (return large) (return (Fresh 0)) 0
      _ <- atomically $ rebuild pa' (relaysOf 0 1) (Map.singleton key [0])
      buckets' <- readTVarIO (paBuckets pa')
      atomically $ writeTVar (snd (buckets' IntMap.! 0)) (fullCharged large (Fresh 0))
      (rule', sink') <- mkEgressRule pa' [] key (const (return Other)) (at 0)
      b <- atomically $ do
        _ <- rule' (at 0)
        sink' (at 0) (MiniProtocolNum 2) n
        rule' (at 0)
      return (if r0 == Rank 2 then a else Rank 0, b)
