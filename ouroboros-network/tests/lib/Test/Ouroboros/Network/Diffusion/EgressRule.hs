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
import Data.Word (Word16, Word8)
import Network.Mux (MiniProtocolNum (..), Rank (..), Tier (..), numberTier,
           tierNumber)
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
    , testProperty "a grant charges its current rank after standing or rank changes" prop_granted_rank
    , testProperty "a stranger's lock is wall-clock time, whatever the reading does" prop_lock_is_time
    , testProperty "the allowance is read at every use, so a smaller one caps a charge at once" prop_allowance_follows_reading
    , testProperty "the floor's classes and the turns threshold follow the rule's tiers" prop_floorClass_follows_rank
    , testProperty "a relay queued dry is served once its credit refills" prop_refill_reaches_queued
    ]

-- | What happens to a connection, at a time in seconds: a batch is granted
-- at the rank the rule answers, in one transaction, an empty batch being a
-- bare query; a batch is granted at a rank given without asking the rule;
-- the pool list is rebuilt under it; or its standing changes.
data Op = Charge Double [(Word16, Int)]
        | ChargeAt Double Word8 [(Word16, Int)]
        | Rebuild Double
        | SetStanding Standing
  deriving Show

data Case = Case {
    cStanding  :: Standing,    -- ^ the standing until a 'SetStanding'
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

rankOf :: Tier -> Rank
rankOf = Rank . tierNumber

instance Arbitrary Case where
  arbitrary = do
    cStanding <- elements [LocalRoot, Partner, Other]
    k <- choose (0, 3)
    cPools <- vectorOf k (choose (- capacity allowance, 3 * capacity allowance))
    cUncharged <- sublistOf [2, 3, 8, 18]
    cLock <- frequency [(1, pure 0), (3, choose (0, 100))]
    n <- choose (0, 60)
    deltas <- vectorOf n (choose (0, 5 :: Double))
    cOps <- forM (scanl1 (+) deltas) $ \t -> frequency
      [ (2, pure (Charge t []))
      , (3, Charge t <$> batch)
      , (3, ChargeAt t <$> rank <*> batch)
      , (1, pure (Rebuild t))
      , (1, SetStanding <$> elements [LocalRoot, Partner, Other]) ]
    return Case { cStanding, cPools, cUncharged, cLock, cOps }
    where
      rank = frequency [(4, tierNumber <$> elements [minBound .. maxBound]), (1, arbitrary)]
      batch = do
        k <- choose (1, 3)
        vectorOf k ((,) <$> elements [2, 3, 4, 8, 10, 18, 19] <*> choose (1, 2 * freshMax))
  shrink c@Case { cStanding, cPools, cUncharged, cLock, cOps } =
       [ c { cOps = ops } | ops <- shrinkList shrinkOp cOps ]
    ++ [ c { cPools = ps } | ps <- shrinkList (const []) cPools ]
    ++ [ c { cUncharged = us } | us <- shrinkList (const []) cUncharged ]
    ++ [ c { cStanding = Other } | cStanding /= Other ]
    ++ [ c { cLock = 0 } | cLock /= 0 ]
    where
      batches = shrinkList (\(num, n) -> [(num, n') | n' <- shrink n, n' > 0])
      shrinkOp (Charge t batch) = [Charge t bs | bs <- batches batch]
      shrinkOp (ChargeAt t rank batch) =
           [ChargeAt t rank bs | bs <- batches batch]
        ++ [ChargeAt t r batch | r <- shrink rank]
      shrinkOp _ = []

-- | The table, stepped by hand with the ranks written out, so the rule's own
-- 'rankFor' is judged rather than echoed: the standing and rank at every
-- grant, and the pool buckets at the end. Only a query opens the own
-- bucket. A grant charges by its rank alone, whatever was asked before: the
-- own bucket at the partner and stranger tiers, the pool's at the pool
-- tier, nothing otherwise. A batch is charged once, its charged protocols'
-- bytes summed.
model :: Case -> ([(Standing, Rank)], [Charged])
model Case { cStanding, cPools, cUncharged, cLock, cOps } =
    go cStanding (map Charged cPools) Nothing cOps
  where
    matched = not (null cPools)
    openAt t own = case own of
      Just _ -> own
      Nothing | t >= cLock -> Just (emptyCharged (clock (at t)))
              | otherwise -> Nothing
    go _ pools _ [] = ([], pools)
    go standing pools own (op : ops) =
      let query t =
            let own' = if standing == LocalRoot then own else openAt t own
                fresh = clock (at t)
                credited = maybe False ((> 0) . credit allowance fresh) own'
                other | matched = if any ((> 0) . credit allowance fresh) pools then PoolTier else RestTier
                      | credited = StrangerTier
                      | otherwise = RestTier
                tier = case standing of
                  LocalRoot -> LocalRootTier
                  Partner | credited -> PartnerTier
                          | otherwise -> other
                  Other -> other
            in (own', rankOf tier)
          grant t rank@(Rank r) batch own' =
            let n = sum [bytes | (num, bytes) <- batch, num `notElem` cUncharged]
                spend = charge allowance (clock (at t)) n
                balances = case numberTier r of
                  Just LocalRootTier -> (pools, own')
                  Just PartnerTier   -> (pools, spend <$> own')
                  Just PoolTier      -> (map spend pools, own')
                  Just StrangerTier  -> (pools, spend <$> own')
                  Just RestTier      -> (pools, own')
                  Nothing            -> (pools, own')
                (pools', own'') = if n <= 0 then (pools, own') else balances
                (observed, final) = go standing pools' own'' ops
            in ((standing, rank) : observed, final)
      in case op of
        Charge t batch -> let (own', rank) = query t in grant t rank batch own'
        ChargeAt t r batch -> grant t (Rank r) batch own
        Rebuild t -> go standing (map (const (emptyCharged (clock (at t)))) pools) own ops
        SetStanding s -> go s pools own ops

-- | The same, through 'mkEgressRule' and the buckets in IOSim: the reading
-- is the clock's at each op's time, and a rebuild installs a list of new
-- pools, so its buckets start empty.
run :: Case -> ([(Standing, Rank)], [Charged])
run Case { cStanding, cPools, cUncharged, cLock, cOps } = runSimOrThrow $ do
    reading <- newTVarIO (Fresh 0)
    pa <- newPoolAllowances (return allowance) (readTVar reading) (realToFrac cLock)
    let key = "peer" :: String
        k = length cPools
        index = Map.singleton key [0 .. k - 1]
    _ <- atomically $ rebuild pa (relaysOf 0 k) index
    buckets0 <- readTVarIO (paBuckets pa)
    forM_ (zip [0 ..] cPools) $ \(i, c) ->
      atomically $ writeTVar (snd (buckets0 IntMap.! i)) (Charged c)
    standing <- newTVarIO cStanding
    generation <- newTVarIO (0 :: Int)
    (rule, sink) <- mkEgressRule pa (map MiniProtocolNum cUncharged) key
                                 (const (readTVar standing)) (at 0)
    observed <- fmap concat . forM cOps $ \op -> atomically $ do
      let grant t rank batch = do
            sink (at t) rank (Map.fromListWith (+) [(MiniProtocolNum num, n) | (num, n) <- batch])
            s <- readTVar standing
            return [(s, rank)]
          tick t = writeTVar reading (clock (at t))
      case op of
        Charge t batch     -> tick t >> rule (at t) >>= \rank -> grant t rank batch
        ChargeAt t r batch -> tick t >> grant t (Rank r) batch
        Rebuild t          -> do tick t
                                 g <- stateTVar generation (\g -> (g + 1, g + 1))
                                 [] <$ rebuild pa (relaysOf g k) index
        SetStanding s      -> [] <$ writeTVar standing s
    buckets <- readTVarIO (paBuckets pa)
    final <- mapM (readTVarIO . snd) (IntMap.elems buckets)
    return (observed, final)

prop_rule :: Case -> Property
prop_rule c@Case { cOps } =
    classify (not (null (ranksFor LocalRoot))) "local root observed"
  . classify (not (null (ranksFor Partner))) "partner observed"
  . classify (rankOf PartnerTier `elem` ranksFor Partner && any (/= rankOf PartnerTier) (ranksFor Partner))
             "partner observed both at partner and another tier"
  . classify (rankOf PoolTier `elem` ranksFor Partner) "partner observed at pool tier"
  . classify (any isOverride cOps) "explicit grant rank"
  . classify (any isRebuild cOps) "pool list rebuilt"
  . classify (any spansProtocols cOps) "batch spans protocols"
  . classify (any changesStanding cOps) "standing changes"
  $ got === model c
  where
    got = run c
    ranksFor s = [rank | (standing, rank) <- fst got, standing == s]
    isOverride ChargeAt {} = True
    isOverride _           = False
    isRebuild Rebuild {} = True
    isRebuild _          = False
    spansProtocols (Charge _ batch)     = length (Map.fromList batch) > 1
    spansProtocols (ChargeAt _ _ batch) = length (Map.fromList batch) > 1
    spansProtocols _                    = False
    changesStanding SetStanding {} = True
    changesStanding _              = False

-- | One grant through the bucket, its standing changed, or its rank fixed,
-- after the rule is installed: before a fast-path grant, or while the
-- request waits at the head of the queue. The queue barrier and a fixed
-- reading take scheduling choices out of the oracle. A probe as a partner
-- afterwards observes the own allowance even when there are pools.
data GrantCase = GrantCase {
    gcFreshMax :: Int,         -- ^ the allowance
    gcNeed     :: Int,         -- ^ the batch, and the bucket's rate and capacity
    gcPools    :: Bool,        -- ^ whether the peer has a pool bucket
    gcInitial  :: Standing,    -- ^ the standing when the rule is installed
    gcFinal    :: Standing,    -- ^ the standing at the grant
    gcQueued   :: Bool,        -- ^ granted from the queue rather than the fast path
    gcOverride :: Bool,        -- ^ the rank is also fixed to the final tier
    gcChangeAt :: Int          -- ^ tenths of a second into the wait, when queued
  }
  deriving Show

instance Arbitrary GrantCase where
  arbitrary = do
    gcFreshMax <- choose (1, 10000)
    gcNeed <- choose (1, 2 * capacity (Allowance gcFreshMax))
    gcPools <- arbitrary
    gcInitial <- elements [LocalRoot, Partner, Other]
    gcFinal <- elements [LocalRoot, Partner, Other]
    gcQueued <- arbitrary
    gcOverride <- arbitrary
    gcChangeAt <- choose (1, 9)
    return GrantCase { gcFreshMax, gcNeed, gcPools, gcInitial, gcFinal, gcQueued, gcOverride, gcChangeAt }
  shrink c@GrantCase { gcFreshMax, gcNeed, gcPools, gcInitial, gcFinal, gcQueued, gcOverride, gcChangeAt } =
       [c { gcFreshMax = n } | n <- shrink gcFreshMax, n > 0]
    ++ [c { gcNeed = n } | n <- shrink gcNeed, n > 0]
    ++ [c { gcPools = False } | gcPools]
    ++ [c { gcInitial = LocalRoot } | gcInitial /= LocalRoot]
    ++ [c { gcFinal = LocalRoot } | gcFinal /= LocalRoot]
    ++ [c { gcQueued = False } | gcQueued]
    ++ [c { gcOverride = False } | gcOverride]
    ++ [c { gcChangeAt = n } | n <- shrink gcChangeAt, n > 0]

prop_granted_rank :: GrantCase -> Property
prop_granted_rank c@GrantCase { gcFreshMax, gcNeed, gcPools, gcInitial, gcFinal, gcQueued, gcOverride, gcChangeAt } =
    classify (gcQueued && gcInitial /= gcFinal) "standing changes while queued"
  . classify gcOverride "rank overridden"
  . classify gcPools "shared pool and private allowance"
  $ counterexample (show (c, got)) $ got === expected
  where
    al = Allowance gcFreshMax
    fresh = Fresh (2 * gcFreshMax)
    currentTier = case gcFinal of
      LocalRoot -> LocalRootTier
      Partner   -> PartnerTier
      Other     -> if gcPools then PoolTier else StrangerTier
    spend = charge al fresh gcNeed (Charged 0)
    ownCharged = case currentTier of
      LocalRootTier -> Charged 0
      PartnerTier   -> spend
      PoolTier      -> Charged 0
      StrangerTier  -> spend
      RestTier      -> Charged 0
    poolCharged = if currentTier == PoolTier then spend else Charged 0
    ownRemaining = credit al fresh ownCharged
    fallback = rankOf (if gcPools && credit al fresh poolCharged > 0 then PoolTier else RestTier)
    expected = (rankOf currentTier, [poolCharged | gcPools],
                if ownRemaining > 0 then rankOf PartnerTier else fallback, fallback)
    got = runSimOrThrow $ do
      reading <- newTVarIO (Fresh 0)
      standing <- newTVarIO Partner
      pa <- newPoolAllowances (return al) (readTVar reading) 0
      _ <- atomically $ rebuild pa (relaysOf 0 (fromEnum gcPools))
                                   (Map.singleton ("peer" :: String) [0 | gcPools])
      (rule, sink) <- mkEgressRule pa [] "peer" (const (readTVar standing)) (Time 0)
      atomically $ do
        _ <- rule (Time 0) -- open the private allowance before it earns credit
        writeTVar reading fresh
        writeTVar standing gcInitial
        _ <- rule (Time 0)
        return ()
      bucket <- Bucket.newBucket (fromIntegral gcNeed) gcNeed Nothing
      h <- Bucket.registerBearer bucket
      chargedRank <- newEmptyTMVarIO
      let grant = Bucket.awaitGrantCharged Nothing 0 0
            (\now rank -> sink now rank (Map.singleton (MiniProtocolNum 2) gcNeed)
                        >> putTMVar chargedRank rank) h gcNeed
          change = atomically $ do
            writeTVar standing gcFinal
            when gcOverride $ Bucket.setRank h (rankOf currentTier)
      atomically $ Bucket.setRankSource h rule
      if gcQueued then do
        atomically $ Bucket.takeOnCredit bucket (Time 0) gcNeed
        worker <- async grant
        atomically $ do
          (_, _, queued, _) <- Bucket.bucketSnapshot bucket (Time 0)
          check (queued == 1)
        threadDelay (fromIntegral gcChangeAt / 10)
        change
        _ <- wait worker
        return ()
      else do
        change
        _ <- grant
        return ()
      rank <- atomically $ readTMVar chargedRank
      now <- getMonotonicTime
      pools <- readTVarIO (paBuckets pa)
      totals <- mapM (readTVarIO . snd) (IntMap.elems pools)
      (oneLeft, noneLeft) <- atomically $ do
        writeTVar standing Partner
        sink now (rankOf PartnerTier) (Map.singleton (MiniProtocolNum 2) (max 0 (ownRemaining - 1)))
        one <- rule now
        sink now (rankOf PartnerTier) (Map.singleton (MiniProtocolNum 2) 1)
        none <- rule now
        return (one, none)
      return (rank, totals, oneLeft, noneLeft)

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
        rank <- rule now
        -- spent: whatever the first ticks granted is charged away
        sink now rank (Map.singleton (MiniProtocolNum 2) (capacity (Allowance rfMax)))
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
        rank <- rule (at 0)
        sink (at 0) rank (Map.singleton (MiniProtocolNum 2) n)
        rule (at 0)
      -- the same from a full bucket under the large allowance alone
      pa' <- newPoolAllowances (return large) (return (Fresh 0)) 0
      _ <- atomically $ rebuild pa' (relaysOf 0 1) (Map.singleton key [0])
      buckets' <- readTVarIO (paBuckets pa')
      atomically $ writeTVar (snd (buckets' IntMap.! 0)) (fullCharged large (Fresh 0))
      (rule', sink') <- mkEgressRule pa' [] key (const (return Other)) (at 0)
      b <- atomically $ do
        rank <- rule' (at 0)
        sink' (at 0) rank (Map.singleton (MiniProtocolNum 2) n)
        rule' (at 0)
      return (if r0 == Rank 2 then a else Rank 0, b)
