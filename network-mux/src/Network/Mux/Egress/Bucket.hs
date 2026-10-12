{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A node-global egress token bucket shared by every mux bearer in the
-- process.
--
-- The state is the instant the bucket is full again: the level at @now@ is
-- @capacity - rate * (full - now)@, capped at the capacity, negative in debt.
-- Taking @n@ bytes moves that instant @n / rate@ later.  The arithmetic is the
-- pure 'grantAt', so grant instants are exact.
--
-- Service order is @('Rank', ticket)@: lower rank first, FIFO among equals;
-- with every bearer at 'Rank' 0 (the default) that is an equal share. With a
-- 'Rotation' the rank set by the application is the tier, and within a tier
-- every bearer is dealt a random place that is re-dealt each period; from
-- 'roTurnsFrom' up, the rest of the cascade, bearers take turns instead. A
-- short head sleeps until its bytes are there, at microsecond resolution.
--
-- Fast path: with nobody queued and enough tokens, a take is one transaction.
-- Otherwise the bearer queues and blocks on its own wake variable; the bearer
-- that is granted wakes the next in line.  Only the head waits on the queue.
--
-- A budget may carry a 'Floor': a second, small bucket for the credited
-- bearers the strict order does not reach. A queued bearer with a floor class
-- (see "Network.Mux.Egress.Floor") also waits in the floor's queue, and while
-- it is not the head of the budget's queue it may be served from the floor
-- when it is the floor's pick and the floor has its bytes; the budget is
-- charged on credit, so the floor redistributes the budget and never adds to
-- it. The head of the budget's queue is served by the cascade; it takes from
-- the floor only when the budget is short and it is the floor's pick, so the
-- floor never waits on a bearer that is waiting on the budget.
--
module Network.Mux.Egress.Bucket
  ( Bucket
  , newBucket
  , setBucketRate
  , BucketHandle
  , registerBearer
  , bearerId
  , Rank (..)
  , setRank
  , setRankSource
  , Rotation (..)
  , GrantSource (..)
  , awaitGrant
  , awaitGrantBorrowing
  , awaitGrantWaited
  , awaitGrantCharged
  , takeOnCredit
    -- * The floor
  , Floor
  , attachFloor
  , floorPending
    -- * Counters
  , BucketStats (..)
  , TierGrants (..)
  , waitBounds
  , burstBounds
  , burstWidths
  , emptyBucketStats
  , bucketSnapshot
  , servedWindow
  , tierRefresh
    -- * Bursts
  , Burst (..)
  , burstDuration
  , takeBurstsAt
  , drainBurstsAt
    -- * State, named for the tests' instances
  , Ticket
  , BurstState
  , TierAsked
  , Detector
  , Open
    -- * Pure core
  , tokenLevel
  , grantAt
  , wakeAt
  , atRate
  , rotatedRank
  , queueRank
  , chargeAt
  , recordServed
  , servedWithin
  , servedByTier
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void, when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64, Word8)
import GHC.Generics (Generic)
import System.Random.SplitMix qualified as SM

import Network.Mux.Egress.Burst
import Network.Mux.Egress.Floor
import Network.Mux.Types (unrankedNumber)


-- | A bearer's tier: lower is served first.
newtype Rank = Rank Word8
  deriving (Eq, Ord, Show)

newtype Ticket = Ticket Word64
  deriving (Eq, Ord, Show, Generic)

-- | Where a waiter queues ('queueRank'), then when it joined.
type WaitKey = (Word32, Ticket)

-- | A cancellable timer: whether it fired, and its cancellation.
type Delay m = (STM m TimeoutState, m ())

-- | How the order within a tier is drawn: a node-local seed, and the period
-- after which every bearer is dealt a new place.
data Rotation = Rotation {
  roSeed      :: !Word64,
  roPeriod    :: !DiffTime,  -- ^ e.g. 599 s; zero or less is no rotation
  roTurnsFrom :: !Word8
    -- ^ tiers at or above this take turns instead of a dealt place: every
    -- request queues behind the tier's current waiters, so backlogged bearers
    -- rotate one batch at a time. The rest of the cascade, where finishing one
    -- transfer first serves nobody and a newcomer must not wait out a flood.
  }
  deriving (Show, Generic)

data Bucket m = Bucket {
  bRate     :: !(StrictTVar m Double),   -- ^ bytes/s; @<= 0@ disables the bucket
  bCapacity :: !Int,                     -- ^ bytes; a small multiple of the batch size
  bRotation :: !(Maybe Rotation),        -- ^ order within a tier, if rotating
  bFull     :: !(StrictTVar m Time),     -- ^ the instant the bucket is full again
  bWaiters  :: !(StrictTVar m (Map WaitKey (StrictTVar m Bool))),
    -- ^ queued bearers, by key, with their wake variables
  bTickets  :: !(StrictTVar m Ticket),
  bBearers  :: !(StrictTVar m Word64),   -- ^ next bearer id
  bStats    :: !(StrictTVar m BucketStats),
  bBurst    :: !(StrictTVar m BurstState),
  bDetector :: !(StrictTVar m Detector),  -- ^ the bursts of credited demand
  bFloor    :: !(StrictTVar m (Maybe (Floor m)))
    -- ^ the floor for the credited bearers the order does not reach, if any
  }
  deriving Generic

-- | The floor hung on a budget: its own bucket, charged to the budget on
-- credit; the credited bearers waiting, by class and ticket, with the bytes
-- each needs and its wake variable; and the round-robin's state.
data Floor m = Floor {
  flShare   :: !Double,                  -- ^ of the budget's rate; follows it
  flBucket  :: !(Bucket m),
  flWaiters :: !(StrictTVar m (Map (FloorClass, Ticket) (Int, StrictTVar m Bool))),
  flState   :: !(StrictTVar m FloorState)
  }
  deriving Generic

-- | Where a grant came from.
data GrantSource = FromBucket | FromBorrow | FromFloor
  deriving (Eq, Show)

-- | The busy period in progress, if any: since when credited bearers have
-- been queued, since when two or more, the most at once, and how many now.
-- Bearers at rest rank or unranked wait for what the credited leave, so
-- they are not counted: a queue of them alone is not contention.
data BurstState = BurstState {
  buSince          :: !(Maybe Time),
  buContendedSince :: !(Maybe Time),
  buPeak           :: !Int,
  buCredited       :: !Int
  }
  deriving Generic

-- | What a bucket has handed out since it was created.
data BucketStats = BucketStats {
  bsBytes         :: !Word64,     -- ^ granted, own or borrowed
  bsBatches       :: !Word64,
  bsBorrowed      :: !Word64,     -- ^ of the bytes granted, those borrowed from a budget
  bsCredited      :: !Word64,     -- ^ taken on credit by 'takeOnCredit'
  bsWaitTokens    :: !DiffTime,   -- ^ summed over grants: request to grant
  bsWaitWritable  :: !DiffTime,   -- ^ summed over grants: the writability gate before it
  bsWaitsOver     :: ![Word64],   -- ^ grants whose token wait exceeded each of 'waitBounds'
  bsBursts        :: !Word64,    -- ^ busy periods: from a first credited bearer
                                 --   queued to none
  bsBusyTime      :: !DiffTime,  -- ^ time with a credited bearer queued
  bsContendedTime :: !DiffTime,  -- ^ time with two or more queued, when the order decides
  bsBurstsOver    :: ![Word64],  -- ^ busy periods longer than each of 'burstBounds'
  bsBurstsWider   :: ![Word64],  -- ^ busy periods with at least each of 'burstWidths'
                                 --   credited bearers queued at once
  bsTiers         :: !(Map Word8 TierGrants),
                                 -- ^ what was granted from the queue, by the tier the
                                 --   bearer queued at; the fast path is not ranked
  bsFloorTiers    :: !(Map Word8 TierGrants),
                                 -- ^ of 'bsTiers', what the floor granted
  bsServed        :: !(Map (Word8, Word64) Time),
                                 -- ^ the last grant by tier and bearer id, the
                                 --   fast path included at the tier last asked
                                 --   within 'tierRefresh'; pruned by
                                 --   'recordServed', so never older than two
                                 --   'servedWindow's
  bsServedPruned  :: !Time,      -- ^ when 'bsServed' was last pruned
  bsBurstsDropped :: !Word64     -- ^ burst records closed with no room to keep
                                 --   them; held by the detector, filled in by
                                 --   'bucketSnapshot'
  }
  deriving (Eq, Show, Generic)

-- | The tier a queued bearer's key was made with: the top byte of the rank.
tierOfKey :: WaitKey -> Word8
tierOfKey (rank, _) = fromIntegral (rank `shiftR` 24)

-- | The busy-period lengths 'bsBurstsOver' counts against: a fan-out at a
-- high budget is tens of milliseconds, at a low one seconds.
burstBounds :: [DiffTime]
burstBounds = [0.01, 0.03, 0.1, 0.3, 1]

-- | The queue widths 'bsBurstsWider' counts against.
burstWidths :: [Int]
burstWidths = [2, 4, 8, 16, 32]

-- | The token-wait bounds 'bsWaitsOver' counts against.
waitBounds :: [DiffTime]
waitBounds = [0.001, 0.01, 0.1, 1, 10]

-- | One zero per bound, each evaluated.
zeros :: [a] -> [Word64]
zeros xs = strictZipWith (\_ _ -> 0) xs xs

-- | 'zipWith' with the spine and every element evaluated. The bound counters
-- live in a TVar for the bucket's whole life, where a lazy element would hold
-- every update before it.
strictZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
strictZipWith f (a : as) (b : bs) = let !c  = f a b
                                        !cs = strictZipWith f as bs
                                    in c : cs
strictZipWith _ _ _ = []

emptyBucketStats :: BucketStats
emptyBucketStats = BucketStats 0 0 0 0 0 0 (zeros waitBounds)
                               0 0 0 (zeros burstBounds) (zeros burstWidths)
                               Map.empty Map.empty Map.empty (Time 0) 0

-- | The credited bearers queued went from @n0@ to @n1@ at @now@.
burstStep :: Time -> Int -> Int -> (BurstState, BucketStats) -> (BurstState, BucketStats)
burstStep !now n0 n1 (bu0, st0) = ended (contended (started (bu0, st0)))
  where
    started (bu, st)
      | n0 == 0 && n1 >= 1 = (bu { buSince = Just now, buPeak = n1 },
                              st { bsBursts = bsBursts st + 1 })
      | otherwise          = (bu { buPeak = max (buPeak bu) n1 }, st)
    contended (bu, st)
      | n0 < 2 && n1 >= 2
      = (bu { buContendedSince = Just now }, st)
      | n0 >= 2 && n1 < 2, Just c <- buContendedSince bu
      = (bu { buContendedSince = Nothing },
         st { bsContendedTime = bsContendedTime st + (now `diffTime` c) })
      | otherwise = (bu, st)
    ended (bu, st)
      | n0 >= 1 && n1 == 0, Just since <- buSince bu
      = let len = now `diffTime` since
        in ( bu { buSince = Nothing, buContendedSince = Nothing, buPeak = 0 }
           , st { bsBusyTime    = bsBusyTime st + len
                , bsBurstsOver  = strictZipWith (\b c -> if len > b then c + 1 else c)
                                          burstBounds (bsBurstsOver st)
                , bsBurstsWider = strictZipWith (\w c -> if buPeak bu >= w then c + 1 else c)
                                          burstWidths (bsBurstsWider st) } )
      | otherwise = (bu, st)

recordGrant :: Maybe Word8  -- ^ the tier it queued at, if it queued
            -> GrantSource -> Int -> DiffTime -> DiffTime -> BucketStats -> BucketStats
recordGrant tier_m source need waitedWritable waitedTokens st =
  st { bsBytes        = bsBytes st + n
     , bsBatches      = bsBatches st + 1
     , bsBorrowed     = bsBorrowed st + (if source == FromBorrow then n else 0)
     , bsWaitTokens   = bsWaitTokens st + waitedTokens
     , bsWaitWritable = bsWaitWritable st + waitedWritable
     , bsWaitsOver    = strictZipWith (\b c -> if waitedTokens > b then c + 1 else c)
                                waitBounds (bsWaitsOver st)
     , bsTiers        = case tier_m of
                             Just tier -> Map.insertWith (<>) tier (TierGrants n 1) (bsTiers st)
                             Nothing   -> bsTiers st
     , bsFloorTiers   = case (tier_m, source) of
                             (Just tier, FromFloor) ->
                               Map.insertWith (<>) tier (TierGrants n 1) (bsFloorTiers st)
                             _ -> bsFloorTiers st
     }
  where
    n = fromIntegral need

-- | How long a grant keeps its bearer counted as served at its tier, and how
-- often the record of them is pruned.
servedWindow :: DiffTime
servedWindow = 59

-- | How long a bearer's tier, once asked, labels its unqueued grants in
-- 'bsServed', and how often a queued bearer asks its rule again: as often as
-- the counters read them.
tierRefresh :: DiffTime
tierRefresh = 7

-- | A grant to bearer @b@ at @tier@ at @now@. Once a 'servedWindow' after the
-- last prune it also drops the bearers not granted within the window, so the
-- record stays bounded whether or not anything reads it.
recordServed :: Word8 -> Word64 -> Time -> BucketStats -> BucketStats
recordServed !tier !b now st
  | now `diffTime` bsServedPruned st >= servedWindow
  = st { bsServed       = Map.insert (tier, b) now (servedWithin servedWindow now (bsServed st))
       , bsServedPruned = now }
  | otherwise
  = st { bsServed = Map.insert (tier, b) now (bsServed st) }

-- | The bearers granted within @window@ of @now@.
servedWithin :: DiffTime -> Time -> Map (Word8, Word64) Time -> Map (Word8, Word64) Time
servedWithin window now = Map.filter (\t -> now `diffTime` t <= window)

-- | Bearers served at each tier; one that changed tier counts at each.
servedByTier :: Map (Word8, Word64) a -> Map Word8 Int
servedByTier m = Map.fromListWith (+) [ (tier, 1) | (tier, _) <- Map.keys m ]

-- | The counters, the token level, the number of bearers queued and how many
-- of them at each tier, at @now@.
bucketSnapshot :: MonadSTM m => Bucket m -> Time
               -> STM m (BucketStats, Double, Int, Map Word8 Int)
bucketSnapshot Bucket { bRate, bCapacity, bFull, bWaiters, bStats, bBurst, bDetector } now = do
  rate    <- readTVar bRate
  full    <- readTVar bFull
  waiters <- readTVar bWaiters
  st      <- readTVar bStats
  bu      <- readTVar bBurst
  det     <- readTVar bDetector
  -- a busy period in progress counts up to now, so the times only ever grow
  let upToNow = maybe 0 (now `diffTime`)
      st' = st { bsBusyTime      = bsBusyTime st + upToNow (buSince bu)
               , bsContendedTime = bsContendedTime st + upToNow (buContendedSince bu)
               , bsBurstsDropped = burstsDropped det }
      queuedByTier = Map.fromListWith (+) [ (tierOfKey k, 1) | k <- Map.keys waiters ]
  return (st', tokenLevel rate bCapacity full now, Map.size waiters, queuedByTier)

newBucket :: (MonadSTM m, MonadMonotonicTime m)
          => Double          -- ^ rate, bytes/s
          -> Int             -- ^ capacity, bytes
          -> Maybe Rotation
          -> m (Bucket m)
newBucket rate capacity bRotation = do
  now      <- getMonotonicTime
  bRate    <- newTVarIO rate
  bFull    <- newTVarIO now
  bWaiters <- newTVarIO Map.empty
  bTickets <- newTVarIO (Ticket 0)
  bBearers <- newTVarIO 0
  bStats   <- newTVarIO emptyBucketStats
  bBurst   <- newTVarIO (BurstState Nothing Nothing 0 0)
  bDetector <- newTVarIO emptyDetector
  bFloor   <- newTVarIO Nothing
  return Bucket { bRate, bCapacity = capacity, bRotation, bFull, bWaiters, bTickets, bBearers,
                  bStats, bBurst, bDetector, bFloor }

-- | The bursts closed since the last call, oldest first, the one in
-- progress closed first if it is over by @now@.
takeBurstsAt :: MonadSTM m => Bucket m -> Time -> STM m [Burst]
takeBurstsAt = takeBurstsWith id

-- | The same, the one in progress closed as it stands whether or not it is
-- over: for a test that ends before the queue drains.
drainBurstsAt :: MonadSTM m => Bucket m -> Time -> STM m [Burst]
drainBurstsAt = takeBurstsWith closeOpen

takeBurstsWith :: MonadSTM m => (Detector -> Detector) -> Bucket m -> Time -> STM m [Burst]
takeBurstsWith finish Bucket { bRate, bCapacity, bDetector } now = do
  rate <- readTVar bRate
  det  <- readTVar bDetector
  let (bursts, det') = takeBursts (finish (burstSweep (gapFor rate bCapacity) now det))
  writeTVar bDetector det'
  return bursts

-- | Hang a floor on a budget: its own bucket at @share@ of the budget's
-- rate, following it through 'setBucketRate', with the budget's capacity,
-- shared fairly in bytes across the credited classes.
attachFloor :: (MonadSTM m, MonadMonotonicTime m)
            => Bucket m -> Double -> m ()
attachFloor Bucket { bRate, bCapacity, bFloor } share = do
  rate      <- readTVarIO bRate
  flBucket  <- newBucket (rate * share) bCapacity Nothing
  flWaiters <- newTVarIO Map.empty
  flState   <- newTVarIO newFloorState
  atomically $ writeTVar bFloor (Just $! Floor { flShare = share, flBucket, flWaiters, flState })

-- | How many bearers wait in the floor's queue; for tests.
floorPending :: MonadSTM m => Bucket m -> STM m Int
floorPending Bucket { bFloor } = do
  fl_m <- readTVar bFloor
  case fl_m of
       Nothing                  -> return 0
       Just Floor { flWaiters } -> Map.size <$> readTVar flWaiters

-- | Change the rate, keeping the token level as of @now@; a floor follows at
-- its share.
setBucketRate :: MonadSTM m => Bucket m -> Time -> Double -> STM m ()
setBucketRate Bucket { bRate, bCapacity, bFull, bFloor } now rate' = do
  rate <- readTVar bRate
  full <- readTVar bFull
  writeTVar bRate rate'
  writeTVar bFull (atRate rate rate' bCapacity now full)
  fl_m <- readTVar bFloor
  case fl_m of
       Just Floor { flShare, flBucket } -> setBucketRate flBucket now (rate' * flShare)
       Nothing                          -> return ()


--
-- Pure core
--

-- | Token level at @now@ of a bucket full again at @full@.
tokenLevel :: Double -> Int -> Time -> Time -> Double
tokenLevel rate cap full now =
  fromIntegral cap - rate * realToFrac (max 0 (full `diffTime` now))

-- | For a request of @need@ bytes made at @now@: the grant instant, when the
-- bytes are there (@now@ if already), and the full instant of the bucket
-- after taking them then.  A request larger than the capacity waits for a
-- full bucket and is served on credit.  Rate @<= 0@ disables the bucket.
grantAt :: Double  -- ^ rate, bytes/s
        -> Int     -- ^ capacity
        -> Time    -- ^ full
        -> Time    -- ^ now
        -> Int     -- ^ need
        -> (Time, Time)
grantAt rate cap full now need
  | rate <= 0 = (now, now)
  | otherwise = (ready, accrual need `addTime` max full ready)
  where
    target = min need cap
    -- @target@ bytes are there within @accrual (cap - target)@ of full
    ready  = max now (negate (accrual (cap - target)) `addTime` full)

    accrual :: Int -> DiffTime
    accrual n = realToFrac (fromIntegral n / rate)

-- | When a head short at @now@ until @ready@ checks again: the wait rounded
-- up to a whole microsecond, at least one.
wakeAt :: Time -> Time -> Time
wakeAt now ready = (fromIntegral micros / 1_000_000) `addTime` now
  where
    micros = max 1 (ceiling ((ready `diffTime` now) * 1_000_000)) :: Integer

-- | The full instant with the same token level at another rate.  From or to
-- a disabled bucket, start full.
atRate :: Double  -- ^ old rate
       -> Double  -- ^ new rate
       -> Int     -- ^ capacity
       -> Time    -- ^ now
       -> Time    -- ^ full
       -> Time
atRate rate rate' cap now full
  | rate <= 0 || rate' <= 0 = now
  | otherwise =
      realToFrac ((fromIntegral cap - tokenLevel rate cap full now) / rate') `addTime` now

-- | The full instant after taking @need@ bytes at @now@ without waiting: a
-- bucket that is short goes into debt, a full one starts from @now@.
chargeAt :: Double -> Time -> Time -> Int -> Time
chargeAt rate full now need
  | rate <= 0 = now
  | otherwise = realToFrac (fromIntegral need / rate) `addTime` max full now

-- | The place a bearer is dealt within its tier for the period that contains
-- @now@: the same for every request in that period, a fresh permutation in the
-- next. 24 bits, leaving the top byte of the queue key to the tier. With a
-- period of zero or less there is no rotation: every bearer is dealt place 0,
-- so its tier is served FIFO.
rotatedRank :: Rotation -> Word64 -> Time -> Word32
rotatedRank Rotation { roSeed, roPeriod } bearer (Time now)
  | roPeriod <= 0 = 0
  | otherwise     =
      fst (SM.nextWord32 (SM.mkSMGen (periodSeed `xor` bearer))) .&. 0x00ffffff
  where
    period     = floor (now / roPeriod) :: Word64
    periodSeed = fst (SM.nextWord64 (SM.mkSMGen (roSeed `xor` period)))

-- | Where a bearer queues, lower first: the tier set with 'setRank' in the top
-- byte, and below it the place the rotation deals it, or 0 without one and
-- 0 in the tiers that take turns.
queueRank :: Maybe Rotation -> Word64 -> Rank -> Time -> Word32
queueRank rotation bearer (Rank tier) now =
  fromIntegral tier `shiftL` 24 .|. maybe 0 place rotation
  where
    place ro | tier >= roTurnsFrom ro = 0
             | otherwise              = rotatedRank ro bearer now


data BucketHandle m = BucketHandle {
  bhBucket :: !(Bucket m),
  bhId     :: !Word64,
  bhRank   :: !(StrictTVar m (Time -> STM m Rank)),
  -- ^ the tier, asked as the bearer joins the queue, at that moment
  bhTier   :: !(StrictTVar m TierAsked), -- ^ the tier last asked, for the fast path
  bhWake   :: !(StrictTVar m Bool)       -- ^ set by the bearer ahead of us when it is granted
  }
  deriving Generic

-- | The tier a bearer's rule last answered, and when.
data TierAsked = NeverAsked
               | TierAsked !Word8 !Time
  deriving Generic

registerBearer :: MonadSTM m => Bucket m -> m (BucketHandle m)
registerBearer bucket@Bucket { bBearers } = do
  bhId <- atomically $ stateTVar bBearers (\n -> (n, n + 1))
  -- unranked bearers are served last, so a bearer that queued before its rule
  -- was installed would lose out rather than jump the queue
  BucketHandle bucket bhId <$> newTVarIO (\_ -> return (Rank unrankedNumber)) <*> newTVarIO NeverAsked
                           <*> newTVarIO False

-- | Handed out in registration order, from 0.
bearerId :: BucketHandle m -> Word64
bearerId = bhId

setRank :: MonadSTM m => BucketHandle m -> Rank -> STM m ()
setRank BucketHandle { bhRank, bhTier } rank = do
  writeTVar bhRank (\_ -> return rank)
  writeTVar bhTier NeverAsked

-- | A rule for the tier instead of a value: asked in the transaction that
-- queues the bearer, with that moment, so whatever it reads is current then.
setRankSource :: MonadSTM m => BucketHandle m -> (Time -> STM m Rank) -> STM m ()
setRankSource BucketHandle { bhRank, bhTier } rule = do
  writeTVar bhRank rule
  writeTVar bhTier NeverAsked


-- | Take @need@ bytes if they are there at @now@, else the instant they will
-- be.  The only place the state is touched.
tryTake :: MonadSTM m => Bucket m -> Time -> Int -> STM m (Either Time ())
tryTake Bucket { bRate, bCapacity, bFull } now need = do
  rate <- readTVar bRate
  full <- readTVar bFull
  let (ready, full') = grantAt rate bCapacity full now need
  if ready == now
     then Right () <$ writeTVar bFull full'
     else return (Left ready)

-- | Take @need@ bytes on credit: never waits, the bucket repays later.
takeOnCredit :: MonadSTM m => Bucket m -> Time -> Int -> STM m ()
takeOnCredit bucket@Bucket { bStats } now need = do
  chargeCredit bucket now need
  modifyTVar bStats (\st -> st { bsCredited = bsCredited st + fromIntegral need })

-- | 'takeOnCredit' without counting it: a slice's charge on the budget, which
-- the slice counts itself.
chargeCredit :: MonadSTM m => Bucket m -> Time -> Int -> STM m ()
chargeCredit Bucket { bRate, bFull } now need = do
  rate <- readTVar bRate
  modifyTVar bFull (\full -> chargeAt rate full now need)

-- | Wake the bearer at the head of the queue, if any.
wakeHead :: MonadSTM m => Map WaitKey (StrictTVar m Bool) -> STM m ()
wakeHead waiters =
  case Map.lookupMin waiters of
       Just (_, wake) -> writeTVar wake True
       Nothing        -> return ()


data Attempt = Granted | Borrowed | FromTheFloor | Displaced | ShortUntil !Time

-- | What a bearer that is not the head of the budget's queue found at the
-- floor.
data FloorAttempt = FloorGranted | FloorNotPick | FloorShortUntil !Time | BecameHead

-- | Block until @need@ bytes are granted to this bearer.
awaitGrant :: forall m. (MonadTimer m, MonadMask m)
           => BucketHandle m -> Int -> m ()
awaitGrant h need = void (awaitGrantWaited Nothing 0 h need)

-- | As 'awaitGrant', but when the bearer's own bucket is short the batch may
-- come from @budget@ instead, if nobody is waiting on it and it has the bytes
-- to spare: capacity nobody else is using. A batch our own bucket pays for is
-- charged to @budget@ on credit in the same transaction, so it always counts
-- against the budget. Returns whether the batch was borrowed.
awaitGrantBorrowing :: forall m. (MonadTimer m, MonadMask m)
                    => BucketHandle m -> Bucket m -> Int -> m Bool
awaitGrantBorrowing h budget need = (== FromBorrow) <$> awaitGrantWaited (Just budget) 0 h need

-- | 'awaitGrant' or, given a budget, 'awaitGrantBorrowing', recording how
-- long the bearer already waited for the writability gate before asking.
awaitGrantWaited :: forall m. (MonadTimer m, MonadMask m)
                 => Maybe (Bucket m) -> DiffTime -> BucketHandle m -> Int -> m GrantSource
awaitGrantWaited borrow_m waited = awaitGrantWith borrow_m waited Nothing

-- | 'awaitGrantWaited' with a charge run in the transaction that grants:
-- what the bearer's rank rule spends is spent the instant the bytes are
-- granted. A queued request is granted only at the tier its rule answers
-- then, so a credit that another grant has just spent moves it to the place
-- the rule gives now, before it is served; the fast path, with nobody
-- waiting, is served whatever its rank and charged all the same.
awaitGrantCharged :: forall m. (MonadTimer m, MonadMask m)
                  => Maybe (Bucket m) -> DiffTime -> (Time -> STM m ())
                  -> BucketHandle m -> Int -> m GrantSource
awaitGrantCharged borrow_m waited charge = awaitGrantWith borrow_m waited (Just charge)

awaitGrantWith :: forall m. (MonadTimer m, MonadMask m)
               => Maybe (Bucket m) -> DiffTime -> Maybe (Time -> STM m ())
               -> BucketHandle m -> Int -> m GrantSource
awaitGrantWith borrow_m waitedWritable charge_m
               BucketHandle { bhBucket = bucket@Bucket { bWaiters, bTickets, bRotation, bStats
                                                       , bBurst, bDetector, bRate, bCapacity
                                                       , bFloor }
                            , bhId, bhRank, bhTier, bhWake }
               need =
  -- masked until the exception handler is in place: a bearer killed on its
  -- way into the queue must not leave a dead entry at the head
  mask $ \unmask -> do
    now <- getMonotonicTime

    -- one transaction: take at once if nothing is queued and the tokens are
    -- there, otherwise join the queue.  Nothing can slip in between.
    r <- atomically $ do
      waiters <- readTVar bWaiters
      fast <- if Map.null waiters
                 then takeOrBorrow Nothing now now
                 else return (Left now)
      case fast of
           Right source -> return (Left source)
           Left _ -> do
             -- evaluated, or the queued key holds a thunk for each until compared
             !ticket <- stateTVar bTickets (\t@(Ticket n) -> (t, Ticket (n + 1)))
             tier    <- readTVar bhRank >>= ($ now)
             writeTVar bhTier (TierAsked (tierOf tier) now)
             let !rank = queueRank bRotation bhId tier now
                 key   = (rank, ticket)

             writeTVar bhWake False
             writeTVar bWaiters (Map.insert key bhWake waiters)
             queueChanged now (tierOf tier) 1

             -- a credited bearer also waits at the floor
             fl_m <- readTVar bFloor
             fkey_m <- case (fl_m, floorClass (tierOf tier)) of
               (Just fl, Just cls) -> do
                 modifyTVar (flWaiters fl) (Map.insert (cls, ticket) (need, bhWake))
                 return (Just (fl, (cls, ticket)))
               _ -> return Nothing
             entry <- newTVar (key, fkey_m)
             return (Right entry)

    case r of
         Left source -> return source
         Right entry -> unmask (waitQueued now entry) `onException` cancel entry
  where
    tierOf :: Rank -> Word8
    tierOf (Rank t) = t

    -- each class's first waiter: its ticket and the bytes it needs
    floorHeads :: Map (FloorClass, Ticket) (Int, StrictTVar m Bool) -> Map FloorClass (Ticket, Int)
    floorHeads = fmap (\(t, (n, _)) -> (t, n)) . classHeads

    -- leave the floor's queue, and wake whoever is its pick now
    floorLeave :: Maybe (Floor m, (FloorClass, Ticket)) -> STM m ()
    floorLeave Nothing = return ()
    floorLeave (Just (fl, fkey)) = do
      fw <- Map.delete fkey <$> readTVar (flWaiters fl)
      writeTVar (flWaiters fl) fw
      wakeFloorPick fl fw

    wakeFloorPick :: Floor m -> Map (FloorClass, Ticket) (Int, StrictTVar m Bool) -> STM m ()
    wakeFloorPick fl fw = do
      st <- readTVar (flState fl)
      case floorPick st (floorHeads fw) of
           Nothing -> return ()
           Just (c, t) ->
             case Map.lookup (c, t) fw of
                  Just (_, wake) -> writeTVar wake True
                  Nothing        -> return ()
    -- the floor serves us: the budget pays on credit, we leave both queues,
    -- and if we were the head the next in line takes it
    floorGrant :: Time -> Time -> WaitKey -> Floor m -> (FloorClass, Ticket) -> FloorClass
               -> Map WaitKey (StrictTVar m Bool)
               -> Map (FloorClass, Ticket) (Int, StrictTVar m Bool) -> FloorState -> STM m ()
    floorGrant asked now key fl fkey c waiters fw st = do
      chargeCredit bucket now need
      charge now
      let waiters' = Map.delete key waiters
          fw'      = Map.delete fkey fw
      writeTVar bWaiters waiters'
      queueChanged now (tierOfKey key) (-1)
      when (fmap fst (Map.lookupMin waiters) == Just key) (wakeHead waiters')
      writeTVar (flWaiters fl) fw'
      writeTVar (flState fl) (floorGranted c need st)
      wakeFloorPick fl fw'
      modifyTVar bStats
        ( recordServed (tierOfKey key) bhId now
        . recordGrant (Just (tierOfKey key)) FromFloor need
                      waitedWritable (now `diffTime` asked))
      noteGrant asked now (tierOfKey key)

    -- a bearer of @tier@ joined (+1) or left (-1) the queue at @now@: when
    -- it is credited, account for the busy period and tell the detector
    queueChanged :: Time -> Word8 -> Int -> STM m ()
    queueChanged now tier delta = when (creditedTier tier) $ do
      bu <- readTVar bBurst
      st <- readTVar bStats
      let n0 = buCredited bu
          n1 = n0 + delta
          (bu', st') = burstStep now n0 n1 (bu { buCredited = n1 }, st)
      writeTVar bBurst bu'
      writeTVar bStats st'
      gap <- gapNow
      modifyTVar bDetector (burstQueued gap now n1)

    -- a grant at @now@ to this bearer at @tier@, asked for at @asked@
    noteGrant :: Time -> Time -> Word8 -> STM m ()
    noteGrant asked now tier = do
      gap <- gapNow
      modifyTVar bDetector
        (burstGrant gap now tier bhId need (now `diffTime` asked) waitedWritable)

    gapNow :: STM m DiffTime
    gapNow = (`gapFor` bCapacity) <$> readTVar bRate

    -- take from our bucket; failing that, from the budget if it is idle:
    -- nobody waiting and the bytes there. Idle but short, the wait is until
    -- whichever bucket has the bytes first. Right: taken, and whether it was
    -- borrowed; Left: the instant to check again. A grant is counted here,
    -- with the wait since the request at @asked@ and the tier it queued at.
    takeOrBorrow :: Maybe Word8 -> Time -> Time -> STM m (Either Time GrantSource)
    takeOrBorrow tier_m asked now = do
      r <- takeOrBorrow' now
      case r of
           Right source -> do
             -- a queued grant asked the rule in this transaction; the fast
             -- path asks it now when it charges, so the charge follows the
             -- rank the rule answers at this grant, not one up to a refresh
             -- old
             served <- case (tier_m, charge_m) of
                            (Just tier, _)     -> return tier
                            (Nothing, Just _)  -> askedNow now
                            (Nothing, Nothing) -> fastTier now
             charge now
             modifyTVar bStats
               ( recordServed served bhId now
               . recordGrant tier_m source need waitedWritable (now `diffTime` asked))
             noteGrant asked now served
           Left _ -> return ()
      return r

    -- the fast path is not ranked: a grant that charges nothing labels itself
    -- in 'bsServed' with the tier last asked, asking the rule only once that is
    -- 'tierRefresh' old, so the grant rarely reads what the rule reads
    -- what a grant charges, if anything
    charge :: Time -> STM m ()
    charge now = maybe (return ()) ($ now) charge_m

    -- the rule asked at this instant, and the cache stamped
    askedNow :: Time -> STM m Word8
    askedNow now = do
      tier <- tierOf <$> (readTVar bhRank >>= ($ now))
      writeTVar bhTier (TierAsked tier now)
      return tier

    fastTier :: Time -> STM m Word8
    fastTier now = do
      asked <- readTVar bhTier
      case asked of
           TierAsked tier at | now `diffTime` at < tierRefresh -> return tier
           _ -> do
             tier <- tierOf <$> (readTVar bhRank >>= ($ now))
             writeTVar bhTier (TierAsked tier now)
             return tier

    takeOrBorrow' :: Time -> STM m (Either Time GrantSource)
    takeOrBorrow' now = do
      own <- tryTake bucket now need
      case (own, borrow_m) of
           (Right (), Nothing)       -> return (Right FromBucket)
           (Right (), Just budget)   -> Right FromBucket <$ chargeCredit budget now need
           (Left ready, Nothing)     -> return (Left ready)
           (Left ready, Just budget@Bucket { bWaiters = budgetWaiters }) -> do
             idle <- Map.null <$> readTVar budgetWaiters
             if not idle
                then return (Left ready)
                else do
                  lent <- tryTake budget now need
                  case lent of
                       Right ()         -> return (Right FromBorrow)
                       Left readyBudget -> return (Left (min ready readyBudget))

    -- wait for a grant, asking the rule again every 'tierRefresh': a bearer
    -- whose tier changed while it waited moves to the new tier's place,
    -- keeping its ticket, and joins or leaves the floor with it; one whose
    -- tier is dealt anew in a new period moves to its new place. Credit that
    -- refills with time, a lock that expires, a standing that changes and
    -- the deal all reach a waiting request this way, within a refresh. The
    -- refresh timer goes with the wait: a grant or a cancellation cancels
    -- it, so no timer outlives the request it was set for.
    waitQueued :: Time -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> m GrantSource
    waitQueued asked entry =
      mask $ \unmask -> do
        period <- registerDelayCancellable tierRefresh >>= newTVarIO
        unmask (loop asked period entry) `finally` (readTVarIO period >>= snd)

    -- the refresh fell due: ask the rule again, start the next period, and
    -- say whether the bearer moved. The timer that fell due is spent; the
    -- next is installed masked, so a cancellation cannot land between its
    -- creation and the handler that cancels it.
    refresh :: StrictTVar m (Delay m)
            -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> m Bool
    refresh period entry = do
      now <- getMonotonicTime
      moved <- atomically (askAgain now entry)
      mask_ $ registerDelayCancellable tierRefresh >>= atomically . writeTVar period
      return moved

    -- the rule asked again at @now@: if the place it gives now differs from
    -- the one queued at, the tier or the deal of a new period, move to it
    askAgain :: Time -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> STM m Bool
    askAgain now entry = do
      (key@(_, ticket), fkey_m) <- readTVar entry
      tier <- readTVar bhRank >>= ($ now)
      writeTVar bhTier (TierAsked (tierOf tier) now)
      let !rank = queueRank bRotation bhId tier now
      if rank == fst key
         then return False
         else do
           waiters <- readTVar bWaiters
           let key'     = (rank, ticket)
               waiters' = Map.insert key' bhWake (Map.delete key waiters)
           writeTVar bWaiters waiters'
           when (fmap fst (Map.lookupMin waiters') /= fmap fst (Map.lookupMin waiters))
                (wakeHead waiters')
           -- in or out of the credited, the busy period follows
           when (creditedTier (tierOfKey key) /= creditedTier (tierOf tier)) $ do
             queueChanged now (tierOfKey key) (-1)
             queueChanged now (tierOf tier) 1
           fl_m <- readTVar bFloor
           fkey' <- case (fl_m, floorClass (tierOf tier)) of
             (Just fl, Just cls) -> do
               fw <- readTVar (flWaiters fl)
               let fw' = Map.insert (cls, ticket) (need, bhWake)
                           (maybe fw (\(_, fkey) -> Map.delete fkey fw) fkey_m)
               writeTVar (flWaiters fl) fw'
               wakeFloorPick fl fw'
               return (Just (fl, (cls, ticket)))
             _ -> do
               floorLeave fkey_m
               return Nothing
           writeTVar entry (key', fkey')
           return True

    -- the rule asked again as a grant is about to be made: if the tier it
    -- answers now is not the one the request queued at, the request moves to
    -- its place now, and is not served at a tier it no longer has. A credit
    -- spent by another grant reaches a waiting request here, not a refresh
    -- later. The deal of a new period is left to the refresh.
    tierMoved :: Time -> WaitKey -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> STM m Bool
    tierMoved now key entry = do
      tier <- tierOf <$> (readTVar bhRank >>= ($ now))
      if tier == tierOfKey key
         then return False
         else askAgain now entry

    -- block until @ready@ holds; at each refresh in between the rule is
    -- asked again, and the wait goes on unless the bearer moved. The timer
    -- or wake-up waited for is the caller's and survives a refresh, so a
    -- bearer that stays put wakes exactly when it would have.
    awaitOr :: StrictTVar m (Delay m)
            -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> STM m () -> m ()
    awaitOr period entry ready = do
      (due, _) <- readTVarIO period
      isDue <- atomically $ (False <$ ready) `orElse` (True <$ fired due)
      when isDue $ do
        moved <- refresh period entry
        if moved then return () else awaitOr period entry ready

    -- a timer's due check
    fired :: STM m TimeoutState -> STM m ()
    fired due = due >>= check . (== TimeoutFired)

    -- sleep until @ready@, or until @early@ holds, through the refreshes in
    -- between; the sleep's timer is cancelled with it
    sleepOr :: Time -> Time -> StrictTVar m (Delay m)
            -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> STM m () -> m ()
    sleepOr now ready period entry early =
      bracket (registerDelayCancellable (wakeAt now ready `diffTime` now)) snd $ \(due, _) ->
        awaitOr period entry (fired due `orElse` early)

    -- the one wake-up a queued bearer blocks on: its own variable, set by the
    -- bearer ahead of it when that one is granted or gives up, or by the
    -- floor when it is the pick
    woken :: STM m ()
    woken = do
      w <- readTVar bhWake
      check w
      writeTVar bhWake False

    loop :: Time -> StrictTVar m (Delay m)
         -> StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> m GrantSource
    loop asked period entry = do
      (key, fkey_m) <- readTVarIO entry
      atHead <- atomically $
        (== Just key) . fmap fst . Map.lookupMin <$> readTVar bWaiters

      if not atHead
         then case fkey_m of
              Nothing -> do
                -- block on our own wake variable and nothing else, so a change
                -- to the queue wakes the one bearer it concerns
                awaitOr period entry woken
                loop asked period entry
              Just (fl, fkey) -> do
                -- not the head: the floor may serve us if we are its pick
                now <- getMonotonicTime
                r <- atomically $ do
                  waiters <- readTVar bWaiters
                  stale <- tierMoved now key entry
                  if stale || fmap fst (Map.lookupMin waiters) == Just key
                     then return BecameHead
                     else do
                       fw <- readTVar (flWaiters fl)
                       st <- readTVar (flState fl)
                       case floorPick st (floorHeads fw) of
                            Just (c, t) | (c, t) == fkey -> do
                              taken <- tryTake (flBucket fl) now need
                              case taken of
                                   Left ready -> return (FloorShortUntil ready)
                                   Right () -> do
                                     floorGrant asked now key fl fkey c waiters fw st
                                     return FloorGranted
                            _ -> return FloorNotPick
                case r of
                     FloorGranted -> return FromFloor
                     BecameHead   -> loop asked period entry
                     FloorNotPick -> do
                       awaitOr period entry woken
                       loop asked period entry
                     FloorShortUntil ready -> do
                       -- sleep until the floor has the bytes; wake early if
                       -- the queues move us
                       sleepOr now ready period entry (readTVar bhWake >>= check)
                       atomically $ writeTVar bhWake False
                       loop asked period entry
         else do
           now <- getMonotonicTime
           r <- atomically $ do
             waiters <- readTVar bWaiters

             stale <- tierMoved now key entry
             if stale || fmap fst (Map.lookupMin waiters) /= Just key
               then return Displaced
               else do
                 taken <- takeOrBorrow (Just (tierOfKey key)) asked now
                 case taken of
                      Right source -> do
                        let waiters' = Map.delete key waiters

                        writeTVar bWaiters waiters'
                        queueChanged now (tierOfKey key) (-1)
                        wakeHead waiters'
                        floorLeave fkey_m

                        return (if source == FromBorrow then Borrowed else Granted)
                      Left ready -> case fkey_m of
                        Nothing -> return (ShortUntil ready)
                        -- the budget is short: if we are the floor's pick, the
                        -- floor serves us, or we wait for whichever is first
                        Just (fl, fkey) -> do
                          fw <- readTVar (flWaiters fl)
                          st <- readTVar (flState fl)
                          case floorPick st (floorHeads fw) of
                               Just (c, t) | (c, t) == fkey -> do
                                 floorTaken <- tryTake (flBucket fl) now need
                                 case floorTaken of
                                      Right () -> do
                                        floorGrant asked now key fl fkey c waiters fw st
                                        return FromTheFloor
                                      Left floorReady -> return (ShortUntil (min ready floorReady))
                               _ -> return (ShortUntil ready)
           case r of
                Granted          -> return FromBucket
                Borrowed         -> return FromBorrow
                FromTheFloor     -> return FromFloor
                Displaced        -> loop asked period entry
                ShortUntil ready -> do
                  -- sleep until the bytes are there; wake early if a lower
                  -- key takes the head, or the floor makes us its pick
                  sleepOr now ready period entry $
                      (readTVar bWaiters >>= check . (/= Just key) . fmap fst . Map.lookupMin)
                    `orElse`
                      (readTVar bhWake >>= check)
                  atomically $ writeTVar bhWake False
                  loop asked period entry

    -- on cancellation leave the queues, under whatever keys we hold now; if
    -- we were the head, pass the baton. A cancellation that lands after the
    -- grant's transaction and before the mask is back finds the request
    -- already gone, and must leave the counts alone.
    cancel :: StrictTVar m (WaitKey, Maybe (Floor m, (FloorClass, Ticket))) -> m ()
    cancel entry = do
     now <- getMonotonicTime
     atomically $ do
      (key, fkey_m) <- readTVar entry
      waiters <- readTVar bWaiters

      when (Map.member key waiters) $ do
        let wasHead  = fmap fst (Map.lookupMin waiters) == Just key
            waiters' = Map.delete key waiters

        writeTVar bWaiters waiters'
        queueChanged now (tierOfKey key) (-1)
        when wasHead (wakeHead waiters')
        floorLeave fkey_m
