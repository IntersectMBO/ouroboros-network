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
-- every bearer is dealt a random place that is re-dealt each period. A short
-- head sleeps until its bytes are there, at microsecond resolution.
--
-- Fast path: with nobody queued and enough tokens, a take is one transaction.
-- Otherwise the bearer queues and blocks on its own wake variable; the bearer
-- that is granted wakes the next in line.  Only the head waits on the queue.
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
  , awaitGrant
  , awaitGrantBorrowing
  , awaitGrantWaited
  , takeOnCredit
    -- * Counters
  , BucketStats (..)
  , waitBounds
  , burstBounds
  , burstWidths
  , emptyBucketStats
  , bucketSnapshot
    -- * Pure core
  , tokenLevel
  , grantAt
  , wakeAt
  , atRate
  , rotatedRank
  , queueRank
  , chargeAt
  ) where

import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (void, when, join)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Data.Bits (shiftL, xor, (.&.), (.|.))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64, Word8)
import System.Random.SplitMix qualified as SM


-- | A bearer's tier: lower is served first.
newtype Rank = Rank Word8
  deriving (Eq, Ord, Show)

newtype Ticket = Ticket Word64
  deriving (Eq, Ord, Show)

-- | Where a waiter queues ('queueRank'), then when it joined.
type WaitKey = (Word32, Ticket)

-- | How the order within a tier is drawn: a node-local seed, and the period
-- after which every bearer is dealt a new place.
data Rotation = Rotation {
  roSeed   :: !Word64,
  roPeriod :: !DiffTime      -- ^ e.g. 599 s; zero or less is no rotation
  }
  deriving Show

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
  bBurst    :: !(StrictTVar m BurstState)
  }

-- | The busy period in progress, if any: since when bearers have been queued,
-- since when two or more, and the most at once.
data BurstState = BurstState {
  buSince          :: !(Maybe Time),
  buContendedSince :: !(Maybe Time),
  buPeak           :: !Int
  }

-- | What a bucket has handed out since it was created.
data BucketStats = BucketStats {
  bsBytes        :: !Word64,     -- ^ granted, own or borrowed
  bsBatches      :: !Word64,
  bsBorrowed     :: !Word64,     -- ^ of the bytes granted, those borrowed from a budget
  bsCredited     :: !Word64,     -- ^ taken on credit by 'takeOnCredit'
  bsWaitTokens   :: !DiffTime,   -- ^ summed over grants: request to grant
  bsWaitWritable :: !DiffTime,   -- ^ summed over grants: the writability gate before it
  bsWaitsOver    :: ![Word64],   -- ^ grants whose token wait exceeded each of 'waitBounds'
  bsBursts        :: !Word64,    -- ^ busy periods: from a first bearer queued to none
  bsBusyTime      :: !DiffTime,  -- ^ time with a bearer queued
  bsContendedTime :: !DiffTime,  -- ^ time with two or more queued, when the order decides
  bsBurstsOver    :: ![Word64],  -- ^ busy periods longer than each of 'burstBounds'
  bsBurstsWider   :: ![Word64]   -- ^ busy periods with at least each of 'burstWidths'
                                 --   bearers queued at once
  }
  deriving (Eq, Show)

-- | The busy-period lengths 'bsBurstsOver' counts against.
burstBounds :: [DiffTime]
burstBounds = [0.1, 1, 10, 60]

-- | The queue widths 'bsBurstsWider' counts against.
burstWidths :: [Int]
burstWidths = [2, 8, 32]

-- | The token-wait bounds 'bsWaitsOver' counts against.
waitBounds :: [DiffTime]
waitBounds = [0.001, 0.01, 0.1, 1, 10]

emptyBucketStats :: BucketStats
emptyBucketStats = BucketStats 0 0 0 0 0 0 (map (const 0) waitBounds)
                               0 0 0 (map (const 0) burstBounds) (map (const 0) burstWidths)

-- | The queue went from @n0@ to @n1@ bearers at @now@.
burstStep :: Time -> Int -> Int -> (BurstState, BucketStats) -> (BurstState, BucketStats)
burstStep now n0 n1 (bu0, st0) = ended (contended (started (bu0, st0)))
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
        in ( BurstState Nothing Nothing 0
           , st { bsBusyTime    = bsBusyTime st + len
                , bsBurstsOver  = zipWith (\b c -> if len > b then c + 1 else c)
                                          burstBounds (bsBurstsOver st)
                , bsBurstsWider = zipWith (\w c -> if buPeak bu >= w then c + 1 else c)
                                          burstWidths (bsBurstsWider st) } )
      | otherwise = (bu, st)

recordGrant :: Bool -> Int -> DiffTime -> DiffTime -> BucketStats -> BucketStats
recordGrant borrowed need waitedWritable waitedTokens st =
  st { bsBytes        = bsBytes st + n
     , bsBatches      = bsBatches st + 1
     , bsBorrowed     = bsBorrowed st + (if borrowed then n else 0)
     , bsWaitTokens   = bsWaitTokens st + waitedTokens
     , bsWaitWritable = bsWaitWritable st + waitedWritable
     , bsWaitsOver    = zipWith (\b c -> if waitedTokens > b then c + 1 else c)
                                waitBounds (bsWaitsOver st)
     }
  where
    n = fromIntegral need

-- | The counters, the token level and the number of bearers queued, at @now@.
bucketSnapshot :: MonadSTM m => Bucket m -> Time -> STM m (BucketStats, Double, Int)
bucketSnapshot Bucket { bRate, bCapacity, bFull, bWaiters, bStats, bBurst } now = do
  rate    <- readTVar bRate
  full    <- readTVar bFull
  waiters <- readTVar bWaiters
  st      <- readTVar bStats
  bu      <- readTVar bBurst
  -- a busy period in progress counts up to now, so the times only ever grow
  let upToNow = maybe 0 (now `diffTime`)
      st' = st { bsBusyTime      = bsBusyTime st + upToNow (buSince bu)
               , bsContendedTime = bsContendedTime st + upToNow (buContendedSince bu) }
  return (st', tokenLevel rate bCapacity full now, Map.size waiters)

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
  bBurst   <- newTVarIO (BurstState Nothing Nothing 0)
  return Bucket { bRate, bCapacity = capacity, bRotation, bFull, bWaiters, bTickets, bBearers,
                  bStats, bBurst }

-- | Change the rate, keeping the token level as of @now@.
setBucketRate :: MonadSTM m => Bucket m -> Time -> Double -> STM m ()
setBucketRate Bucket { bRate, bCapacity, bFull } now rate' = do
  rate <- readTVar bRate
  full <- readTVar bFull
  writeTVar bRate rate'
  writeTVar bFull (atRate rate rate' bCapacity now full)


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
-- byte, and below it the place the rotation deals it, or 0 without one.
queueRank :: Maybe Rotation -> Word64 -> Rank -> Time -> Word32
queueRank rotation bearer (Rank tier) now =
  fromIntegral tier `shiftL` 24 .|. maybe 0 (\ro -> rotatedRank ro bearer now) rotation


data BucketHandle m = BucketHandle {
  bhBucket :: !(Bucket m),
  bhId     :: !Word64,
  bhRank   :: !(StrictTVar m (STM m Rank)),   -- ^ the tier, asked as the bearer
                                              --   joins the queue
  bhWake   :: !(StrictTVar m Bool)       -- ^ set by the bearer ahead of us when it is granted
  }

registerBearer :: MonadSTM m => Bucket m -> m (BucketHandle m)
registerBearer bucket@Bucket { bBearers } = do
  bhId <- atomically $ stateTVar bBearers (\n -> (n, n + 1))
  BucketHandle bucket bhId <$> newTVarIO (return (Rank 0)) <*> newTVarIO False

-- | Handed out in registration order, from 0.
bearerId :: BucketHandle m -> Word64
bearerId = bhId

setRank :: MonadSTM m => BucketHandle m -> Rank -> STM m ()
setRank BucketHandle { bhRank } rank = writeTVar bhRank (return rank)

-- | A rule for the tier instead of a value: asked in the transaction that
-- queues the bearer, so whatever it reads is current at that moment.
setRankSource :: MonadSTM m => BucketHandle m -> STM m Rank -> STM m ()
setRankSource BucketHandle { bhRank } = writeTVar bhRank


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


data Attempt = Granted | Borrowed | Displaced | ShortUntil !Time

-- | Block until @need@ bytes are granted to this bearer.
awaitGrant :: forall m. (MonadTimer m, MonadMask m)
           => BucketHandle m -> Int -> m ()
awaitGrant h need = void (awaitGrantWith Nothing 0 h need)

-- | As 'awaitGrant', but when the bearer's own bucket is short the batch may
-- come from @budget@ instead, if nobody is waiting on it and it has the bytes
-- to spare: capacity nobody else is using. A batch our own bucket pays for is
-- charged to @budget@ on credit in the same transaction, so it always counts
-- against the budget. Returns whether the batch was borrowed.
awaitGrantBorrowing :: forall m. (MonadTimer m, MonadMask m)
                    => BucketHandle m -> Bucket m -> Int -> m Bool
awaitGrantBorrowing h budget = awaitGrantWith (Just budget) 0 h

-- | 'awaitGrant' or, given a budget, 'awaitGrantBorrowing', recording how
-- long the bearer already waited for the writability gate before asking.
awaitGrantWaited :: forall m. (MonadTimer m, MonadMask m)
                 => Maybe (Bucket m) -> DiffTime -> BucketHandle m -> Int -> m Bool
awaitGrantWaited = awaitGrantWith

awaitGrantWith :: forall m. (MonadTimer m, MonadMask m)
               => Maybe (Bucket m) -> DiffTime -> BucketHandle m -> Int -> m Bool
awaitGrantWith borrow_m waitedWritable
               BucketHandle { bhBucket = bucket@Bucket { bWaiters, bTickets, bRotation, bStats
                                                       , bBurst }
                            , bhId, bhRank, bhWake }
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
                 then takeOrBorrow now now
                 else return (Left now)
      case fast of
           Right borrowed -> return (Left borrowed)
           Left _ -> do
             ticket <- stateTVar bTickets (\t@(Ticket n) -> (t, Ticket (n + 1)))
             tier   <- join (readTVar bhRank)
             let key = (queueRank bRotation bhId tier now, ticket)

             writeTVar bhWake False
             writeTVar bWaiters (Map.insert key bhWake waiters)
             queueChanged now (Map.size waiters) (Map.size waiters + 1)
             return (Right key)

    case r of
         Left borrowed -> return borrowed
         Right key     -> unmask (loop now key) `onException` cancel key
  where
    -- the queue went from @n0@ to @n1@ bearers: account for busy periods
    queueChanged :: Time -> Int -> Int -> STM m ()
    queueChanged now n0 n1 = when (n0 /= n1) $ do
      bu <- readTVar bBurst
      st <- readTVar bStats
      let (bu', st') = burstStep now n0 n1 (bu, st)
      writeTVar bBurst bu'
      writeTVar bStats st'

    -- take from our bucket; failing that, from the budget if it is idle:
    -- nobody waiting and the bytes there. Idle but short, the wait is until
    -- whichever bucket has the bytes first. Right: taken, and whether it was
    -- borrowed; Left: the instant to check again. A grant is counted here,
    -- with the wait since the request at @asked@.
    takeOrBorrow :: Time -> Time -> STM m (Either Time Bool)
    takeOrBorrow asked now = do
      r <- takeOrBorrow' now
      case r of
           Right borrowed ->
             modifyTVar bStats
               (recordGrant borrowed need waitedWritable (now `diffTime` asked))
           Left _ -> return ()
      return r

    takeOrBorrow' :: Time -> STM m (Either Time Bool)
    takeOrBorrow' now = do
      own <- tryTake bucket now need
      case (own, borrow_m) of
           (Right (), Nothing)       -> return (Right False)
           (Right (), Just budget)   -> Right False <$ chargeCredit budget now need
           (Left ready, Nothing)     -> return (Left ready)
           (Left ready, Just budget@Bucket { bWaiters = budgetWaiters }) -> do
             idle <- Map.null <$> readTVar budgetWaiters
             if not idle
                then return (Left ready)
                else do
                  lent <- tryTake budget now need
                  case lent of
                       Right ()         -> return (Right True)
                       Left readyBudget -> return (Left (min ready readyBudget))

    loop :: Time -> WaitKey -> m Bool
    loop asked key = do
      atHead <- atomically $
        (== Just key) . fmap fst . Map.lookupMin <$> readTVar bWaiters

      if not atHead
         then do
           -- block on our own wake variable and nothing else, so a change to
           -- the queue wakes the one bearer it concerns; the bearer ahead of
           -- us sets it when it is granted or gives up
           atomically $ do
             w <- readTVar bhWake
             check w
             writeTVar bhWake False
           loop asked key
         else do
           now <- getMonotonicTime
           r <- atomically $ do
             waiters <- readTVar bWaiters

             if fmap fst (Map.lookupMin waiters) /= Just key
               then return Displaced
               else do
                 taken <- takeOrBorrow asked now
                 case taken of
                      Right borrowed -> do
                        let waiters' = Map.delete key waiters

                        writeTVar bWaiters waiters'
                        queueChanged now (Map.size waiters) (Map.size waiters')
                        wakeHead waiters'

                        return (if borrowed then Borrowed else Granted)
                      Left ready -> return (ShortUntil ready)
           case r of
                Granted          -> return False
                Borrowed         -> return True
                Displaced        -> loop asked key
                ShortUntil ready -> do
                  -- sleep until the bytes are there; wake early if a lower
                  -- key takes the head
                  delayVar <- registerDelay (wakeAt now ready `diffTime` now)
                  atomically $
                      (LazySTM.readTVar delayVar >>= check)
                    `orElse`
                      (readTVar bWaiters >>= check . (/= Just key) . fmap fst . Map.lookupMin)
                  loop asked key

    -- on cancellation leave the queue; if we were its head, pass the baton
    cancel :: WaitKey -> m ()
    cancel key = do
     now <- getMonotonicTime
     atomically $ do
      waiters <- readTVar bWaiters

      let wasHead  = fmap fst (Map.lookupMin waiters) == Just key
          waiters' = Map.delete key waiters

      writeTVar bWaiters waiters'
      queueChanged now (Map.size waiters) (Map.size waiters')
      when wasHead (wakeHead waiters')
