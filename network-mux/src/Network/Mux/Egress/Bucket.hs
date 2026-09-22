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
-- with every bearer at 'Rank' 0 (the default) that is an equal share.  A short
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
  , Rank (..)
  , setRank
  , awaitGrant
    -- * Pure core
  , tokenLevel
  , grantAt
  , wakeAt
  , atRate
  ) where

import Control.Concurrent.Class.MonadSTM qualified as LazySTM
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word32, Word64)


newtype Rank = Rank Word32
  deriving (Eq, Ord, Show)

newtype Ticket = Ticket Word64
  deriving (Eq, Ord, Show)

type WaitKey = (Rank, Ticket)

data Bucket m = Bucket {
  bRate     :: !(StrictTVar m Double),   -- ^ bytes/s; @<= 0@ disables the bucket
  bCapacity :: !Int,                     -- ^ bytes; a small multiple of the batch size
  bFull     :: !(StrictTVar m Time),     -- ^ the instant the bucket is full again
  bWaiters  :: !(StrictTVar m (Map WaitKey (StrictTVar m Bool))),
    -- ^ queued bearers, by key, with their wake variables
  bTickets  :: !(StrictTVar m Ticket)
  }

newBucket :: (MonadSTM m, MonadMonotonicTime m)
          => Double  -- ^ rate, bytes/s
          -> Int     -- ^ capacity, bytes
          -> m (Bucket m)
newBucket rate capacity = do
  now      <- getMonotonicTime
  bRate    <- newTVarIO rate
  bFull    <- newTVarIO now
  bWaiters <- newTVarIO Map.empty
  bTickets <- newTVarIO (Ticket 0)
  return Bucket { bRate, bCapacity = capacity, bFull, bWaiters, bTickets }

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


data BucketHandle m = BucketHandle {
  bhBucket :: !(Bucket m),
  bhRank   :: !(StrictTVar m Rank),
  bhWake   :: !(StrictTVar m Bool)       -- ^ set by the bearer ahead of us when it is granted
  }

registerBearer :: MonadSTM m => Bucket m -> m (BucketHandle m)
registerBearer bucket = BucketHandle bucket <$> newTVarIO (Rank 0) <*> newTVarIO False

setRank :: MonadSTM m => BucketHandle m -> Rank -> STM m ()
setRank BucketHandle { bhRank } = writeTVar bhRank


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

-- | Wake the bearer at the head of the queue, if any.
wakeHead :: MonadSTM m => Map WaitKey (StrictTVar m Bool) -> STM m ()
wakeHead waiters =
  case Map.lookupMin waiters of
       Just (_, wake) -> writeTVar wake True
       Nothing        -> return ()


data Attempt = Granted | Displaced | ShortUntil !Time

-- | Block until @need@ bytes are granted to this bearer.
awaitGrant :: forall m. (MonadTimer m, MonadMask m)
           => BucketHandle m -> Int -> m ()
awaitGrant BucketHandle { bhBucket = bucket@Bucket { bWaiters, bTickets }, bhRank, bhWake }
           need =
  -- masked until the exception handler is in place: a bearer killed on its
  -- way into the queue must not leave a dead entry at the head
  mask $ \unmask -> do
    now <- getMonotonicTime

    -- one transaction: take at once if nothing is queued and the tokens are
    -- there, otherwise join the queue.  Nothing can slip in between.
    key_m <- atomically $ do
      waiters <- readTVar bWaiters
      granted <- if Map.null waiters
                    then either (const False) (const True) <$> tryTake bucket now need
                    else return False
      if granted
         then return Nothing
         else do
           ticket <- stateTVar bTickets (\t@(Ticket n) -> (t, Ticket (n + 1)))
           rank   <- readTVar bhRank
           let key = (rank, ticket)

           writeTVar bhWake False
           writeTVar bWaiters (Map.insert key bhWake waiters)
           return (Just key)

    case key_m of
         Nothing  -> return ()
         Just key -> unmask (loop key) `onException` cancel key
  where
    loop :: WaitKey -> m ()
    loop key = do
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
           loop key
         else do
           now <- getMonotonicTime
           r <- atomically $ do
             waiters <- readTVar bWaiters

             if fmap fst (Map.lookupMin waiters) /= Just key
               then return Displaced
               else do
                 taken <- tryTake bucket now need
                 case taken of
                      Right ()   -> do
                        let waiters' = Map.delete key waiters

                        writeTVar bWaiters waiters'
                        wakeHead waiters'

                        return Granted
                      Left ready -> return (ShortUntil ready)
           case r of
                Granted          -> return ()
                Displaced        -> loop key
                ShortUntil ready -> do
                  -- sleep until the bytes are there; wake early if a lower
                  -- key takes the head
                  delayVar <- registerDelay (wakeAt now ready `diffTime` now)
                  atomically $
                      (LazySTM.readTVar delayVar >>= check)
                    `orElse`
                      (readTVar bWaiters >>= check . (/= Just key) . fmap fst . Map.lookupMin)
                  loop key

    -- on cancellation leave the queue; if we were its head, pass the baton
    cancel :: WaitKey -> m ()
    cancel key = atomically $ do
      waiters <- readTVar bWaiters

      let wasHead  = fmap fst (Map.lookupMin waiters) == Just key
          waiters' = Map.delete key waiters

      writeTVar bWaiters waiters'
      when wasHead (wakeHead waiters')
