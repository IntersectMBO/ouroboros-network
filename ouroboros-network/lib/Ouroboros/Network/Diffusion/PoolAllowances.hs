{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Credit buckets for the residual tier of the egress scheduler.
--
-- A bucket is refilled by the fresh need the chain announces and charged by
-- what a peer is served. It gates class, not rate: while its credit is
-- positive the peer is served at residual rank, at zero it is served at rest
-- and nothing is charged. The arithmetic is pure and exact over a monotone
-- reading of fresh bytes; where the reading comes from, a clock from the
-- protocol parameters in the prototype, an announcement counter from
-- consensus eventually, is the caller's business.
module Ouroboros.Network.Diffusion.PoolAllowances
  ( -- * The pure core
    Allowance (..)
  , Fresh (..)
  , Charged (..)
  , kappa
  , capacity
  , credit
  , charge
  , resetCharged
  , freshAt
    -- * Buckets by pool
  , PoolAllowances (..)
  , newPoolAllowances
  , rebuild
  , reindex
  , BucketRef (..)
  , bucketsFor
  , hasCredit
  , chargeBuckets
    -- * A connection's rule and sink
  , Standing (..)
  , rankFor
  , mkEgressRule
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI (Time, diffTime)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Network.Mux (MiniProtocolNum, Rank (..))

-- | Bytes of fresh need announced so far: a monotone reading.
newtype Fresh = Fresh { unFresh :: Int }
  deriving (Eq, Ord, Show)

-- | Bytes charged to one bucket.
newtype Charged = Charged { unCharged :: Int }
  deriving (Eq, Ord, Show)

-- | The allowance's one parameter: the most fresh bytes one object can
-- announce, block body, list and closure at their maxima. κ = 3/2 and the
-- capacity 2·κ·max follow from it.
newtype Allowance = Allowance { alFreshMax :: Int }
  deriving (Eq, Show)

-- | κ · fresh with κ = 3/2, rounded down.
kappa :: Fresh -> Int
kappa (Fresh f) = (3 * f) `div` 2

-- | What a bucket holds at most: two fresh objects' worth of refill.
capacity :: Allowance -> Int
capacity Allowance { alFreshMax } = 2 * kappa (Fresh alFreshMax)

-- | The bucket's credit: what κ·fresh has granted less what was charged,
-- never above the capacity and never below zero.
credit :: Allowance -> Fresh -> Charged -> Int
credit al fresh (Charged c) = max 0 (min (capacity al) (kappa fresh - c))

-- | Charge bytes while the credit is positive. The charged total is first
-- lifted to at least κ·fresh − C, which is what "the bucket holds at most C"
-- means in this representation: a bucket left alone for an hour reads C, and
-- spending C then empties it. At zero credit nothing is charged, so a peer at
-- rest runs up no debt.
charge :: Allowance -> Fresh -> Int -> Charged -> Charged
charge al fresh n ch@(Charged c)
  | credit al fresh ch > 0 = Charged (max c (kappa fresh - capacity al) + n)
  | otherwise              = ch

-- | A full bucket at this reading.
resetCharged :: Allowance -> Fresh -> Charged
resetCharged al fresh = Charged (kappa fresh - capacity al)

-- | The prototype's reading: fresh bytes grow at a rate from the protocol
-- parameters, (B_max + L_max + F_max)·f per second, from an origin.
freshAt :: Double -> Time -> Time -> Fresh
freshAt rate t0 now = Fresh (max 0 (floor (rate * realToFrac (now `diffTime` t0) :: Double)))

-- | One bucket per big-ledger pool, by position in the stake-sorted list, and
-- the resolved addresses that map to each. Replaced as a whole when the
-- ledger list changes; the generation lets a bearer notice and look its
-- buckets up again, so no bucket outlives the list it came from.
data PoolAllowances m addr = PoolAllowances {
    paAllowance  :: !Allowance,
    paFreshAt    :: !(Time -> Fresh),
    paGeneration :: !(StrictTVar m Int),
    paBuckets    :: !(StrictTVar m (IntMap (StrictTVar m Charged))),
    paIndex      :: !(StrictTVar m (Map addr [Int]))
  }

newPoolAllowances :: MonadSTM m
                  => Allowance -> (Time -> Fresh) -> m (PoolAllowances m addr)
newPoolAllowances paAllowance paFreshAt = do
  paGeneration <- newTVarIO 0
  paBuckets    <- newTVarIO IntMap.empty
  paIndex      <- newTVarIO Map.empty
  return PoolAllowances { .. }

-- | Install a new list: @n@ pools, every bucket full at @now@, and the
-- addresses that resolve to each position.
rebuild :: MonadSTM m
        => PoolAllowances m addr -> Time -> Int -> Map addr [Int] -> STM m ()
rebuild PoolAllowances { .. } now n index = do
  buckets <- mapM (\i -> do tv <- newTVar (resetCharged paAllowance (paFreshAt now))
                            return (i, tv))
                  [0 .. n - 1]
  writeTVar paBuckets (IntMap.fromList buckets)
  writeTVar paIndex index
  modifyTVar paGeneration (+ 1)

-- | Swap the address index alone: the same list, its relays resolved anew.
reindex :: MonadSTM m => PoolAllowances m addr -> Map addr [Int] -> STM m ()
reindex PoolAllowances { paIndex } = writeTVar paIndex

-- | A bearer's view: its pools' buckets, and the generation they came from.
data BucketRef m = BucketRef {
    brGeneration :: !Int,
    brBuckets    :: ![StrictTVar m Charged]
  }

-- | The buckets an address maps to now; empty for a stranger.
bucketsFor :: (MonadSTM m, Ord addr)
           => PoolAllowances m addr -> addr -> STM m (BucketRef m)
bucketsFor PoolAllowances { .. } addr = do
  brGeneration <- readTVar paGeneration
  index        <- readTVar paIndex
  buckets      <- readTVar paBuckets
  let brBuckets = [ tv | i <- Map.findWithDefault [] addr index
                       , Just tv <- [IntMap.lookup i buckets] ]
  return BucketRef { .. }

-- | Any of the buckets has credit at @now@.
hasCredit :: MonadSTM m
          => PoolAllowances m addr -> Time -> [StrictTVar m Charged] -> STM m Bool
hasCredit PoolAllowances { paAllowance, paFreshAt } now =
  fmap or . mapM (\tv -> (> 0) . credit paAllowance (paFreshAt now) <$> readTVar tv)

-- | Charge every bucket that has credit at @now@.
chargeBuckets :: MonadSTM m
              => PoolAllowances m addr -> Time -> Int -> [StrictTVar m Charged] -> STM m ()
chargeBuckets PoolAllowances { paAllowance, paFreshAt } now n =
  mapM_ (\tv -> modifyTVar tv (charge paAllowance (paFreshAt now) n))

-- | What the governor says about a peer, read as its bearer queues.
data Standing = LocalRoot | Partner | Other
  deriving (Eq, Show)

-- | A peer's rank from its standing and its credit: local roots first,
-- partners next, then the residual tier's classes: a big-ledger pool's
-- relay with credit, a stranger with credit, and at zero credit the rest.
rankFor :: Standing -> Bool -> Bool -> Rank
rankFor LocalRoot _       _     = Rank 0
rankFor Partner   _       _     = Rank 1
rankFor Other     matched hasAny
  | not hasAny = Rank 4
  | matched    = Rank 2
  | otherwise  = Rank 3

-- | A connection's rank rule and charge sink, sharing one view of the
-- buckets: its pools' buckets, looked up again whenever the list was
-- rebuilt, or when it belongs to no pool its own bucket, full as the
-- connection starts. Bytes on the uncharged protocols are never charged.
mkEgressRule :: (MonadSTM m, Ord addr)
             => PoolAllowances m addr
             -> [MiniProtocolNum]
             -> addr
             -> (Time -> STM m Standing)
             -> Time
             -> m (Time -> STM m Rank, Time -> MiniProtocolNum -> Int -> STM m ())
mkEgressRule allowances@PoolAllowances { paAllowance, paFreshAt, paGeneration }
             uncharged key standing t0 = do
  stranger <- newTVarIO (resetCharged paAllowance (paFreshAt t0))
  cached   <- newTVarIO BucketRef { brGeneration = -1, brBuckets = [] }
  let buckets = do
        ref <- readTVar cached
        generation <- readTVar paGeneration
        if brGeneration ref == generation
           then return (brBuckets ref)
           else do
             ref' <- bucketsFor allowances key
             writeTVar cached ref'
             return (brBuckets ref')

      rule now = do
        s <- standing now
        case s of
             Other -> do
               bs <- buckets
               hasAny <- case bs of
                              [] -> (> 0) . credit paAllowance (paFreshAt now) <$> readTVar stranger
                              _  -> hasCredit allowances now bs
               return (rankFor Other (not (null bs)) hasAny)
             _ -> return (rankFor s False False)

      sink now num n
        | num `elem` uncharged = return ()
        | otherwise = do
            bs <- buckets
            case bs of
                 [] -> modifyTVar stranger (charge paAllowance (paFreshAt now) n)
                 _  -> chargeBuckets allowances now n bs

  return (rule, sink)

