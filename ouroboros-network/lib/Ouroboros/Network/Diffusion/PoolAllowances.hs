{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Credit buckets for the egress scheduler's partner and residual tiers.
--
-- A bucket is refilled by the fresh need the chain announces and charged by
-- what a peer is served. It gates class, not rate: while its credit is
-- positive the peer is served at its tier, at zero it falls to the next
-- class it qualifies for, rest at the last, and nothing is charged there. A
-- partner spends its own bucket at the partner tier; a big-ledger pool's
-- relays share the pool's; a stranger spends its own at the stranger tier.
-- The arithmetic is pure and exact over a monotone reading of fresh bytes;
-- where the reading comes from, a clock from the protocol parameters in the
-- prototype, an announcement counter from consensus eventually, is the
-- caller's business.
module Ouroboros.Network.Diffusion.PoolAllowances
  ( -- * The pure core
    Allowance (..)
  , Fresh (..)
  , Charged (..)
  , kappa
  , capacity
  , credit
  , charge
  , lockedCharged
  , freshAt
    -- * Buckets by pool
  , PoolKey
  , poolKeys
  , PoolAllowances (..)
  , newPoolAllowances
  , rebuild
  , reindex
  , patchIndex
  , bucketsFor
  , hasCredit
  , chargeBuckets
    -- * A connection's rule and sink
  , Standing (..)
  , restTier
  , rankFor
  , mkEgressRule
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forM)
import Control.Monad.Class.MonadTime.SI (Time, diffTime)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Word (Word8)
import Network.Mux (MiniProtocolNum, Rank (..), Tier (..), tierNumber)

import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)

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

-- | An empty bucket that opens @lock@ fresh bytes after this reading: the
-- charged total is set at κ of the reading plus the lock, so the credit reads
-- zero until the reading has moved past the lock, then fills from empty at the
-- fresh rate. Nothing is charged meanwhile, since 'charge' leaves a dry bucket
-- alone. A lock is an empty bucket opened later: @lockedCharged f lock@ is
-- @lockedCharged (f + lock) 0@.
lockedCharged :: Fresh -> Fresh -> Charged
lockedCharged (Fresh f) (Fresh lock) = Charged (kappa (Fresh (f + lock)))

-- | A pool as the buckets know it: its relay list as the ledger carries it
-- and, among pools registering the same list, which of them. A pool keeps
-- its bucket from poll to poll by this key, whatever its stake or position;
-- re-registered with other relays it is a new pool, with an empty bucket.
type PoolKey = (NonEmpty RelayAccessPoint, Int)

-- | The list's keys, in its order: each relay list with its ordinal among
-- the equal lists before it.
poolKeys :: [NonEmpty RelayAccessPoint] -> [PoolKey]
poolKeys = go Map.empty
  where
    go _ [] = []
    go seen (rs : rest) =
      let n = Map.findWithDefault 0 rs seen
      in (rs, n) : go (Map.insert rs (n + 1) seen) rest

-- | The prototype's reading: fresh bytes grow at a rate from the protocol
-- parameters, (B_max + L_max + F_max)·f per second, from an origin.
freshAt :: Double -> Time -> Time -> Fresh
freshAt rate t0 now = Fresh (max 0 (floor (rate * realToFrac (now `diffTime` t0) :: Double)))

-- | One bucket per big-ledger pool, by position in the stake-sorted list,
-- each with the pool's key, and the resolved addresses that map to each
-- position. A stranger's own bucket is locked for 'paStrangerLock' fresh
-- bytes from its connection's start.
data PoolAllowances m addr = PoolAllowances {
    paAllowance    :: !Allowance,
    paFreshAt      :: !(Time -> Fresh),
    paStrangerLock :: !Fresh,
    paBuckets      :: !(StrictTVar m (IntMap (PoolKey, StrictTVar m Charged))),
    paIndex        :: !(StrictTVar m (Map addr [Int]))
  }

newPoolAllowances :: MonadSTM m
                  => Allowance -> (Time -> Fresh) -> Fresh -> m (PoolAllowances m addr)
newPoolAllowances paAllowance paFreshAt paStrangerLock = do
  paBuckets <- newTVarIO IntMap.empty
  paIndex   <- newTVarIO Map.empty
  return PoolAllowances { .. }

-- | Install the list as read: a pool known by its key keeps its bucket, one
-- new to the list gets an empty one at this reading, one that left loses
-- its; and the addresses that resolve to each position. Returns how many
-- buckets were kept. Empty for the newcomers, so that nothing which changes
-- the list hands out credit: a pool re-registered with other relays is a
-- newcomer with nothing, and a change of stake or order moves a pool's
-- bucket with it.
rebuild :: MonadSTM m
        => PoolAllowances m addr -> Time -> [NonEmpty RelayAccessPoint] -> Map addr [Int] -> STM m Int
rebuild PoolAllowances { .. } now relays index = do
  old <- readTVar paBuckets
  let known = Map.fromList (IntMap.elems old)
  buckets <- forM (zip [0 ..] (poolKeys relays)) $ \(i, key) ->
    case Map.lookup key known of
         Just tv -> return (i, (key, tv), True)
         Nothing -> do
           tv <- newTVar (lockedCharged (paFreshAt now) (Fresh 0))
           return (i, (key, tv), False)
  writeTVar paBuckets (IntMap.fromList [ (i, b) | (i, b, _) <- buckets ])
  writeTVar paIndex index
  return (length [ () | (_, _, True) <- buckets ])

-- | Swap the address index alone: the same list, its relays resolved anew.
reindex :: MonadSTM m => PoolAllowances m addr -> Map addr [Int] -> STM m ()
reindex PoolAllowances { paIndex } index = writeTVar paIndex index

-- | Change some addresses' entries: the positions each maps to now, none
-- removing the address.
patchIndex :: (MonadSTM m, Ord addr) => PoolAllowances m addr -> [(addr, [Int])] -> STM m ()
patchIndex PoolAllowances { paIndex } changes =
  modifyTVar paIndex $ \index ->
    foldr (\(addr, ps) -> if null ps then Map.delete addr else Map.insert addr ps) index changes

-- | The buckets an address maps to now; none for a stranger.
bucketsFor :: (MonadSTM m, Ord addr)
           => PoolAllowances m addr -> addr -> STM m [StrictTVar m Charged]
bucketsFor PoolAllowances { paBuckets, paIndex } addr = do
  index   <- readTVar paIndex
  buckets <- readTVar paBuckets
  return [ tv | i <- Map.findWithDefault [] addr index
              , Just (_, tv) <- [IntMap.lookup i buckets] ]

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

-- | The rest: the tier of a peer without credit, served from what the tiers
-- above leave idle. From here up bearers take turns rather than being dealt
-- a place, so a newcomer among a flood still gets its share of that.
restTier :: Word8
restTier = tierNumber RestTier

-- | A peer's rank from its standing and its credit: local roots first; a
-- partner with credit in its own bucket next, and one without is the peer
-- it would otherwise be; then the residual tier's classes: a big-ledger
-- pool's relay with credit in the pool's buckets, a stranger with credit in
-- its own, and at zero credit the rest. The pool credit is 'Nothing' for a
-- peer that belongs to no pool.
rankFor :: Standing
        -> Bool        -- ^ credit in the connection's own bucket
        -> Maybe Bool  -- ^ credit in its pool's buckets, if it has a pool
        -> Rank
rankFor LocalRoot _   _     = Rank (tierNumber LocalRootTier)
rankFor Partner   own pools
  | own                     = Rank (tierNumber PartnerTier)
  | otherwise               = rankFor Other own pools
rankFor Other     own pools =
  case pools of
       Just True   -> Rank (tierNumber PoolTier)
       Just False  -> Rank restTier
       Nothing
         | own     -> Rank (tierNumber StrangerTier)
         | otherwise -> Rank restTier

-- | A connection's rank rule and charge sink, sharing one view of the
-- buckets: its pools' buckets, looked up at every use, and its own bucket,
-- which opens empty once the connection has outlived the stranger lock, so
-- a reconnection starts the wait over. The sink runs in the transaction
-- that grants a batch, just after the rule was asked there, and charges the
-- bucket that gave the rank the rule last answered: a partner's own bucket
-- at the partner tier, the pool's buckets at the pool's, the own bucket at
-- the stranger's; at rest nothing. Bytes on the uncharged protocols are
-- never charged.
mkEgressRule :: (MonadSTM m, Ord addr)
             => PoolAllowances m addr
             -> [MiniProtocolNum]
             -> addr
             -> (Time -> STM m Standing)
             -> Time
             -> m (Time -> STM m Rank, Time -> MiniProtocolNum -> Int -> STM m ())
mkEgressRule allowances@PoolAllowances { paAllowance, paFreshAt, paStrangerLock }
             uncharged key standing t0 = do
  own       <- newTVarIO (lockedCharged (paFreshAt t0) paStrangerLock)
  asPartner <- newTVarIO False   -- the rule's last answer was the partner tier
  let rule now = do
        s <- standing now
        rank <- case s of
             LocalRoot -> return (rankFor LocalRoot False Nothing)
             _ -> do
               bs <- bucketsFor allowances key
               ownCredit <- (> 0) . credit paAllowance (paFreshAt now) <$> readTVar own
               pools     <- case bs of
                                 [] -> return Nothing
                                 _  -> Just <$> hasCredit allowances now bs
               return (rankFor s ownCredit pools)
        writeTVar asPartner (rank == Rank (tierNumber PartnerTier))
        return rank

      sink now num n
        | num `elem` uncharged = return ()
        | otherwise = do
            partner <- readTVar asPartner
            bs      <- bucketsFor allowances key
            case (partner, bs) of
                 (True, _)   -> modifyTVar own (charge paAllowance (paFreshAt now) n)
                 (False, []) -> modifyTVar own (charge paAllowance (paFreshAt now) n)
                 (False, _)  -> chargeBuckets allowances now n bs

  return (rule, sink)
