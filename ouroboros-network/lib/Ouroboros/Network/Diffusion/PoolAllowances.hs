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
-- The arithmetic is pure and exact over a monotone reading of fresh bytes,
-- consensus's count of freshly announced bytes.
module Ouroboros.Network.Diffusion.PoolAllowances
  ( -- * The pure core
    Allowance (..)
  , Fresh (..)
  , Charged (..)
  , kappa
  , capacity
  , credit
  , charge
  , emptyCharged
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
import Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime)
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

-- | An empty bucket at this reading: zero credit now, filling from here at
-- the fresh rate.
emptyCharged :: Fresh -> Charged
emptyCharged = Charged . kappa

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

-- | One bucket per big-ledger pool, by position in the stake-sorted list,
-- each with the pool's key, and the resolved addresses that map to each
-- position. A stranger's own bucket is locked for 'paStrangerLock' of
-- wall-clock time from its connection's start, then opens empty at that
-- moment's reading and fills at the fresh rate. The allowance is read at
-- every use, so a parameter change on the chain takes effect at the next
-- one: credit above a smaller capacity is clamped, a larger one has room.
data PoolAllowances m addr = PoolAllowances {
    paAllowance    :: !(STM m Allowance),
    paFresh        :: !(STM m Fresh),
    paStrangerLock :: !DiffTime,
    paBuckets      :: !(StrictTVar m (IntMap (PoolKey, StrictTVar m Charged))),
    paIndex        :: !(StrictTVar m (Map addr [Int]))
  }

newPoolAllowances :: MonadSTM m
                  => STM m Allowance -> STM m Fresh -> DiffTime -> m (PoolAllowances m addr)
newPoolAllowances paAllowance paFresh paStrangerLock = do
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
        => PoolAllowances m addr -> [NonEmpty RelayAccessPoint] -> Map addr [Int] -> STM m Int
rebuild PoolAllowances { .. } relays index = do
  fresh <- paFresh
  old   <- readTVar paBuckets
  let known = Map.fromList (IntMap.elems old)
  buckets <- forM (zip [0 ..] (poolKeys relays)) $ \(i, key) ->
    case Map.lookup key known of
         Just tv -> return (i, (key, tv), True)
         Nothing -> do
           tv <- newTVar (emptyCharged fresh)
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

-- | Any of the buckets has credit.
hasCredit :: MonadSTM m
          => PoolAllowances m addr -> [StrictTVar m Charged] -> STM m Bool
hasCredit PoolAllowances { paAllowance, paFresh } tvs = do
  al    <- paAllowance
  fresh <- paFresh
  or <$> mapM (\tv -> (> 0) . credit al fresh <$> readTVar tv) tvs

-- | Charge every bucket that has credit.
chargeBuckets :: MonadSTM m
              => PoolAllowances m addr -> Int -> [StrictTVar m Charged] -> STM m ()
chargeBuckets PoolAllowances { paAllowance, paFresh } n tvs = do
  al    <- paAllowance
  fresh <- paFresh
  mapM_ (\tv -> modifyTVar tv (charge al fresh n)) tvs

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
-- never charged. A batch's charged bytes are charged as one sum, so a batch
-- granted with credit is charged in full however its bytes split among
-- protocols.
mkEgressRule :: (MonadSTM m, Ord addr)
             => PoolAllowances m addr
             -> [MiniProtocolNum]
             -> addr
             -> (Time -> STM m Standing)
             -> Time
             -> m (Time -> STM m Rank, Time -> Map MiniProtocolNum Int -> STM m ())
mkEgressRule allowances@PoolAllowances { paAllowance, paFresh, paStrangerLock }
             uncharged key standing t0 = do
  -- the connection's own bucket: nothing until the lock has passed, then
  -- empty at the reading of its first use after that, filling at the fresh
  -- rate; a reconnection is a new rule and starts the wait over
  own       <- newTVarIO Nothing
  asPartner <- newTVarIO False   -- the rule's last answer was the partner tier
  let unlockAt = paStrangerLock `addTime` t0
      ownAt now fresh = do
        mb <- readTVar own
        case mb of
             Just _ -> return mb
             Nothing
               | now < unlockAt -> return Nothing
               | otherwise -> do
                   let opened = Just (emptyCharged fresh)
                   writeTVar own opened
                   return opened
      chargeOwn now fresh n = do
        mb <- ownAt now fresh
        case mb of
             Just ch -> do al <- paAllowance
                           writeTVar own (Just $! charge al fresh n ch)
             Nothing -> return ()

      rule now = do
        s <- standing now
        rank <- case s of
             LocalRoot -> return (rankFor LocalRoot False Nothing)
             _ -> do
               bs        <- bucketsFor allowances key
               al        <- paAllowance
               fresh     <- paFresh
               ownCredit <- maybe False ((> 0) . credit al fresh) <$> ownAt now fresh
               pools     <- case bs of
                                 [] -> return Nothing
                                 _  -> Just <$> hasCredit allowances bs
               return (rankFor s ownCredit pools)
        writeTVar asPartner (rank == Rank (tierNumber PartnerTier))
        return rank

      sink now batch
        | n <= 0    = return ()
        | otherwise = do
            partner <- readTVar asPartner
            bs      <- bucketsFor allowances key
            fresh   <- paFresh
            case (partner, bs) of
                 (True, _)   -> chargeOwn now fresh n
                 (False, []) -> chargeOwn now fresh n
                 (False, _)  -> chargeBuckets allowances n bs
        where
          n = sum [ bytes | (num, bytes) <- Map.toList batch, num `notElem` uncharged ]

  return (rule, sink)
