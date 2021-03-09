{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Data.List (sortOn, unfoldr)
import qualified Data.Map.Strict as Map
import           Data.Monoid (All (..), Any (..))
import           Data.Ord (Down (..))
import qualified Data.Set as Set
import           Data.Word (Word32)
import           System.Random

import           Network.Socket (SockAddr)

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.KnownPeers


-- | Timeout for 'spsDeactivateTimeout'.
--
-- The maximal timeout on 'ChainSync' (in 'StMustReply' state) is @269s@.
--
deactivateTimeout :: DiffTime
deactivateTimeout = 300

-- | Timeout for 'spsCloseConnectionTimeout'.
--
-- This timeout depends on 'KeepAlive' and 'TipSample' timeouts.  'KeepAlive'
-- keeps agancy most of the time, but 'TipSample' can give away its agency for
-- longer periods of time.  Here we allow it to get 6 blocks (assuming a new
-- block every @20s@).
--
closeConnectionTimeout :: DiffTime
closeConnectionTimeout = 120


simplePeerSelectionPolicy :: forall m. MonadSTM m
                          => StrictTVar m StdGen
                          -> PeerSelectionPolicy SockAddr m
simplePeerSelectionPolicy rngVar = PeerSelectionPolicy {
      policyPickKnownPeersForGossip = simplePromotionPolicy,
      policyPickColdPeersToPromote  = simplePromotionPolicy,
      policyPickWarmPeersToPromote  = simplePromotionPolicy,

      policyPickHotPeersToDemote    = simpleDemotionPolicy,
      policyPickWarmPeersToDemote   = simpleDemotionPolicy,
      policyPickColdPeersToForget   = simpleDemotionPolicy,

      policyFindPublicRootTimeout   = 5,    -- seconds
      policyMaxInProgressGossipReqs = 2,
      policyGossipRetryTime         = 3600, -- seconds
      policyGossipBatchWaitTime     = 3,    -- seconds
      policyGossipOverallTimeout    = 10    -- seconds
    }
  where
    -- promotiong policy; in first place pick local roots, then public ones,
    -- then order them with smallest fail count with a random value as a tie breaker.
    simplePromotionPolicy :: PickPolicy SockAddr m
    simplePromotionPolicy available pickNum = do
      available' <- addRand available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, (KnownPeerInfo { knownPeerSource, knownPeerFailCount }, rn)) ->
                          (knownPeerSource, knownPeerFailCount, rn))
             . Map.assocs
             $ available'

    -- Add a random number in order to prevent ordering based on SockAddr
    addRand :: Map.Map SockAddr KnownPeerInfo -> STM m (Map.Map SockAddr (KnownPeerInfo, Word32))
    addRand available = do
      inRng <- readTVar rngVar

      let (rng, rng') = split inRng
          rns = take (Map.size available) $ unfoldr (Just . random)  rng :: [Word32]
          available' = Map.fromList $ map (\(rn, (k, v)) -> (k, (v, rn))) $ zip rns (Map.toList available)
      writeTVar rngVar rng'
      return available'

    -- demotion policy; it demotes local root peers at last.
    --
    -- Note: if we forbid to demote local roots, it might happen that the
    -- governor will be above its limit of known local root peers, and it will
    -- not be able to make progress at forgeting them.  This happened using
    -- `relays-new.cardano-mainnet.iohk.io` as a local root peer provider.
    -- This might also happen in a more realistic scenario in which a pool
    -- operator is adding new local roots and removing old ones.  If we forbid
    -- forgeting local roots, the governor will never forget the old, non
    -- functioning local roots.
    --
    -- A to be more realistic we could start forgetting local roots if their
    -- 'knownPeerFailCount' is above some quota.  But this does not help with
    -- p2p-governor stepping in place.  After all if we know about a new cold
    -- peer which exceeds the number of 'targetNumberOfRootPeers' it's
    -- 'knonwPeerFailCount' will not have a chance to grow unless it's picked by
    -- the governor for promotion.
    --
    simpleDemotionPolicy :: PickPolicy SockAddr m
    simpleDemotionPolicy available pickNum = do
      available' <- addRand available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, (KnownPeerInfo { knownPeerSource, knownPeerFailCount }, rn)) ->
                         (Down knownPeerSource, Down knownPeerFailCount, rn))
             . Map.assocs
             $ available'


-- | Make a disjunction of two predicates.  Disjunction is also known as @join@
-- in [lattice theory](https://en.wikipedia.org/wiki/Lattice_\(order\)).
--
join :: (a -> Bool)
     -> (a -> Bool)
     ->  a -> Bool
join f g = getAny . (Any . f <> Any . g)

infixr 6 `join`


-- | Make a conjuction of two predicates.  Conjuction is also known as @meet@ in
-- [lattice theory](https://en.wikipedia.org/wiki/Lattice_\(order\)).
--
meet :: (a -> Bool)
     -> (a -> Bool)
     ->  a -> Bool
meet f g = getAll . (All . f <> All . g)

infixr 7 `meet`
