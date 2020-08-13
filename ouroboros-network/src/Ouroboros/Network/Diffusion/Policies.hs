{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime

import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import qualified Data.Set as Set

import           Network.Socket (SockAddr)

import           Ouroboros.Network.PeerSelection.Types
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.KnownPeers


-- | Timeout for 'spsDeactivateTimeout'.
--
-- The maximal timeoout on 'ChainSync' (in 'StMustReply' state) is @269s@.
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


simplePeerSelectionPolicy :: forall m. Applicative (STM m)
                          => PeerSelectionPolicy SockAddr m
simplePeerSelectionPolicy = PeerSelectionPolicy {
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
    -- then order them with smallest fail count.
    simplePromotionPolicy :: PickPolicy SockAddr m
    simplePromotionPolicy available pickNum
      | Map.size available <= pickNum
      = pure $ Map.keysSet available

      | otherwise
      = pure . Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, KnownPeerInfo { knownPeerSource, knownPeerFailCount }) ->
                          (knownPeerSource, knownPeerFailCount))
             . Map.assocs
             $ available

    -- demotion policy; it never demotes local root peers.
    --
    simpleDemotionPolicy :: PickPolicy SockAddr m
    simpleDemotionPolicy available pickNum
      | Map.size available <= pickNum
      = pure . Map.keysSet
             . Map.filter ((/= PeerSourceLocalRoot) . knownPeerSource)
             $ available

      | otherwise
      = pure . Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, KnownPeerInfo { knownPeerSource, knownPeerFailCount }) ->
                          (Down knownPeerSource, Down knownPeerFailCount))
             . filter ((/= PeerSourceLocalRoot) . knownPeerSource . snd)
             . Map.assocs
             $ available
