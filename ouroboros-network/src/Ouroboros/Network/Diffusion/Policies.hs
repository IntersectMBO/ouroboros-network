{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime

import qualified Data.Set as Set

import           Network.Socket (SockAddr)

import           Ouroboros.Network.PeerSelection.Governor.Types


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
    -- TODO
    simplePromotionPolicy :: PickPolicy SockAddr m
    simplePromotionPolicy available pickNum
      = pure . Set.take pickNum
             $ available

    -- TODO
    simpleDemotionPolicy :: PickPolicy SockAddr m
    simpleDemotionPolicy available pickNum
      = pure . Set.take pickNum
             $ available
