{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Data.List (sortOn)
import qualified Data.Map.Strict as Map
import           Data.Ord (Down (..))
import qualified Data.Set as Set
import           Data.Word (Word32)
import           System.Random

import           Network.Socket (SockAddr)

import           Ouroboros.Network.PeerSelection.Types
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

     -- Add metrics and a random number in order to prevent ordering based on SockAddr
     -- TODO: upstreamyness is added here
    addMetrics :: Map.Map SockAddr PeerSource -> STM m (Map.Map SockAddr (PeerSource, Word32))
    addMetrics available = do
      inRng <- readTVar rngVar
      let (inRng', available') =
            Map.mapAccum
              (\rng peersource ->
                let (nonce, rng') = random rng in
                (rng', (peersource, nonce))
              )
              inRng
              available
      writeTVar rngVar inRng'
      return available'

    simplePromotionPolicy :: PickPolicy SockAddr m
    simplePromotionPolicy available pickNum = do
      available' <- addMetrics available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, rn) -> rn)
             . Map.assocs
             $ available'

    simpleDemotionPolicy :: PickPolicy SockAddr m
    simpleDemotionPolicy available pickNum = do
      available' <- addMetrics available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, rn) -> Down rn)
             . Map.assocs
             $ available'

