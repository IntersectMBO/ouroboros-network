{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime

import           Data.List (sortOn, unfoldr)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word32)
import           System.Random

import           Network.Socket (SockAddr)

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.PeerMetric


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
                          -> STM m ChurnMode
                          -> PeerMetrics m SockAddr
                          -> PeerSelectionPolicy SockAddr m
simplePeerSelectionPolicy rngVar getChurnMode metrics = PeerSelectionPolicy {
      policyPickKnownPeersForGossip = simplePromotionPolicy,
      policyPickColdPeersToPromote  = simplePromotionPolicy,
      policyPickWarmPeersToPromote  = simplePromotionPolicy,

      policyPickHotPeersToDemote    = hotDemotionPolicy,
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
    addRand :: Set.Set SockAddr -> STM m (Map.Map SockAddr Word32)
    addRand available = do
      inRng <- readTVar rngVar

      let (rng, rng') = split inRng
          rns = take (Set.size available) $ unfoldr (Just . random)  rng :: [Word32]
          available' = Map.fromList $ zip (Set.toList available) rns
      writeTVar rngVar rng'
      return available'

    hotDemotionPolicy :: PickPolicy SockAddr m
    hotDemotionPolicy _ _ available pickNum = do
        mode <- getChurnMode
        scores <- case mode of
                       ChurnModeNormal ->
                           upstreamyness <$> getHeaderMetrics metrics
                       ChurnModeBulkSync ->
                           fetchyness <$> getFetchedMetrics metrics
        available' <- addRand available
        return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(peer, rn) ->
                          (Map.findWithDefault 0 peer scores, rn))
             . Map.assocs
             $ available'

    simplePromotionPolicy :: PickPolicy SockAddr m
    simplePromotionPolicy _ _ available pickNum = do
      available' <- addRand available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, rn) -> rn)
             . Map.assocs
             $ available'

    simpleDemotionPolicy :: PickPolicy SockAddr m
    simpleDemotionPolicy _ _ available pickNum = do
      available' <- addRand available
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(_, rn) -> rn)
             . Map.assocs
             $ available'
