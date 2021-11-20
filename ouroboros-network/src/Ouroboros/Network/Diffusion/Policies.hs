{-# LANGUAGE FlexibleContexts    #-}
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
import qualified System.Random as Rnd

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.PeerMetric
import           Ouroboros.Network.InboundGovernor (InboundGovernorObservableState (..))
import           Ouroboros.Network.ConnectionManager.Types (ConnectionType
                   (..), Provenance (..), PrunePolicy)


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


simplePeerSelectionPolicy :: forall m peerAddr.
                             ( MonadSTM m
                             , Ord peerAddr
                             )
                          => StrictTVar m StdGen
                          -> STM m ChurnMode
                          -> PeerMetrics m peerAddr
                          -> PeerSelectionPolicy peerAddr m
simplePeerSelectionPolicy rngVar getChurnMode metrics = PeerSelectionPolicy {
      policyPickKnownPeersForGossip = simplePromotionPolicy,
      policyPickColdPeersToPromote  = simplePromotionPolicy,
      policyPickWarmPeersToPromote  = simplePromotionPolicy,

      policyPickHotPeersToDemote    = hotDemotionPolicy,
      policyPickWarmPeersToDemote   = warmDemotionPolicy,
      policyPickColdPeersToForget   = coldForgetPolicy,

      policyFindPublicRootTimeout   = 5,    -- seconds
      policyMaxInProgressGossipReqs = 2,
      policyGossipRetryTime         = 3600, -- seconds
      policyGossipBatchWaitTime     = 3,    -- seconds
      policyGossipOverallTimeout    = 10    -- seconds
    }
  where

     -- Add scaled random number in order to prevent ordering based on SockAddr
    addRand :: Set.Set peerAddr
            -> (peerAddr -> Word32 -> (peerAddr, Word32))
            -> STM m (Map.Map peerAddr Word32)
    addRand available scaleFn = do
      inRng <- readTVar rngVar

      let (rng, rng') = split inRng
          rns = take (Set.size available) $ unfoldr (Just . random)  rng :: [Word32]
          available' = Map.fromList $ zipWith scaleFn (Set.toList available) rns
      writeTVar rngVar rng'
      return available'

    hotDemotionPolicy :: PickPolicy peerAddr m
    hotDemotionPolicy _ _ _ available pickNum = do
        mode <- getChurnMode
        scores <- case mode of
                       ChurnModeNormal ->
                           upstreamyness <$> getHeaderMetrics metrics
                       ChurnModeBulkSync ->
                           fetchyness <$> getFetchedMetrics metrics
        available' <- addRand available (,)
        return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn (\(peer, rn) ->
                          (Map.findWithDefault 0 peer scores, rn))
             . Map.assocs
             $ available'

    -- Randomly pick peers to demote, peeers with knownPeerTepid set are twice
    -- as likely to be demoted.
    warmDemotionPolicy :: PickPolicy peerAddr m
    warmDemotionPolicy _ _ isTepid available pickNum = do
      available' <- addRand available (tepidWeight isTepid)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'


    -- Randomly pick peers to forget, peers with failures are more likely to
    -- be forgotten.
    coldForgetPolicy :: PickPolicy peerAddr m
    coldForgetPolicy _ failCnt _ available pickNum = do
      available' <- addRand available (failWeight failCnt)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    simplePromotionPolicy :: PickPolicy peerAddr m
    simplePromotionPolicy _ _ _ available pickNum = do
      available' <- addRand available (,)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    -- Failures lowers r
    failWeight :: (peerAddr -> Int)
                -> peerAddr
                -> Word32
                -> (peerAddr, Word32)
    failWeight failCnt peer r =
        (peer, r `div` fromIntegral (failCnt peer + 1))

    -- Tepid flag cuts r in half
    tepidWeight :: (peerAddr -> Bool)
                -> peerAddr
                -> Word32
                -> (peerAddr, Word32)
    tepidWeight isTepid peer r =
          if isTepid peer then (peer, r `div` 2)
                          else (peer, r)


--
-- PrunePolicy
--

-- | Sort by upstreamness and a random score.
--
-- Note: this 'PrunePolicy' does not depend on 'igsConnections'.  We put
-- 'igsPrng' in 'InboundGovernorState' only to show that we can have
-- a 'PrunePolicy' which depends on the 'InboundGovernorState' as a more
-- refined policy would do.
--
-- /complexity:/ \(\mathcal{O}(n\log\;n)\)
--
-- TODO: complexity could be improved.
--
prunePolicy :: ( MonadSTM m
               , Ord peerAddr
               )
            => StrictTVar m InboundGovernorObservableState
            -> PrunePolicy peerAddr (STM m)
prunePolicy stateVar mp n = do
    state <- readTVar stateVar
    let (prng', prng'') = Rnd.split (igosPrng state)
    writeTVar stateVar (state { igosPrng = prng'' })

    return
      $ Set.fromList
      . take n
      . map (fst . fst)
      -- 'True' values (upstream / outbound connections) will sort last.
      . sortOn (\((_, connType), score) -> (isUpstream connType, score, connType))
      . zip (Map.assocs mp)
      $ (Rnd.randoms prng' :: [Int])
  where
    isUpstream :: ConnectionType -> Bool
    isUpstream = \connType ->
      case connType of
        UnnegotiatedConn Outbound -> True
        UnnegotiatedConn Inbound  -> False
        OutboundIdleConn _        -> True
        InboundIdleConn         _ -> False
        NegotiatedConn Outbound _ -> True
        NegotiatedConn Inbound  _ -> False
        DuplexConn                -> True
