{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Ouroboros.Network.Diffusion.Policies where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadTime.SI

import Data.List (sortOn, unfoldr)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Word (Word32)
import System.Random
import System.Random qualified as Rnd

import Ouroboros.Network.ConnectionManager.Types (ConnectionType (..),
           Provenance (..), PrunePolicy)
import Ouroboros.Network.ExitPolicy as ExitPolicy
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.PeerMetric


-- | Timeout for 'spsDeactivateTimeout' and churn hot to warm demotions.
--
-- The maximal timeout on 'ChainSync' (in 'StMustReply' state) is @269s@,
-- see `maxChainSyncTimeout` below.
--
deactivateTimeout :: DiffTime
deactivateTimeout = 300

-- | Timeout for 'spsCloseConnectionTimeout'.
--
-- This timeout depends on 'KeepAlive' and 'TipSample' timeouts.  'KeepAlive'
-- keeps agency most of the time, but 'TipSample' can give away its agency for
-- longer periods of time.  Here we allow it to get 6 blocks (assuming a new
-- block every @20s@).
--
closeConnectionTimeout :: DiffTime
closeConnectionTimeout = 120


-- | Chain sync `mustReplayTimeout` lower bound.
--
minChainSyncTimeout :: DiffTime
minChainSyncTimeout = 135


-- | Chain sync `mustReplayTimeout` upper bound.
--
maxChainSyncTimeout :: DiffTime
maxChainSyncTimeout = 269

-- | Churn timeouts after 60s trying to establish a connection.
--
-- This doesn't mean the connection is terminated after it, just churns moves
-- on.
--
churnEstablishConnectionTimeout :: DiffTime
churnEstablishConnectionTimeout = 60


-- | Number of events tracked by 'PeerMetrics'.  This corresponds to one hour of
-- blocks on mainnet.
--
-- TODO: issue #3866
--
peerMetricsConfiguration :: PeerMetricsConfiguration
peerMetricsConfiguration = PeerMetricsConfiguration {
    maxEntriesToTrack = 180
  }


-- | Minimal delay between adding inbound peers to known set of outbound
-- governor.
--
-- It is set to 60s, the same as the peer sharing request timeout.
--
inboundPeersRetryDelay :: DiffTime
inboundPeersRetryDelay = 60


-- | Maximal number of light peers included at once.
--
maxInboundPeers :: Int
maxInboundPeers = 10


-- | Merge two dictionaries where values of the first one are obligatory, while
-- the second one are optional.
--
optionalMerge
    :: Ord k
    => Map k a
    -> Map k b
    -> Map k (a, Maybe b)
optionalMerge = Map.merge (Map.mapMissing (\_ a -> (a, Nothing)))
                           Map.dropMissing
                          (Map.zipWithMatched (\_ a b -> (a, Just b)))



simplePeerSelectionPolicy :: forall m peerAddr.
                             ( MonadSTM m
                             , Ord peerAddr
                             )
                          => StrictTVar m StdGen
                          -> PeerMetrics m peerAddr
                          -> RepromoteDelay
                          -- ^ delay on error
                          -> PeerSelectionPolicy peerAddr m
simplePeerSelectionPolicy rngVar metrics errorDelay = PeerSelectionPolicy {
      policyPickKnownPeersForPeerShare = simplePromotionPolicy,
      policyPickColdPeersToPromote     = simplePromotionPolicy,
      policyPickWarmPeersToPromote     = simplePromotionPolicy,
      policyPickInboundPeers           = simplePromotionPolicy,

      policyPickHotPeersToDemote  = hotDemotionPolicy,
      policyPickWarmPeersToDemote = warmDemotionPolicy,
      policyPickColdPeersToForget = coldForgetPolicy,

      policyFindPublicRootTimeout      = 5,    -- seconds
      policyMaxInProgressPeerShareReqs = 2,
      policyPeerShareRetryTime         = 900,  -- seconds
      policyPeerShareBatchWaitTime     = 3,    -- seconds
      policyPeerShareOverallTimeout    = 10,   -- seconds
      policyPeerShareActivationDelay   = 300,  -- seconds

      policyErrorDelay = ExitPolicy.repromoteDelay errorDelay
    }
  where

    hotDemotionPolicy :: PickPolicy peerAddr (STM m)
    hotDemotionPolicy _ _ _ available pickNum = do
        jpm <- joinedPeerMetricAt metrics
        hup <- upstreamyness metrics
        bup <- fetchynessBlocks metrics

        let scores = Map.unionWith (+) hup bup `optionalMerge` jpm

        available' <- addRand rngVar available (,)
        return $ Set.fromList
             . map fst
             . take pickNum
               -- order the results, resolve the ties using slot number when
               -- a peer joined the leader board.
               --
               -- note: this will prefer to preserve newer peers, whose results
               -- less certain than peers who entered leader board earlier.
             . sortOn (\(peer, rn) ->
                          (Map.findWithDefault (0, Nothing) peer scores, rn))
             . Map.assocs
             $ available'

    -- Randomly pick peers to demote, peers with knownPeerTepid set are twice
    -- as likely to be demoted.
    warmDemotionPolicy :: PickPolicy peerAddr (STM m)
    warmDemotionPolicy _ _ isTepid available pickNum = do
      available' <- addRand rngVar available (tepidWeight isTepid)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'


    -- Randomly pick peers to forget, peers with failures are more likely to
    -- be forgotten.
    coldForgetPolicy :: PickPolicy peerAddr (STM m)
    coldForgetPolicy _ failCnt _ available pickNum = do
      available' <- addRand rngVar available (failWeight failCnt)
      return $ Set.fromList
             . map fst
             . take pickNum
             . sortOn snd
             . Map.assocs
             $ available'

    simplePromotionPolicy :: PickPolicy peerAddr (STM m)
    simplePromotionPolicy _ _ _ available pickNum = do
      available' <- addRand rngVar available (,)
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


 -- Add scaled random number in order to prevent ordering based on SockAddr
addRand :: ( MonadSTM m
           , Ord peerAddr
           )
        => StrictTVar m StdGen
        -> Set.Set peerAddr
        -> (peerAddr -> Word32 -> (peerAddr, Word32))
        -> STM m (Map.Map peerAddr Word32)
addRand rngVar available scaleFn = do
  inRng <- readTVar rngVar

  let (rng, rng') = splitGen inRng
      rns = take (Set.size available) $ unfoldr (Just . random)  rng :: [Word32]
      available' = Map.fromList $ zipWith scaleFn (Set.toList available) rns
  writeTVar rngVar rng'
  return available'

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
prunePolicy :: Ord peerAddr
            => PrunePolicy peerAddr
prunePolicy prng mp n =
        Set.fromList
      . take n
      . map (fst . fst)
      -- 'True' values (upstream / outbound connections) will sort last.
      . sortOn (\((_, connType), score) -> (isUpstream connType, score, connType))
      . zip (Map.assocs mp)
      $ (Rnd.randoms prng :: [Int])
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
