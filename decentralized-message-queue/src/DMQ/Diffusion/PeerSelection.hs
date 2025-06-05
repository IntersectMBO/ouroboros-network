module DMQ.Diffusion.PeerSelection where

import Data.Set (Set)
import Data.Set qualified as Set
import Network.Socket (SockAddr)
import Ouroboros.Network.PeerSelection.Governor.Types
import System.Random (Random (..), StdGen)

-- | Trivial peer selection policy used as dummy value
--
policy :: StdGen -> PeerSelectionPolicy SockAddr IO
policy gen =
  PeerSelectionPolicy {
    policyPickKnownPeersForPeerShare = \_ _ _ -> pickTrivially
  , policyPickColdPeersToForget   = \_ _ _    -> pickTrivially
  , policyPickColdPeersToPromote  = \_ _ _    -> pickTrivially
  , policyPickWarmPeersToPromote  = \_ _ _    -> pickTrivially
  , policyPickHotPeersToDemote    = \_ _ _    -> pickTrivially
  , policyPickWarmPeersToDemote   = \_ _ _    -> pickTrivially
  , policyPickInboundPeers        = \_ _ _    -> pickTrivially
  , policyFindPublicRootTimeout   = 5
  , policyMaxInProgressPeerShareReqs = 0
  , policyPeerShareRetryTime         = 0 -- seconds
  , policyPeerShareBatchWaitTime     = 0 -- seconds
  , policyPeerShareOverallTimeout    = 0 -- seconds
  , policyPeerShareActivationDelay   = 2 -- seconds
  }
  where
    pickTrivially :: Applicative m => Set SockAddr -> Int -> m (Set SockAddr)
    pickTrivially set n = pure
                        . fst
                        $ go gen (Set.toList set) n []
      where
        go g _ 0 acc  = (Set.fromList acc, g)
        go g [] _ acc = (Set.fromList acc, g)
        go g xs k acc =
          let (idx, g') = randomR (0, length xs - 1) g
              picked = xs !! idx
              xs' = take idx xs ++ drop (idx + 1) xs
          in go g' xs' (k - 1) (picked : acc)
