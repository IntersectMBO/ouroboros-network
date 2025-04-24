{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Cardano.Network.Diffusion.Policies where

import Control.Concurrent.Class.MonadSTM.Strict

import Cardano.Network.PeerSelection.Churn (ChurnMode (..))
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Ouroboros.Network.Diffusion.Policies (addRand, optionalMerge,
           simplePeerSelectionPolicy)
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.PeerMetric
import System.Random

simpleChurnModePeerSelectionPolicy
  :: forall m peerAddr.
    ( MonadSTM m
    , Ord peerAddr
    )
  => StrictTVar m StdGen
  -> STM m ChurnMode
  -> PeerMetrics m peerAddr
  -> RepromoteDelay
  -- ^ delay on error
  -> PeerSelectionPolicy peerAddr m
simpleChurnModePeerSelectionPolicy rngVar getChurnMode metrics errorDelay =
  (simplePeerSelectionPolicy rngVar metrics errorDelay) {
    policyPickHotPeersToDemote = hotDemotionPolicy
  }
  where
    hotDemotionPolicy :: PickPolicy peerAddr (STM m)
    hotDemotionPolicy _ _ _ available pickNum = do
        mode <- getChurnMode
        scores <- case mode of
                       ChurnModeNormal -> do
                           jpm <- joinedPeerMetricAt metrics
                           hup <- upstreamyness metrics
                           bup <- fetchynessBlocks metrics
                           return $ Map.unionWith (+) hup bup `optionalMerge` jpm

                       ChurnModeBulkSync -> do
                           jpm <- joinedPeerMetricAt metrics
                           bup <- fetchynessBytes metrics
                           return $ bup `optionalMerge` jpm

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
