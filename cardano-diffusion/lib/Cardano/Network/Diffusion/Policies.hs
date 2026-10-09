{-# LANGUAGE ScopedTypeVariables #-}

-- Constants used in 'Ouroboros.Network.Diffusion'
module Cardano.Network.Diffusion.Policies where

import Control.Concurrent.Class.MonadSTM.Strict

import Cardano.Network.FetchMode (FetchMode (..), PraosFetchMode (..))
import Cardano.Network.PeerSelection.Churn (ChurnMode (..))
import Data.Map.Strict (Map)
import Ouroboros.Network.Block (SlotNo)
import Ouroboros.Network.Diffusion.Policies (deadlineHotScores,
           mkHotDemotionPolicy, optionalMerge, simplePeerSelectionPolicy)
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
  -> PeerSelectionPolicy peerAddr m
simpleChurnModePeerSelectionPolicy rngVar getChurnMode metrics =
  (simplePeerSelectionPolicy rngVar metrics) {
    policyPickHotPeersToDemote = mkHotDemotionPolicy rngVar hotScores
  }
  where
    -- The score this policy ranks hot peers by for demotion: in deadline mode
    -- the peer's 'upstreamyness' plus its 'fetchynessBlocks', while syncing
    -- its 'fetchynessBytes', with 'joinedPeerMetricAt' as the tie-break.
    hotScores :: STM m (Map peerAddr (Int, Maybe SlotNo))
    hotScores = do
        mode <- getChurnMode
        case mode of
             ChurnMode (PraosFetchMode FetchModeDeadline) ->
                 deadlineHotScores metrics
             ChurnMode (PraosFetchMode FetchModeBulkSync) ->
                 bytesScores
             ChurnMode GenesisFetchMode ->
                 bytesScores

    bytesScores :: STM m (Map peerAddr (Int, Maybe SlotNo))
    bytesScores = do
        jpm <- joinedPeerMetricAt metrics
        bup <- fetchynessBytes metrics
        return $ bup `optionalMerge` jpm
