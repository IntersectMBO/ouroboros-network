
module Ouroboros.Network.BlockFetch.Decision.Trace where

import Ouroboros.Network.BlockFetch.ClientState (TraceLabelPeer)
import Ouroboros.Network.Block (Point)
import Ouroboros.Network.BlockFetch.Decision.Deadline (FetchDecision)

data TraceDecisionEvent peer header
  = PeersFetch [TraceLabelPeer peer (FetchDecision [Point header])]
  deriving (Show)
