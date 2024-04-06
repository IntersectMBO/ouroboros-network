module Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState (..)) where

data OutboundConnectionsState = ConnectedToExternalOutboundPeers
                              | ConnectedToOnlyLocalOutboundPeers
  deriving (Eq, Show)
