module Ouroboros.Cardano.Network.Types where

import Ouroboros.Cardano.Network.PublicRootPeers (ExtraPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)

data ChurnMode = ChurnModeBulkSync
               | ChurnModeNormal
               deriving Show

type CardanoPublicRootPeers peeraddr =
  PublicRootPeers (ExtraPeers peeraddr) peeraddr
