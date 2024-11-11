module Ouroboros.Network.PeerSelection.RootPeersDNS (PeerActionsDNS (..)) where

import Data.IP (IP)
import Network.Socket (PortNumber)

import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions

-- | Record of some parameters that are commonly used together
--
data PeerActionsDNS peeraddr resolver exception m = PeerActionsDNS {
  paToPeerAddr :: IP -> PortNumber -> peeraddr,
  paDnsActions :: DNSActions resolver exception m
  }
