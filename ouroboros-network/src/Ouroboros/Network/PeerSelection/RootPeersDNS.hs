module Ouroboros.Network.PeerSelection.RootPeersDNS
  ( module Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore
  , DnsTrace (..)
  , DnsPeersKind (..)
  , PeerActionsDNS (..)
  ) where

import Data.IP (IP)
import Network.Socket (PortNumber)

import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSSemaphore

-- | Record of some parameters that are commonly used together
--
data PeerActionsDNS peeraddr resolver exception m = PeerActionsDNS {
  paToPeerAddr   :: IP -> PortNumber -> peeraddr,
  paDnsActions   :: DNSActions peeraddr resolver exception m,
  paDnsSemaphore :: DNSSemaphore m
  }
