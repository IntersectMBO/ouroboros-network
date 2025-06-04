{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Network.Diffusion.Types
  ( Tracers
  , Configuration
  , Applications
  , CardanoPeerSelectionCounters
  , CardanoLocalRootConfig
  , CardanoTraceLocalRootPeers
  , CardanoTracePeerSelection
  , CardanoDebugPeerSelection
  ) where

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)

import Control.Exception (IOException)
import Network.Socket (SockAddr, Socket)

import Cardano.Network.PeerSelection.ExtraRootPeers (ExtraPeers)
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState (ExtraState)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Types
           (ExtraPeerSelectionSetsWithSizes)
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano
import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (DebugPeerSelection,
           PeerSelectionCounters, TracePeerSelection)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (LocalRootConfig)

type DNSResolverError = IOException

type Tracers =
  Diffusion.Tracers
    RemoteAddress NodeToNodeVersion  NodeToNodeVersionData
    LocalAddress  NodeToClientVersion NodeToClientVersionData
    DNSResolverError
    Cardano.ExtraState
    Cardano.DebugPeerSelectionState
    PeerTrustable
    (Cardano.ExtraPeers RemoteAddress)
    (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
    IO


type Configuration =
  Diffusion.Configuration
    PeerTrustable
    IO
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress


type Applications a =
  Diffusion.Applications
    RemoteAddress
    NodeToNodeVersion
    NodeToNodeVersionData
    LocalAddress
    NodeToClientVersion
    NodeToClientVersionData
    IO
    a



type CardanoLocalRootConfig = LocalRootConfig PeerTrustable


type CardanoTraceLocalRootPeers =
  TraceLocalRootPeers PeerTrustable RemoteAddress DNSResolverError


type CardanoTracePeerSelection =
  TracePeerSelection Cardano.DebugPeerSelectionState
                     PeerTrustable
                     (ExtraPeers SockAddr)
                     RemoteAddress


type CardanoDebugPeerSelection =
  DebugPeerSelection ExtraState
                     PeerTrustable
                     (ExtraPeers RemoteAddress)
                     RemoteAddress


type CardanoPeerSelectionCounters =
  PeerSelectionCounters (ExtraPeerSelectionSetsWithSizes RemoteAddress)
