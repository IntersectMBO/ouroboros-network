{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Cardano.Network.Diffusion.Types where

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)

import Control.Exception (IOException, SomeException)
import Network.Socket (SockAddr, Socket)

import Ouroboros.Cardano.Network.PeerSelection.ExtraRootPeers (ExtraPeers)
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (ExtraState)
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.Types
           (ExtraPeerSelectionSetsWithSizes)
import Ouroboros.Network.Diffusion
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (DebugPeerSelection,
           PeerSelectionCounters, TracePeerSelection)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (LocalRootConfig)

type CardanoLocalRootConfig = LocalRootConfig PeerTrustable

type CardanoNetworkTracers =
  Tracers RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
          LocalAddress  NodeToClientVersion NodeToClientVersionData
          IOException Cardano.ExtraState Cardano.DebugPeerSelectionState
          PeerTrustable (ExtraPeers RemoteAddress)
          (ExtraPeerSelectionSetsWithSizes RemoteAddress) IO

type CardanoDiffusionConfiguration =
  DiffusionConfiguration
    PeerTrustable
    IO
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress

type CardanoTraceLocalRootPeers =
  TraceLocalRootPeers PeerTrustable RemoteAddress SomeException

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
