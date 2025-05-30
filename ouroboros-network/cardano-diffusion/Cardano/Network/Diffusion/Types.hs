{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Network.Diffusion.Types where

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)

import Control.Exception (IOException, SomeException)
import Network.Socket (SockAddr, Socket)

import Cardano.Network.PeerSelection.ExtraRootPeers (ExtraPeers)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState (ExtraState)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Types
           (ExtraPeerSelectionSetsWithSizes)
import Ouroboros.Network.Diffusion.Types (Configuration, Tracers)
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

type CardanoConfiguration =
  Configuration
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
