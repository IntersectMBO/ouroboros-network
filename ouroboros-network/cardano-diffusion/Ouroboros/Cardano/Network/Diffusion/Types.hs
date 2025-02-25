{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Ouroboros.Cardano.Network.Diffusion.Types where

import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)

import Control.Exception (IOException, SomeException)
import Network.DNS (Resolver)
import Network.Socket (SockAddr)

import Ouroboros.Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Churn qualified as Churn
import Ouroboros.Cardano.Network.PeerSelection.ExtraRootPeers (ExtraPeers)
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState
           (ExtraState)
import Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Ouroboros.Cardano.Network.PeerSelection.Governor.Types
           (ExtraPeerSelectionSetsWithSizes)
import Ouroboros.Network.Diffusion
import Ouroboros.Network.NodeToClient (LocalAddress, NodeToClientVersion,
           NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types
           (BootstrapPeersCriticalTimeoutError, DebugPeerSelection,
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

type CardanoArguments extraDebugState ntnAddr =
  Arguments ExtraState
            Cardano.DebugPeerSelectionState
            PeerTrustable
            (ExtraPeers ntnAddr)
            (Cardano.LedgerPeersConsensusInterface IO)
            (Churn.ExtraArguments IO)
            (ExtraPeerSelectionSetsWithSizes ntnAddr)
            BootstrapPeersCriticalTimeoutError
            Resolver
            IOException
            IO

type CardanoTraceLocalRootPeers =
  TraceLocalRootPeers PeerTrustable RemoteAddress SomeException

type CardanoTracePeerSelection =
  TracePeerSelection Cardano.DebugPeerSelectionState
                     PeerTrustable
                     (ExtraPeers SockAddr)
                     SockAddr

type CardanoDebugPeerSelection =
  DebugPeerSelection ExtraState
                     PeerTrustable
                     (ExtraPeers SockAddr)
                     SockAddr

type CardanoPeerSelectionCounters =
  PeerSelectionCounters (ExtraPeerSelectionSetsWithSizes SockAddr)
