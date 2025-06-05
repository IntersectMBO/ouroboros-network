{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Network.Diffusion.Types
  ( CardanoArguments (..)
  , CardanoTracers
  , CardanoConfiguration
  , CardanoApplications
  , Diffusion.Tracers (..)
  , Diffusion.Configuration (..)
  , Diffusion.Applications (..)
  , CardanoPeerSelectionCounters
  , CardanoLocalRootConfig
  , CardanoTraceLocalRootPeers
  , CardanoTracePeerSelection
  , CardanoDebugPeerSelection
    -- * Re-exports
  , PeerMetrics
  , Cardano.Churn.TracerChurnMode
  ) where


import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (IOException)
import Control.Tracer (Tracer)
import Network.Socket (SockAddr, Socket)

import Cardano.Network.LedgerPeerConsensusInterface qualified as Cardano
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Cardano.Network.PeerSelection.Churn qualified as Cardano.Churn
import Cardano.Network.PeerSelection.ExtraRootPeers (ExtraPeers)
import Cardano.Network.PeerSelection.ExtraRootPeers qualified as Cardano
import Cardano.Network.PeerSelection.Governor.PeerSelectionState (ExtraState)
import Cardano.Network.PeerSelection.Governor.PeerSelectionState qualified as Cardano
import Cardano.Network.PeerSelection.Governor.Types qualified as Cardano
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import Cardano.Network.Types (NumberOfBigLedgerPeers (..))

import Ouroboros.Network.Diffusion qualified as Diffusion
import Ouroboros.Network.Diffusion.Configuration (ConsensusMode)
import Ouroboros.Network.NodeToClient (LocalAddress, LocalSocket,
           NodeToClientVersion, NodeToClientVersionData)
import Ouroboros.Network.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.PeerSelection.Governor.Types (DebugPeerSelection,
           PeerSelectionCounters, PeerSelectionTargets (..), TracePeerSelection)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers (LocalRootConfig)

-- | Arguments required to instantiate Cardano Node Diffusion
--
-- NOTE: it is instantiated in `ouroboros-consensus-diffusion`.
-- TODO: we might need to split this type into two parts.
--
data CardanoArguments m =
  CardanoArguments {
    consensusMode         :: ConsensusMode
  , numBigLedgerPeers     :: NumberOfBigLedgerPeers
  , genesisPeerTargets    :: PeerSelectionTargets
  , readUseBootstrapPeers :: STM m UseBootstrapPeers
  , tracerChurnMode       :: Tracer m Cardano.Churn.TracerChurnMode
  , churnModeVar          :: StrictTVar m Cardano.Churn.ChurnMode
  , churnMetrics          :: PeerMetrics m RemoteAddress
  , ledgerPeersAPI        :: LedgerPeersConsensusInterface (Cardano.LedgerPeersConsensusInterface m) m
  }

type DNSResolverError = IOException

type CardanoTracers =
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


type CardanoConfiguration =
  Diffusion.Configuration
    PeerTrustable
    IO
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress


type CardanoApplications a =
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
  PeerSelectionCounters (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
