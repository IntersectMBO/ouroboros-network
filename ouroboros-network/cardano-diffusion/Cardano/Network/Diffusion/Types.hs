{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Cardano.Network.Diffusion.Types
  ( CardanoNodeArguments (..)
  , CardanoConsensusArguments (..)
  , CardanoTracers
  , Diffusion.Tracers (..)
  , Diffusion.nullTracers
  , CardanoConfiguration
  , CardanoApplications
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
data CardanoNodeArguments m = CardanoNodeArguments {
    consensusMode          :: ConsensusMode,
    -- ^ Field which comes from `cardano-node` configuration file
    -- (`ncConsensusMode`).
    genesisPeerTargets     :: PeerSelectionTargets,
    -- ^ Fields which come from `cardano-node` configuration file.
    minNumOfBigLedgerPeers :: NumberOfBigLedgerPeers,
    -- ^ Field which comes from `cardano-node` configuration file.
    tracerChurnMode        :: Tracer m Cardano.Churn.TracerChurnMode
    -- ^ Field which comes from `cardano-node` tracing system.
  }

-- | Arguments required to instantiate Cardano Node Diffusion.
--
data CardanoConsensusArguments ntnAddr m =
  CardanoConsensusArguments {
    churnModeVar           :: StrictTVar m Cardano.Churn.ChurnMode,
    -- ^ churn mode var is created in `ouroboros-consensus-diffusion` and shared
    -- with diffusion and peer selection policy (instantiated in
    -- `ouroboros-consensus-diffusion):
    --
    -- * peer selection is updating it;
    -- * peer selection policy is reading it.

    churnMetrics           :: PeerMetrics m ntnAddr,
    -- ^ churn metrics are used in cardano diffusion by `SIGUSR1` handler; in
    -- consensus they are passed to
    --
    -- * applications (e.g. `chain-sync` and `block-fetch`), where they are
    --   updated;
    -- * peer selection policy, where they are read.

    ledgerPeersAPI         :: LedgerPeersConsensusInterface (Cardano.LedgerPeersConsensusInterface m) m,
    -- ^ ledger and consensus APIs

    readUseBootstrapPeers  :: STM m UseBootstrapPeers
    -- ^ `UseBootstrapPeers` from topology file.
    --
    -- `readUseBootstrapPeers` is created in `cardano-node`, then passed to
    -- consensus through `RunNodeArgs`, from which it is passed to diffusion.
  }

type DNSResolverError = IOException

type CardanoTracers m =
  Diffusion.Tracers
    RemoteAddress NodeToNodeVersion  NodeToNodeVersionData
    LocalAddress  NodeToClientVersion NodeToClientVersionData
    DNSResolverError
    Cardano.ExtraState
    Cardano.DebugPeerSelectionState
    PeerTrustable
    (Cardano.ExtraPeers RemoteAddress)
    (Cardano.ExtraPeerSelectionSetsWithSizes RemoteAddress)
    m


type CardanoConfiguration m =
  Diffusion.Configuration
    PeerTrustable
    m
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress


type CardanoApplications m a =
  Diffusion.Applications
    RemoteAddress
    NodeToNodeVersion
    NodeToNodeVersionData
    LocalAddress
    NodeToClientVersion
    NodeToClientVersionData
    m a



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
