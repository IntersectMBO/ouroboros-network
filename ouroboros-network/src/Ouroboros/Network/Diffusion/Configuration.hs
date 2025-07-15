{-# LANGUAGE NamedFieldPuns #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- nb. the module Ouroboros.Network.Diffusion.Governor should be imported qualified as PeerSelection by convention to aid comprehension

module Ouroboros.Network.Diffusion.Configuration
  ( defaultAcceptedConnectionsLimit
  , defaultPeerSharing
  , defaultDeadlineTargets
  , defaultDeadlineChurnInterval
  , defaultBulkChurnInterval
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , DiffusionMode (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
  , ConsensusMode (..)
  , defaultConsensusMode
  , defaultEgressPollInterval
  , deactivateTimeout
  , closeConnectionTimeout
  , peerMetricsConfiguration
  , defaultTimeWaitTimeout
  , defaultProtocolIdleTimeout
  , defaultResetTimeout
  , handshake_QUERY_SHUTDOWN_DELAY
  , ps_POLICY_PEER_SHARE_STICKY_TIME
  , ps_POLICY_PEER_SHARE_MAX_PEERS
  , local_PROTOCOL_IDLE_TIMEOUT
  , local_TIME_WAIT_TIMEOUT
  ) where

import Control.Monad.Class.MonadTime.SI

import Cardano.Network.ConsensusMode
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode.Version (DiffusionMode (..))
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSharing (ps_POLICY_PEER_SHARE_MAX_PEERS,
           ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.Handshake (handshake_QUERY_SHUTDOWN_DELAY)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))

-- | Outbound governor targets
-- Targets may vary depending on whether a node is operating in
-- Genesis mode.

-- | Default peer targets in Praos mode
--
defaultDeadlineTargets :: PeerSelectionTargets
defaultDeadlineTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 60,
    targetNumberOfKnownPeers                = 150,
    targetNumberOfEstablishedPeers          = 30,
    targetNumberOfActivePeers               = 20,
    targetNumberOfKnownBigLedgerPeers       = 15,
    targetNumberOfEstablishedBigLedgerPeers = 10,
    targetNumberOfActiveBigLedgerPeers      = 5 }

-- | Inbound governor targets
--
defaultAcceptedConnectionsLimit :: AcceptedConnectionsLimit
defaultAcceptedConnectionsLimit =
  AcceptedConnectionsLimit {
    acceptedConnectionsHardLimit = 512,
    acceptedConnectionsSoftLimit = 384,
    acceptedConnectionsDelay     = 5 }

-- | Node's peer sharing participation flag
--
defaultPeerSharing :: PeerSharing
defaultPeerSharing = PeerSharingEnabled

defaultDeadlineChurnInterval :: DiffTime
defaultDeadlineChurnInterval = 3300

defaultBulkChurnInterval :: DiffTime
defaultBulkChurnInterval = 900

--
-- Constants
--

-- | Protocol inactivity timeout for local (e.g. /node-to-client/) connections.
--
local_PROTOCOL_IDLE_TIMEOUT :: DiffTime
local_PROTOCOL_IDLE_TIMEOUT = 2 -- 2 seconds

-- | Used to set 'cmWaitTimeout' for local (e.g. /node-to-client/) connections.
--
local_TIME_WAIT_TIMEOUT :: DiffTime
local_TIME_WAIT_TIMEOUT = 0

-- | Mux egress queue polling
-- for tuning latency vs. network efficiency
defaultEgressPollInterval :: DiffTime
defaultEgressPollInterval = 0
