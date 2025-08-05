{-# LANGUAGE NamedFieldPuns #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- nb. the module Ouroboros.Network.Diffusion.Governor should be imported qualified as PeerSelection by convention to aid comprehension

module Ouroboros.Network.Diffusion.Configuration
  ( defaultAcceptedConnectionsLimit
  , defaultPeerSharing
  , defaultBlockFetchConfiguration
  , defaultDeadlineTargets
  , defaultDeadlineChurnInterval
  , defaultBulkChurnInterval
  , BlockProducerOrRelay (..)
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , BlockFetchConfiguration (..)
  , DiffusionMode (..)
  , MiniProtocolParameters (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
  , ConsensusMode (..)
  , defaultConsensusMode
  , defaultEgressPollInterval
  , defaultMiniProtocolParameters
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
import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
           GenesisBlockFetchConfiguration (..))
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode (DiffusionMode (..),
           MiniProtocolParameters (..), defaultMiniProtocolParameters)
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


-- | A Boolean like type to differentiate between a node which is configured as
-- a block producer and a relay.  Some default options depend on that value.
--
data BlockProducerOrRelay = BlockProducer | Relay
  deriving Show


-- | Default peer targets in Praos mode
--
defaultDeadlineTargets :: BlockProducerOrRelay
                       -- ^ block producer or relay node
                       -> PeerSelectionTargets
defaultDeadlineTargets bp =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = case bp of { BlockProducer -> 100; Relay -> 60  },
    targetNumberOfKnownPeers                = case bp of { BlockProducer -> 100; Relay -> 150 },
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
defaultPeerSharing :: BlockProducerOrRelay
                   -> PeerSharing
defaultPeerSharing BlockProducer = PeerSharingDisabled
defaultPeerSharing Relay         = PeerSharingEnabled

-- | Configuration for FetchDecisionPolicy.
--
defaultBlockFetchConfiguration :: Int -> BlockFetchConfiguration
defaultBlockFetchConfiguration bfcSalt =
  BlockFetchConfiguration {
    bfcMaxConcurrencyBulkSync = 1,
    bfcMaxConcurrencyDeadline = 1,
    bfcMaxRequestsInflight    = fromIntegral $ blockFetchPipeliningMax defaultMiniProtocolParameters,
    bfcDecisionLoopIntervalGenesis = 0.04,  -- 40ms
    bfcDecisionLoopIntervalPraos = 0.01,  -- 10ms
    bfcGenesisBFConfig        = GenesisBlockFetchConfiguration
      { gbfcGracePeriod = 10 },  -- seconds
    bfcSalt }

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
