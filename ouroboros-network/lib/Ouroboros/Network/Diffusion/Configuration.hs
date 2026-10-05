{-# LANGUAGE NamedFieldPuns #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- nb. the module Ouroboros.Network.Diffusion.Governor should be imported qualified as PeerSelection by convention to aid comprehension

module Ouroboros.Network.Diffusion.Configuration
  ( defaultAcceptedConnectionsLimit
  , defaultPeerSharing
  , defaultDeadlineTargets
  , defaultDeadlineChurnInterval
  , defaultBulkChurnInterval
  , BlockProducerOrRelay (..)
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , DiffusionMode (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
  , defaultEgressPollInterval
  , defaultEgressSchedulingWith
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

import Network.Mux qualified as Mx
import Network.Mux.Types (MiniProtocolDir, MiniProtocolNum)
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, peerMetricsConfiguration)
import Ouroboros.Network.Diffusion.Types (EgressScheduling (..))
import Ouroboros.Network.DiffusionMode
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

-- | Used to set 'timeWaitTimeout' for local (e.g. /node-to-client/) connections.
--
local_TIME_WAIT_TIMEOUT :: DiffTime
local_TIME_WAIT_TIMEOUT = 0

-- | Mux egress queue polling
-- for tuning latency vs. network efficiency
defaultEgressPollInterval :: DiffTime
defaultEgressPollInterval = 0

-- | Scheduled egress at a 950 Mb/s budget, the line rate of a 1 Gb/s uplink:
-- a bucket of two batches, 15% of it reserved for the slice lane, the order
-- within a tier re-dealt every 599 s, and TCP_NOTSENT_LOWAT at one batch. The
-- lane rule is the caller's, since it names the tx-submission protocol.
defaultEgressSchedulingWith :: (MiniProtocolNum -> MiniProtocolDir -> Mx.Lane) -> EgressScheduling
defaultEgressSchedulingWith laneOf = EgressScheduling {
    esBudget         = 950e6 / 8,
    esCapacity       = 2 * 131072,
    esSlicePercent   = 15,
    esFloorPercent   = 10,
    esRotationPeriod = 599,
    esNotSentLowWat  = Just 131072,
    esLaneOf         = laneOf,
    -- two deadline churn rounds at their longest, an interval plus the churn
    -- governor's 600 s of fuzz each, plus the time it waits for the demotions
    -- of the second to complete: a partner has outlived two churns for certain
    esTenureThreshold = 2 * (defaultDeadlineChurnInterval + 600) + deactivateTimeout,
    -- an 88 kB block, a 37 kB list and a 1 MiB closure, announced in one
    -- block out of twenty slots
    esFreshMaxBytes       = 88000 + 37000 + 1048576,
    esFreshBytesPerSecond = fromIntegral (88000 + 37000 + 1048576 :: Int) * 0.05,
    -- about a quarter of an hour; a prime, like the other periods, so it does
    -- not compete with them
    esStrangerLock        = 887,
    -- the protocol numbers are the application's to name
    esUnchargedProtocols  = []
  }
