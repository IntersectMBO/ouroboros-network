{-# LANGUAGE NamedFieldPuns #-}

-- | One stop shop for configuring diffusion layer for upstream clients
-- nb. the module Ouroboros.Network.Diffusion.Governor should be imported qualified as PeerSelection by convention to aid comprehension

module Ouroboros.Network.Diffusion.Configuration
  ( defaultAcceptedConnectionsLimit
  , defaultPeerSharing
  , defaultBlockFetchConfiguration
  , defaultChainSyncTimeout
  , defaultDeadlineTargets
  , defaultDeadlineChurnInterval
  , defaultBulkChurnInterval
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , BlockFetchConfiguration (..)
  , ChainSyncTimeout (..)
  , DiffusionMode (..)
  , MiniProtocolParameters (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
  , ConsensusMode (..)
  , defaultConsensusMode
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
import System.Random (randomRIO)

import Cardano.Network.ConsensusMode
import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
           GenesisBlockFetchConfiguration (..))
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, maxChainSyncTimeout, minChainSyncTimeout,
           peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode (DiffusionMode (..),
           MiniProtocolParameters (..), defaultMiniProtocolParameters)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSharing (ps_POLICY_PEER_SHARE_MAX_PEERS,
           ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Ouroboros.Network.Protocol.Handshake (handshake_QUERY_SHUTDOWN_DELAY)
import Ouroboros.Network.Protocol.Limits (shortWait)
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
defaultPeerSharing = PeerSharingDisabled

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

defaultChainSyncTimeout :: IO ChainSyncTimeout
defaultChainSyncTimeout = do
    -- These values approximately correspond to false positive
    -- thresholds for streaks of empty slots with 99% probability,
    -- 99.9% probability up to 99.999% probability.
    -- t = T_s [log (1-Y) / log (1-f)]
    -- Y = [0.99, 0.999...]
    -- T_s = slot length of 1s.
    -- f = 0.05
    -- The timeout is randomly picked per bearer to avoid all bearers
    -- going down at the same time in case of a long streak of empty
    -- slots.
    -- To avoid global synchronosation the timeout is picked uniformly
    -- from the interval 135 - 269, corresponds to the a 99.9% to
    -- 99.9999% thresholds.
    -- TODO: The timeout should be drawn at random everytime chainsync
    --       enters the must reply state. A static per connection timeout
    --       leads to selection preassure for connections with a large
    --       timeout, see #4244.
    mustReplyTimeout <- Just . realToFrac <$> randomRIO ( realToFrac minChainSyncTimeout :: Double
                                                        , realToFrac maxChainSyncTimeout :: Double
                                                        )
    return ChainSyncTimeout { canAwaitTimeout  = shortWait,
                              intersectTimeout = shortWait,
                              mustReplyTimeout,
                              idleTimeout      = Just 3673 }

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
