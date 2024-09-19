{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Diffusion.Configuration
  ( DefaultNumBootstrapPeers (..)
  , defaultNumBootstrapPeers
  , defaultPeerSelectionTargets
  , defaultAcceptedConnectionsLimit
  , defaultDiffusionMode
  , defaultPeerSharing
  , defaultBlockFetchConfiguration
  , defaultChainSyncTimeout
  , defaultEnableNewTxSubmissionProtocol
    -- re-exports
  , AcceptedConnectionsLimit (..)
  , BlockFetchConfiguration (..)
  , ChainSyncTimeout (..)
  , MiniProtocolParameters (..)
  , P2P (..)
  , PeerSelectionTargets (..)
  , PeerSharing (..)
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
  ) where

import System.Random (randomRIO)

import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import Ouroboros.Network.ConnectionManager.Core (defaultProtocolIdleTimeout,
           defaultResetTimeout, defaultTimeWaitTimeout)
import Ouroboros.Network.Diffusion (P2P (..))
import Ouroboros.Network.Diffusion.Policies (closeConnectionTimeout,
           deactivateTimeout, maxChainSyncTimeout, minChainSyncTimeout,
           peerMetricsConfiguration)
import Ouroboros.Network.NodeToNode (MiniProtocolParameters (..),
           defaultMiniProtocolParameters)
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSharing (ps_POLICY_PEER_SHARE_MAX_PEERS,
           ps_POLICY_PEER_SHARE_STICKY_TIME)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout (..))
import Ouroboros.Network.Protocol.Handshake (handshake_QUERY_SHUTDOWN_DELAY)
import Ouroboros.Network.Protocol.Limits (shortWait)
import Ouroboros.Network.Server.RateLimiting (AcceptedConnectionsLimit (..))
import Ouroboros.Network.TxSubmission.Inbound.Server
           (EnableNewTxSubmissionProtocol (..))


newtype DefaultNumBootstrapPeers = DefaultNumBootstrapPeers { getDefaultNumBootstrapPeers :: Int }
  deriving (Eq, Show)

defaultNumBootstrapPeers :: DefaultNumBootstrapPeers
defaultNumBootstrapPeers = DefaultNumBootstrapPeers 30

-- |Outbound governor targets
--
defaultPeerSelectionTargets :: PeerSelectionTargets
defaultPeerSelectionTargets =
  PeerSelectionTargets {
    targetNumberOfRootPeers                 = 60,
    targetNumberOfKnownPeers                = 85,
    targetNumberOfEstablishedPeers          = 40,
    targetNumberOfActivePeers               = 15,
    targetNumberOfKnownBigLedgerPeers       = 15,
    targetNumberOfEstablishedBigLedgerPeers = 10,
    targetNumberOfActiveBigLedgerPeers      = 5 }

-- |Inbound governor targets
--
defaultAcceptedConnectionsLimit :: AcceptedConnectionsLimit
defaultAcceptedConnectionsLimit =
  AcceptedConnectionsLimit {
    acceptedConnectionsHardLimit = 512,
    acceptedConnectionsSoftLimit = 384,
    acceptedConnectionsDelay     = 5 }

-- |Principal mode of network operation
defaultDiffusionMode :: P2P
defaultDiffusionMode = NonP2P

-- |Node's peer sharing participation flag
defaultPeerSharing :: PeerSharing
defaultPeerSharing = PeerSharingDisabled

-- | Configuration for FetchDecisionPolicy.
defaultBlockFetchConfiguration :: Int -> BlockFetchConfiguration
defaultBlockFetchConfiguration bfcSalt =
  BlockFetchConfiguration {
    bfcMaxConcurrencyBulkSync = 1,
    bfcMaxConcurrencyDeadline = 1,
    bfcMaxRequestsInflight    = fromIntegral $ blockFetchPipeliningMax defaultMiniProtocolParameters,
    bfcDecisionLoopInterval   = 0.01, -- 10ms
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

defaultEnableNewTxSubmissionProtocol :: EnableNewTxSubmissionProtocol
defaultEnableNewTxSubmissionProtocol = DisableNewTxSubmissionProtocol
