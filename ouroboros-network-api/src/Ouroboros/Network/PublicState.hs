{-# LANGUAGE NamedFieldPuns #-}

-- | Public state of P2P network which can be queryied using `cardano-cli`.
--
module Ouroboros.Network.PublicState where

import Data.Set (Set)
import Data.Set qualified as Set

data ConnectionManagerState = ConnectionManagerState {
    fullDuplexConns     :: !Int,
    -- ^ Number of full duplex connections (e.g. connections which negotiated
    -- `InitiatorAndResponderDiffusionMode` and are used in both directions) in
    -- the connection manager.
    duplexConns         :: !Int,
    -- ^ Number of connections which negotiated
    -- `InitiatorAndResponderDiffusionMode` in the connection manager.
    unidirectionalConns :: !Int,
    -- ^ Number of connections which negotiated `InitiatorOnlyDiffusionMode` in
    -- the connection manager.
    inboundConns        :: !Int,
    -- ^ Number of inbound connections in the connection manager.
    outobundConns       :: !Int
    -- ^ Number of outbound connections in the connection manager.
  }

instance Semigroup ConnectionManagerState where
  ConnectionManagerState {
    fullDuplexConns,
    duplexConns,
    unidirectionalConns,
    inboundConns,
    outobundConns
  }
    <>
    ConnectionManagerState {
      fullDuplexConns     = fullDuplexConns',
      duplexConns         = duplexConns',
      unidirectionalConns = unidirectionalConns',
      inboundConns        = inboundConns',
      outobundConns       = outobundConns'
    }
    =
    ConnectionManagerState {
      fullDuplexConns     = fullDuplexConns + fullDuplexConns',
      duplexConns         = duplexConns + duplexConns',
      unidirectionalConns = unidirectionalConns + unidirectionalConns',
      inboundConns        = inboundConns + inboundConns',
      outobundConns       = outobundConns + outobundConns'
    }

instance Monoid ConnectionManagerState where
  mempty =
    ConnectionManagerState {
      fullDuplexConns     = 0,
      duplexConns         = 0,
      unidirectionalConns = 0,
      inboundConns        = 0,
      outobundConns       = 0
    }


data InboundState peeraddr = InboundState {
    remoteHotSet  :: Set peeraddr,
    remoteWarmSet :: Set peeraddr,
    remoteColdSet :: Set peeraddr,
    remoteIdleSet :: Set peeraddr
  }

instance Ord peeraddr => Semigroup (InboundState peeraddr) where
    InboundState {
      remoteHotSet,
      remoteWarmSet,
      remoteColdSet,
      remoteIdleSet
    }
      <>
      InboundState {
        remoteHotSet  = remoteHotSet',
        remoteWarmSet = remoteWarmSet',
        remoteColdSet = remoteColdSet',
        remoteIdleSet = remoteIdleSet'
      }
      =
      InboundState {
        remoteHotSet  = remoteHotSet <> remoteHotSet',
        remoteWarmSet = remoteWarmSet <> remoteWarmSet',
        remoteColdSet = remoteColdSet <> remoteColdSet',
        remoteIdleSet = remoteIdleSet <> remoteIdleSet'
      }

instance Ord peeraddr => Monoid (InboundState peeraddr) where
  mempty = InboundState {
    remoteHotSet  = Set.empty,
    remoteWarmSet = Set.empty,
    remoteColdSet = Set.empty,
    remoteIdleSet = Set.empty
  }


data OutboundState peeraddr = OutboundState {
    knownPeers       :: Set peeraddr,
    establishedPeers :: Set peeraddr,
    activePeers      :: Set peeraddr
  }

instance Ord peeraddr => Semigroup (OutboundState peeraddr) where
  OutboundState {
    knownPeers,
    establishedPeers,
    activePeers
  }
    <> OutboundState {
      knownPeers = knownPeers',
      establishedPeers = establishedPeers',
      activePeers = activePeers'
    }
    =
    OutboundState {
      knownPeers       = knownPeers <> knownPeers',
      establishedPeers = establishedPeers <> establishedPeers',
      activePeers      = activePeers <> activePeers'
    }

instance Ord peeraddr => Monoid (OutboundState peeraddr) where
  mempty = OutboundState {
    knownPeers       = Set.empty,
    establishedPeers = Set.empty,
    activePeers      = Set.empty
  }

data NetworkState peeraddr = NetworkState {

    connectionManagerState :: ConnectionManagerState,
    -- TODO:
    -- handshakes          :: !(Map peeraddr (version, versionData)),
    inboundGovernorState   :: InboundState peeraddr,
    outboundGovernorState  :: OutboundState peeraddr
  }

instance Ord peeraddr => Semigroup (NetworkState peeraddr) where
  NetworkState {
    connectionManagerState,
    inboundGovernorState,
    outboundGovernorState
  }
    <> NetworkState {
      connectionManagerState = connectionManagerState',
      inboundGovernorState = inboundGovernorState',
      outboundGovernorState = outboundGovernorState'
    }
    =
    NetworkState {
      connectionManagerState = connectionManagerState <> connectionManagerState',
      inboundGovernorState   = inboundGovernorState <> inboundGovernorState',
      outboundGovernorState  = outboundGovernorState <> outboundGovernorState'
    }

instance Ord peeraddr => Monoid (NetworkState peeraddr) where
  mempty = NetworkState {
    connectionManagerState = mempty,
    inboundGovernorState   = mempty,
    outboundGovernorState  = mempty
  }

