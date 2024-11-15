{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Public state of P2P network which can be queryied using `cardano-cli`.
--
module Ouroboros.Network.PublicState
  ( -- * Network state
    NetworkState (..)
  , mapNetworkStateMonotonic
  , ConnectionManagerState (..)
  , InboundState (..)
  , mapInboundStateMonotonic
  , emptyInboundState
  , OutboundState (..)
  , mapOutboundStateMonotonic
    -- * Codecs
  , encodeNetworkState
  , decodeNetworkState
    -- * Re-exports
  , RemoteAddressEncoding (..)
  ) where

import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import Codec.Serialise (Serialise)
import Codec.Serialise.Class (decode, encode)
import Control.Monad (replicateM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Ouroboros.Network.ConnectionId
import Ouroboros.Network.ConnectionManager.Public
import Ouroboros.Network.PeerSelection.PeerSharing.Codec
           (RemoteAddressEncoding (..))


data ConnectionManagerState peeraddr = ConnectionManagerState {
    connectionMap                 :: Map (ConnectionId peeraddr) AbstractState,
    -- ^ map of connections, without outbound connections in
    -- `ReservedOutboundSt` state.

    registeredOutboundConnections :: Set peeraddr
    -- ^ set of outbound connections in `ReserverdOutboundSt` state.
  }
  deriving (Eq, Show)

-- | Map 'ConnectionManagerState'
--
mapConnectionManagerStateMonotonic
  :: (peeraddr -> peeraddr')
  -- ^ a monotonic map
  -> ConnectionManagerState peeraddr
  -> ConnectionManagerState peeraddr'
mapConnectionManagerStateMonotonic
  fn
  ConnectionManagerState {
      connectionMap,
      registeredOutboundConnections
  } =
  ConnectionManagerState {
    connectionMap                 = Map.mapKeysMonotonic (fmap fn) connectionMap,
    registeredOutboundConnections = Set.mapMonotonic fn registeredOutboundConnections
  }


data InboundState peeraddr = InboundState {
    remoteHotSet  :: !(Set (ConnectionId peeraddr)),
    remoteWarmSet :: !(Set (ConnectionId peeraddr)),
    remoteColdSet :: !(Set (ConnectionId peeraddr)),
    remoteIdleSet :: !(Set (ConnectionId peeraddr))
  }
  deriving (Eq, Show)

mapInboundStateMonotonic
  :: (peeraddr -> peeraddr')
  -- ^ a monotonic map
  -> InboundState peeraddr
  -> InboundState peeraddr'
mapInboundStateMonotonic
  fn
  InboundState {
    remoteHotSet,
    remoteWarmSet,
    remoteColdSet,
    remoteIdleSet
  } =
  InboundState {
    remoteHotSet  = Set.mapMonotonic (fmap fn) remoteHotSet,
    remoteWarmSet = Set.mapMonotonic (fmap fn) remoteWarmSet,
    remoteColdSet = Set.mapMonotonic (fmap fn) remoteColdSet,
    remoteIdleSet = Set.mapMonotonic (fmap fn) remoteIdleSet
  }

emptyInboundState :: InboundState peeraddr
emptyInboundState = InboundState {
    remoteHotSet  = Set.empty,
    remoteWarmSet = Set.empty,
    remoteColdSet = Set.empty,
    remoteIdleSet = Set.empty
  }

data OutboundState peeraddr = OutboundState {
    coldPeers :: Set peeraddr,
    warmPeers :: Set peeraddr,
    hotPeers  :: Set peeraddr
  }
  deriving (Eq, Show)

mapOutboundStateMonotonic
  :: (peeraddr -> peeraddr')
  -- ^ a monotonic map
  -> OutboundState peeraddr
  -> OutboundState peeraddr'
mapOutboundStateMonotonic
  fn
  OutboundState {
    coldPeers,
    warmPeers,
    hotPeers
  } =
  OutboundState {
    coldPeers = Set.mapMonotonic fn coldPeers,
    warmPeers = Set.mapMonotonic fn warmPeers,
    hotPeers  = Set.mapMonotonic fn hotPeers
  }



data NetworkState peeraddr = NetworkState {
    connectionManagerState :: ConnectionManagerState peeraddr,
    -- TODO:
    -- handshakes          :: !(Map (ConnectionId peeraddr) (version, versionData)),
    inboundGovernorState   :: InboundState peeraddr,
    outboundGovernorState  :: OutboundState peeraddr
  }
  deriving (Eq, Show)

mapNetworkStateMonotonic
  :: (peeraddr -> peeraddr')
  -- ^ a monotonic map
  -> NetworkState peeraddr
  -> NetworkState peeraddr'
mapNetworkStateMonotonic
  fn
  NetworkState {
    connectionManagerState,
    inboundGovernorState,
    outboundGovernorState
  }
  =
  NetworkState {
    connectionManagerState = mapConnectionManagerStateMonotonic fn
                             connectionManagerState,
    inboundGovernorState   = mapInboundStateMonotonic fn
                             inboundGovernorState,
    outboundGovernorState  = mapOutboundStateMonotonic fn
                             outboundGovernorState
  }


--
-- CBOR encoding / decoding
--

encodeNetworkState
    :: Serialise (RemoteAddressEncoding peeraddr)
    => NetworkState peeraddr
    -> Encoding
encodeNetworkState
  NetworkState {
    outboundGovernorState = OutboundState {
      coldPeers,
      warmPeers,
      hotPeers
    },
    inboundGovernorState = InboundState {
      remoteHotSet,
      remoteWarmSet,
      remoteColdSet,
      remoteIdleSet
    },
    connectionManagerState = ConnectionManagerState {
      connectionMap,
      registeredOutboundConnections
    }
  }
  =  encodeListLen 9
  -- outbound state
  <> encode (RemoteAddressEncoding `map` Set.toList coldPeers)
  <> encode (RemoteAddressEncoding `map` Set.toList warmPeers)
  <> encode (RemoteAddressEncoding `map` Set.toList hotPeers)

  -- inbound state
  <> encode (toPair <$> Set.toList remoteHotSet)
  <> encode (toPair <$> Set.toList remoteWarmSet)
  <> encode (toPair <$> Set.toList remoteColdSet)
  <> encode (toPair <$> Set.toList remoteIdleSet)

  -- connection manager state
  <> encodeListLen (fromIntegral $ Map.size connectionMap)
  <> Map.foldMapWithKey (\connId st -> encodeListLen 2
                                    <> encode (toPair connId)
                                    <> encodeConnState st)
                        connectionMap
  <> encode (RemoteAddressEncoding `map` Set.toList registeredOutboundConnections)

  where
    toPair :: ConnectionId addr -> (RemoteAddressEncoding addr, RemoteAddressEncoding addr)
    toPair ConnectionId { remoteAddress, localAddress } =
      (RemoteAddressEncoding remoteAddress, RemoteAddressEncoding localAddress)

    encodeConnState :: AbstractState -> Encoding
    encodeConnState UnknownConnectionSt =
         encodeListLen 1
      <> encodeWord8 0
    encodeConnState ReservedOutboundSt =
         encodeListLen 1
      <> encodeWord8 1
    encodeConnState (UnnegotiatedSt a) =
         encodeListLen 2
      <> encodeWord8 2
      <> encodeProvenance a
    encodeConnState (InboundIdleSt a) =
         encodeListLen 2
      <> encodeWord8 3
      <> encodeDataFlow a
    encodeConnState (InboundSt a) =
         encodeListLen 2
      <> encodeWord8 4
      <> encodeDataFlow a
    encodeConnState OutboundUniSt =
         encodeListLen 1
      <> encodeWord8 5
    encodeConnState (OutboundDupSt a) =
         encodeListLen 2
      <> encodeWord8 6
      <> encodeTimeoutExpired a
    encodeConnState (OutboundIdleSt a) =
         encodeListLen 2
      <> encodeWord8 7
      <> encodeDataFlow a
    encodeConnState DuplexSt =
         encodeListLen 1
      <> encodeWord8 8
    encodeConnState WaitRemoteIdleSt =
         encodeListLen 1
      <> encodeWord8 9
    encodeConnState TerminatingSt =
         encodeListLen 1
      <> encodeWord8 10
    encodeConnState TerminatedSt =
         encodeListLen 1
      <> encodeWord8 11

    encodeProvenance :: Provenance -> Encoding
    encodeProvenance Inbound  = encodeWord8 0
    encodeProvenance Outbound = encodeWord8 1

    encodeDataFlow :: DataFlow -> Encoding
    encodeDataFlow Unidirectional = encodeWord8 0
    encodeDataFlow Duplex         = encodeWord8 1

    encodeTimeoutExpired :: TimeoutExpired -> Encoding
    encodeTimeoutExpired Expired = encodeWord8 0
    encodeTimeoutExpired Ticking = encodeWord8 1


decodeNetworkState
  :: (Serialise (RemoteAddressEncoding peeraddr), Ord peeraddr)
  => Decoder s (NetworkState peeraddr)
decodeNetworkState = do
    _ <- decodeListLen
    -- outbound state
    coldPeers <- Set.fromList . map getRemoteAddressEncoding <$> decode
    warmPeers <- Set.fromList . map getRemoteAddressEncoding <$> decode
    hotPeers <- Set.fromList . map getRemoteAddressEncoding <$> decode

    -- inbound state
    remoteHotSet  <- Set.fromList . map fromPair <$> decode
    remoteWarmSet <- Set.fromList . map fromPair <$> decode
    remoteColdSet <- Set.fromList . map fromPair <$> decode
    remoteIdleSet <- Set.fromList . map fromPair <$> decode

    -- connection manager state
    a <- decodeListLen
    connectionMap
      <- fmap Map.fromList $ replicateM a $ do
      _ <- decodeListLen
      connId <- fromPair <$> decode
      st <- decodeConnState
      return (connId, st)
    registeredOutboundConnections
      <- Set.fromList . map getRemoteAddressEncoding <$> decode


    return NetworkState {
      outboundGovernorState = OutboundState {
        coldPeers,
        warmPeers,
        hotPeers
      },
      inboundGovernorState = InboundState {
        remoteHotSet,
        remoteWarmSet,
        remoteColdSet,
        remoteIdleSet
      },
      connectionManagerState = ConnectionManagerState {
        connectionMap,
        registeredOutboundConnections
      }
    }

  where
    fromPair :: (RemoteAddressEncoding addr, RemoteAddressEncoding addr) -> ConnectionId addr
    fromPair (RemoteAddressEncoding remoteAddress, RemoteAddressEncoding localAddress) =
      ConnectionId {remoteAddress, localAddress}

    decodeProvenance :: Decoder s Provenance
    decodeProvenance = do
      tag <- decodeWord8
      case tag of
        0 -> return Inbound
        1 -> return Outbound
        _ -> fail ("decodeProvenance: unknown tag " ++ show tag)

    decodeDataFlow :: Decoder s DataFlow
    decodeDataFlow = do
      tag <- decodeWord8
      case tag of
        0 -> return Unidirectional
        1 -> return Duplex
        _ -> fail ("decodeDataFlow: unknown tag " ++ show tag)

    decodeTimeoutExpired :: Decoder s TimeoutExpired
    decodeTimeoutExpired = do
      tag <- decodeWord8
      case tag of
        0 -> return Expired
        1 -> return Ticking
        _ -> fail ("decodeTimeoutExpired: unknown tag " ++ show tag)

    decodeConnState :: Decoder s AbstractState
    decodeConnState = do
      _ <- decodeListLen
      tag <- decodeWord8
      case tag of
        0  -> return UnknownConnectionSt
        1  -> return ReservedOutboundSt
        2  -> UnnegotiatedSt <$> decodeProvenance
        3  -> InboundIdleSt <$> decodeDataFlow
        4  -> InboundSt <$> decodeDataFlow
        5  -> return OutboundUniSt
        6  -> OutboundDupSt <$> decodeTimeoutExpired
        7  -> OutboundIdleSt <$> decodeDataFlow
        8  -> return DuplexSt
        9  -> return WaitRemoteIdleSt
        10 -> return TerminatingSt
        11 -> return TerminatedSt
        _  -> fail ("decodeConnState: unknown tag " ++ show tag)
