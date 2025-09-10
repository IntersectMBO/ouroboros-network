{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | This is the starting point for a module that will bring together the
-- overall node to client protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToClient
  ( nodeToClientProtocols
  , NodeToClientProtocols (..)
  , NodeToClientVersion (..)
  , NodeToClientVersionData (..)
  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo
  , connectToWithMux
    -- * Null Protocol Peers
  , chainSyncPeerNull
  , localStateQueryPeerNull
  , localTxSubmissionPeerNull
  , localTxMonitorPeerNull
    -- * Re-exported network interface
  , IOManager (..)
  , AssociateWithIOCP
  , withIOManager
  , LocalSnocket
  , localSnocket
  , LocalSocket (..)
  , LocalAddress (..)
  , LocalConnectionId
    -- * Versions
  , Versions (..)
  , versionedNodeToClientProtocols
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
    -- ** Codecs
  , nodeToClientHandshakeCodec
  , nodeToClientVersionCodec
  , nodeToClientCodecCBORTerm
    -- * Re-exports
  , ConnectionId (..)
  , MinimalInitiatorContext (..)
  , ResponderContext (..)
  , TraceSendRecv (..)
  , ProtocolLimitFailure
  , Handshake
  , HandshakeTr
  ) where

import Control.Concurrent.Async qualified as Async
import Control.Exception (SomeException)
import Control.Monad (forever)
import Control.Monad.Class.MonadTimer.SI

import Codec.CBOR.Term qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Void (Void, absurd)

import Network.Mux qualified as Mx
import Network.TypedProtocol.Peer.Client
import Network.TypedProtocol.Stateful.Peer.Client qualified as Stateful

import Ouroboros.Network.Context
import Ouroboros.Network.Driver (TraceSendRecv (..))
import Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient.Version
import Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Type qualified as ChainSync
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
import Ouroboros.Network.Protocol.LocalStateQuery.Client as LocalStateQuery
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as LocalStateQuery
import Ouroboros.Network.Protocol.LocalTxMonitor.Client as LocalTxMonitor
import Ouroboros.Network.Protocol.LocalTxMonitor.Type qualified as LocalTxMonitor
import Ouroboros.Network.Protocol.LocalTxSubmission.Client as LocalTxSubmission
import Ouroboros.Network.Protocol.LocalTxSubmission.Type qualified as LocalTxSubmission
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket

-- The Handshake tracer types are simply terrible.
type HandshakeTr ntcAddr ntcVersion =
    Mx.WithBearer (ConnectionId ntcAddr)
                  (TraceSendRecv (Handshake ntcVersion CBOR.Term))


-- | Record of node-to-client mini protocols.
--
data NodeToClientProtocols appType ntcAddr bytes m a b = NodeToClientProtocols {
    -- | local chain-sync mini-protocol
    --
    localChainSyncProtocol    :: RunMiniProtocolWithMinimalCtx
                                   appType ntcAddr bytes m a b,

    -- | local tx-submission mini-protocol
    --
    localTxSubmissionProtocol :: RunMiniProtocolWithMinimalCtx
                                   appType ntcAddr bytes m a b,

    -- | local state-query mini-protocol
    --
    localStateQueryProtocol   :: RunMiniProtocolWithMinimalCtx
                                   appType ntcAddr bytes m a b,

    -- | local tx-monitor mini-protocol
    --
    localTxMonitorProtocol    :: RunMiniProtocolWithMinimalCtx
                                   appType ntcAddr bytes m a b
  }


-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-client protocol.
--
-- This function specifies the wire format protocol numbers as well as the
-- protocols that run for each 'NodeToClientVersion'.
--
-- They are chosen to not overlap with the node to node protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
nodeToClientProtocols
  :: NodeToClientProtocols appType addr bytes m a b
  -> NodeToClientVersion
  -> NodeToClientVersionData
  -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
nodeToClientProtocols protocols _version _versionData =
    OuroborosApplication $
      case protocols of
        NodeToClientProtocols {
            localChainSyncProtocol,
            localTxSubmissionProtocol,
            localStateQueryProtocol,
            localTxMonitorProtocol
          } ->
          [ localChainSyncMiniProtocol localChainSyncProtocol
          , localTxSubmissionMiniProtocol localTxSubmissionProtocol
          , localStateQueryMiniProtocol localStateQueryProtocol
          , localTxMonitorMiniProtocol localTxMonitorProtocol
          ]

  where
    localChainSyncMiniProtocol localChainSyncProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 5,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localChainSyncProtocol
      }
    localTxSubmissionMiniProtocol localTxSubmissionProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 6,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localTxSubmissionProtocol
      }
    localStateQueryMiniProtocol localStateQueryProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 7,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localStateQueryProtocol
      }
    localTxMonitorMiniProtocol localTxMonitorProtocol = MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 9,
        miniProtocolStart  = StartOnDemand,
        miniProtocolLimits = maximumMiniProtocolLimits,
        miniProtocolRun    = localTxMonitorProtocol
    }

maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 0xffffffff
    }


-- | 'Versions' containing a single version of 'nodeToClientProtocols'.
--
versionedNodeToClientProtocols
    :: NodeToClientVersion
    -> NodeToClientVersionData
    -> NodeToClientProtocols appType LocalAddress bytes m a b
    -> Versions NodeToClientVersion
                NodeToClientVersionData
                (OuroborosApplicationWithMinimalCtx appType LocalAddress bytes m a b)
versionedNodeToClientProtocols versionNumber versionData protocols =
    simpleSingletonVersions
      versionNumber
      versionData
      (nodeToClientProtocols protocols versionNumber)

-- | A specialised version of 'Ouroboros.Network.Socket.connectToNode'.  It is
-- a general purpose function which can connect using any version of the
-- protocol.  This is mostly useful for future enhancements.
--
connectTo
  :: LocalSnocket
  -- ^ callback constructed by 'Ouroboros.Network.IOManager.withIOManager'
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> Versions NodeToClientVersion
              NodeToClientVersionData
              (OuroborosApplicationWithMinimalCtx
                 Mx.InitiatorMode LocalAddress BL.ByteString IO a Void)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> FilePath
  -- ^ path of the unix socket or named pipe
  -> IO (Either SomeException a)
connectTo snocket tracers versions path =
    fmap fn <$>
    connectToNode
      snocket
      makeLocalBearer
      ConnectToArgs {
        ctaHandshakeCodec      = nodeToClientHandshakeCodec,
        ctaHandshakeTimeLimits = noTimeLimitsHandshake,
        ctaVersionDataCodec    = cborTermVersionDataCodec nodeToClientCodecCBORTerm,
        ctaConnectTracers      = tracers,
        ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
      }
      mempty
      versions
      Nothing
      (localAddressFromPath path)
  where
    fn :: forall x. Either x Void -> x
    fn = either id absurd

-- | A version of `connectTo` which exposes `Mx.Mux` interfaces which allows to
-- run mini-protocols and handle their termination (e.g. restart them when they
-- terminate or error).
--
connectToWithMux
  :: LocalSnocket
  -- ^ callback constructed by 'Ouroboros.Network.IOManager.withIOManager'
  -> NetworkConnectTracers LocalAddress NodeToClientVersion
  -> Versions NodeToClientVersion
              NodeToClientVersionData
              (OuroborosApplicationWithMinimalCtx
                 Mx.InitiatorMode LocalAddress BL.ByteString IO a b)
  -- ^ A dictionary of protocol versions & applications to run on an established
  -- connection.  The application to run will be chosen by initial handshake
  -- protocol (the highest shared version will be chosen).
  -> FilePath
  -- ^ path of the unix socket or named pipe
  -> (    ConnectionId LocalAddress
       -> NodeToClientVersion
       -> NodeToClientVersionData
       -> OuroborosApplicationWithMinimalCtx Mx.InitiatorMode LocalAddress BL.ByteString IO a b
       -> Mx.Mux Mx.InitiatorMode IO
       -> Async.Async ()
       -> IO x)
  -- ^ callback which has access to negotiated protocols and mux handle created for
  -- that connection.  The `Async` is a handle the the thread which runs
  -- `Mx.runMux`.  The `Mux` handle allows schedule mini-protocols.
  --
  -- NOTE: when the callback returns or errors, the mux thread will be killed.
  -> IO x
connectToWithMux snocket tracers versions path k =
  connectToNodeWithMux
    snocket
    makeLocalBearer
    ConnectToArgs {
      ctaHandshakeCodec      = nodeToClientHandshakeCodec,
      ctaHandshakeTimeLimits = noTimeLimitsHandshake,
      ctaVersionDataCodec    = cborTermVersionDataCodec nodeToClientCodecCBORTerm,
      ctaConnectTracers      = tracers,
      ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
    }
    mempty
    versions
    Nothing
    (localAddressFromPath path)
    k


type LocalConnectionId = ConnectionId LocalAddress

--
-- Null Protocol Peers
--

chainSyncPeerNull
    :: forall (header :: Type) (point :: Type) (tip :: Type) m a. MonadDelay m
    => Client (ChainSync.ChainSync header point tip)
               NonPipelined ChainSync.StIdle m a
chainSyncPeerNull =
    ChainSync.chainSyncClientPeer
      (ChainSync.ChainSyncClient untilTheCowsComeHome )

localStateQueryPeerNull
    :: forall (block :: Type) (point :: Type) (query :: Type -> Type) m a.
       MonadDelay m
    => Stateful.Client (LocalStateQuery.LocalStateQuery block point query)
                       LocalStateQuery.StIdle LocalStateQuery.State m a
localStateQueryPeerNull =
    LocalStateQuery.localStateQueryClientPeer
      (LocalStateQuery.LocalStateQueryClient untilTheCowsComeHome)

localTxSubmissionPeerNull
    :: forall (tx :: Type) (reject :: Type) m a. MonadDelay m
    => Client (LocalTxSubmission.LocalTxSubmission tx reject)
              NonPipelined LocalTxSubmission.StIdle m a
localTxSubmissionPeerNull =
    LocalTxSubmission.localTxSubmissionClientPeer
      (LocalTxSubmission.LocalTxSubmissionClient untilTheCowsComeHome)

localTxMonitorPeerNull
    :: forall (txid :: Type) (tx :: Type) (slot :: Type) m a. MonadDelay m
    => Client (LocalTxMonitor.LocalTxMonitor txid tx slot)
              NonPipelined LocalTxMonitor.StIdle m a
localTxMonitorPeerNull =
    LocalTxMonitor.localTxMonitorClientPeer
      (LocalTxMonitor.LocalTxMonitorClient untilTheCowsComeHome)

-- ;)
untilTheCowsComeHome :: MonadDelay m => m a
untilTheCowsComeHome = forever $ threadDelay 43200 {- day in seconds -}
