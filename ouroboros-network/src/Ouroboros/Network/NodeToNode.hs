{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode
  ( nodeToNodeProtocols
  , NodeToNodeProtocols (..)
  , NodeToNodeProtocolsWithExpandedCtx
  , NodeToNodeProtocolsWithMinimalCtx
  , MiniProtocolParameters (..)
  , chainSyncProtocolLimits
  , blockFetchProtocolLimits
  , txSubmissionProtocolLimits
  , keepAliveProtocolLimits
  , peerSharingProtocolLimits
  , defaultMiniProtocolParameters
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo
  , AcceptedConnectionsLimit (..)
  , ntnDataFlow
  , addSafetyMargin
    -- * P2P Governor
  , PeerAdvertise (..)
  , PeerSelectionTargets (..)
    -- * Subscription Workers
    -- ** Versions
  , Versions (..)
  , DiffusionMode (..)
  , simpleSingletonVersions
  , foldMapVersions
  , combineVersions
    -- *** Codecs
  , nodeToNodeHandshakeCodec
  , nodeToNodeVersionCodec
  , nodeToNodeCodecCBORTerm
    -- * Re-exports
  , ExpandedInitiatorContext (..)
  , MinimalInitiatorContext (..)
  , ResponderContext (..)
  , ConnectionId (..)
  , ControlMessage (..)
  , ControlMessageSTM
  , RemoteAddress
  , RemoteConnectionId
  , IsBigLedgerPeer (..)
  , NumTxIdsToAck (..)
  , ProtocolLimitFailure
  , Handshake
  , Socket
    -- ** Exceptions
  , ExceptionInHandler (..)
    -- ** Traces
  , AcceptConnectionsPolicyTrace (..)
  , TraceSendRecv (..)
  , HandshakeTr
    -- * For Consensus ThreadNet Tests
  , chainSyncMiniProtocolNum
  , blockFetchMiniProtocolNum
  , txSubmissionMiniProtocolNum
  , keepAliveMiniProtocolNum
  , peerSharingMiniProtocolNum
  ) where

import Control.Exception (SomeException)

import Codec.CBOR.Term qualified as CBOR
import Data.ByteString.Lazy qualified as BL
import Data.Word
import Network.Mux qualified as Mx
import Network.Socket (Socket, StructLinger (..))
import Network.Socket qualified as Socket

import Ouroboros.Network.ConnectionManager.Types (DataFlow (..),
           ExceptionInHandler (..))
import Ouroboros.Network.Context
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver (TraceSendRecv (..))
import Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToNode.Version
import Ouroboros.Network.PeerSelection.Governor.Types
           (PeerSelectionTargets (..))
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Type
import Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
import Ouroboros.Network.Protocol.TxSubmission2.Type (NumTxIdsToAck (..))
import Ouroboros.Network.Server.RateLimiting
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket
import Ouroboros.Network.Util.ShowProxy (ShowProxy, showProxy)


-- The Handshake tracer types are simply terrible.
type HandshakeTr ntnAddr ntnVersion =
    Mx.WithBearer (ConnectionId ntnAddr)
                  (TraceSendRecv (Handshake ntnVersion CBOR.Term))


data NodeToNodeProtocols appType initiatorCtx responderCtx bytes m a b = NodeToNodeProtocols {
    -- | chain-sync mini-protocol
    --
    chainSyncProtocol    :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b,

    -- | block-fetch mini-protocol
    --
    blockFetchProtocol   :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b,

    -- | tx-submission mini-protocol
    --
    txSubmissionProtocol :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b,

    -- | keep-alive mini-protocol
    --
    keepAliveProtocol    :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b,

    -- | peer sharing mini-protocol
    --
    peerSharingProtocol  :: RunMiniProtocol appType initiatorCtx responderCtx bytes m a b

  }

type NodeToNodeProtocolsWithExpandedCtx appType ntnAddr bytes m a b =
    NodeToNodeProtocols appType (ExpandedInitiatorContext ntnAddr m) (ResponderContext ntnAddr) bytes m a b
type NodeToNodeProtocolsWithMinimalCtx  appType ntnAddr bytes m a b =
    NodeToNodeProtocols appType (MinimalInitiatorContext ntnAddr)  (ResponderContext ntnAddr) bytes m a b


data MiniProtocolParameters = MiniProtocolParameters {
      chainSyncPipeliningHighMark :: !Word16,
      -- ^ high threshold for pipelining (we will never exceed that many
      -- messages pipelined).

      chainSyncPipeliningLowMark  :: !Word16,
      -- ^ low threshold: if we hit the 'chainSyncPipeliningHighMark' we will
      -- listen for responses until there are at most
      -- 'chainSyncPipeliningLowMark' pipelined message
      --
      -- Must be smaller than 'chainSyncPipeliningHighMark'.
      --
      -- Note: 'chainSyncPipeliningLowMark' and 'chainSyncPipeliningLowMark'
      -- are passed to 'pipelineDecisionLowHighMark'.

      blockFetchPipeliningMax     :: !Word16,
      -- ^ maximal number of pipelined messages in 'block-fetch' mini-protocol.

      txSubmissionMaxUnacked      :: !NumTxIdsToAck
      -- ^ maximal number of unacked tx (pipelining is bounded by twice this
      -- number)
    }

defaultMiniProtocolParameters :: MiniProtocolParameters
defaultMiniProtocolParameters = MiniProtocolParameters {
      chainSyncPipeliningLowMark  = 200
    , chainSyncPipeliningHighMark = 300
    , blockFetchPipeliningMax     = 100
    , txSubmissionMaxUnacked       = 10
  }

-- | Make an 'OuroborosApplication' for the bundle of mini-protocols that
-- make up the overall node-to-node protocol.
--
-- This function specifies the wire format protocol numbers.
--
-- The application specific protocol numbers start from 2.  The
-- @'MiniProtocolNum' 0@ is reserved for the 'Handshake' protocol, while
-- @'MiniProtocolNum' 1@ is reserved for DeltaQ messages.
-- 'Handshake' protocol is not included in 'NodeToNodeProtocols' as it runs
-- before mux is started but it reusing 'MuxBearer' to send and receive
-- messages.  Only when the handshake protocol succeeds, we will know which
-- protocols to run / multiplex.
--
-- These are chosen to not overlap with the node to client protocol numbers (and
-- the handshake protocol number).  This is not essential for correctness, but
-- is helpful to allow a single shared implementation of tools that can analyse
-- both protocols, e.g.  wireshark plugins.
--
nodeToNodeProtocols
  :: MiniProtocolParameters
  -> NodeToNodeProtocols muxMode initiatorCtx responderCtx bytes m a b
  -> NodeToNodeVersion
  -- ^ negotiated version number
  -> NodeToNodeVersionData
  -- ^ negotiated version data
  -> OuroborosBundle muxMode initiatorCtx responderCtx bytes m a b
nodeToNodeProtocols miniProtocolParameters protocols
                    _version NodeToNodeVersionData { peerSharing }
                    =
    TemperatureBundle
      -- Hot protocols: 'chain-sync', 'block-fetch' and 'tx-submission'.
      (WithHot $
        case protocols of
          NodeToNodeProtocols { chainSyncProtocol,
                                blockFetchProtocol,
                                txSubmissionProtocol
                              } ->
            [ MiniProtocol {
                miniProtocolNum    = chainSyncMiniProtocolNum,
                miniProtocolStart  = StartOnDemand,
                miniProtocolLimits = chainSyncProtocolLimits miniProtocolParameters,
                miniProtocolRun    = chainSyncProtocol
              }
            , MiniProtocol {
                miniProtocolNum    = blockFetchMiniProtocolNum,
                miniProtocolStart  = StartOnDemand,
                miniProtocolLimits = blockFetchProtocolLimits miniProtocolParameters,
                miniProtocolRun    = blockFetchProtocol
              }
            , MiniProtocol {
                miniProtocolNum    = txSubmissionMiniProtocolNum,
                miniProtocolStart  = StartOnDemand,
                miniProtocolLimits = txSubmissionProtocolLimits miniProtocolParameters,
                miniProtocolRun    = txSubmissionProtocol
              }
            ])

      -- Warm protocols: reserved for 'tip-sample'.
      (WithWarm [])

      -- Established protocols: 'keep-alive'.
      (WithEstablished $
        case protocols of
          NodeToNodeProtocols { keepAliveProtocol,
                                peerSharingProtocol } ->
              MiniProtocol {
                miniProtocolNum    = keepAliveMiniProtocolNum,
                miniProtocolStart  = StartOnDemandAny,
                miniProtocolLimits = keepAliveProtocolLimits miniProtocolParameters,
                miniProtocolRun    = keepAliveProtocol
              }
            : case peerSharing of
                PeerSharingEnabled ->
                  [ MiniProtocol {
                      miniProtocolNum    = peerSharingMiniProtocolNum,
                      miniProtocolStart  = StartOnDemand,
                      miniProtocolLimits = peerSharingProtocolLimits miniProtocolParameters,
                      miniProtocolRun    = peerSharingProtocol
                    }
                  ]
                PeerSharingDisabled ->
                  []
      )

addSafetyMargin :: Int -> Int
addSafetyMargin x = x + x `div` 10

chainSyncProtocolLimits
  , blockFetchProtocolLimits
  , txSubmissionProtocolLimits
  , keepAliveProtocolLimits
  , peerSharingProtocolLimits :: MiniProtocolParameters -> MiniProtocolLimits

chainSyncProtocolLimits MiniProtocolParameters { chainSyncPipeliningHighMark } =
  MiniProtocolLimits {
      -- The largest message over ChainSync is @MsgRollForward@ which mainly
      -- consists of a BlockHeader.
      -- TODO: 1400 comes from maxBlockHeaderSize in genesis, but should come
      -- from consensus rather than being hard coded.
      maximumIngressQueue = addSafetyMargin $
        fromIntegral chainSyncPipeliningHighMark * 1400
    }

blockFetchProtocolLimits MiniProtocolParameters { blockFetchPipeliningMax } = MiniProtocolLimits {
    -- block-fetch client can pipeline at most 'blockFetchPipeliningMax'
    -- blocks (currently '10').  This is currently hard coded in
    -- 'Ouroboros.Network.BlockFetch.blockFetchLogic' (where
    -- @maxInFlightReqsPerPeer = 100@ is specified).  In the future the
    -- block fetch client will count bytes rather than blocks.  By far
    -- the largest (and the only pipelined message) in 'block-fetch'
    -- protocol is 'MsgBlock'.  Current block size limit is 88kiB and
    -- `blockFetchPipeliningMax` below is set to `100`.  This means that
    -- overall queue limit must be:
    --
    --   ```
        -- 100 * 88kiB = 8.8MiB
    --   ```
    --
    -- In the byron era this limit was set to `10 * 2MiB`, we keep the more
    -- relaxed limit here.
    --
    maximumIngressQueue = addSafetyMargin $
      max (10 * 2_097_154 :: Int) (fromIntegral blockFetchPipeliningMax * 90_112)
  }

txSubmissionProtocolLimits MiniProtocolParameters { txSubmissionMaxUnacked } = MiniProtocolLimits {
      -- tx-submission server can pipeline both 'MsgRequestTxIds' and
      -- 'MsgRequestTx'. This means that there can be many
      -- 'MsgReplyTxIds', 'MsgReplyTxs' messages in an inbound queue (their
      -- sizes are strictly greater than the corresponding request
      -- messages).
      --
      -- Each 'MsgRequestTx' can contain at max @maxTxIdsToRequest = 3@
      -- (defined in -- 'Ouroboros.Network.TxSubmission.Inbound.txSubmissionInbound')
      --
      -- Each 'MsgRequestTx' can request at max @maxTxToRequest = 2@
      -- (defined in -- 'Ouroboros.Network.TxSubmission.Inbound.txSubmissionInbound')
      --
      -- The 'txSubmissionInBound' server can at most put `100`
      -- unacknowledged transactions.  It also pipelines both 'MsgRequestTx`
      -- and `MsgRequestTx` in turn. This means that the inbound queue can
      -- have at most `100` `MsgRequestTxIds` and `MsgRequestTx` which will
      -- contain a single `TxId` / `Tx`.
      --
      -- TODO: the unacknowledged transactions are configured in `NodeArgs`,
      -- and we should take this parameter as an input for this computation.
      --
      -- The upper bound of size of a single transaction is 64k, while the
      -- size of `TxId` is `34` bytes (`type TxId = Hash Tx`).
      --
      -- Ingress side of `txSubmissinInbound`
      --
      -- - 'MsgReplyTxs' carrying a single `TxId`:
      -- ```
      --    1  -- encodeListLen 2
      --  + 1  -- encodeWord 1
      --  + 1  -- encodeListLenIndef
      --  + 1  -- encodeListLen 2
      --  + 34 -- encode 'TxId'
      --  + 5  -- encodeWord32 (size of tx)
      --  + 1  -- encodeBreak
      --  = 44
      -- ```
      -- - 'MsgReplyTx' carrying a single 'Tx':
      -- ```
      --    1      -- encodeListLen 2
      --  + 1      -- encodeWord 3
      --  + 1      -- encodeListLenIndef
      --  + 65_536 -- 64kiB transaction
      --  + 1      -- encodeBreak
      --  = 65_540
      -- ```
      --
      -- On the ingress side of 'txSubmissionOutbound' we can have at most
      -- `MaxUnacked' 'MsgRequestTxsIds' and the same amount of
      -- 'MsgRequsetTx' containing a single 'TxId'.  The size of
      -- 'MsgRequestTxsIds' is much smaller that 'MsgReplyTx', and the size
      -- of `MsgReqeustTx` with a single 'TxId' is smaller than
      -- 'MsgReplyTxIds' which contains a single 'TxId' (it just contains
      -- the 'TxId' without the size of 'Tx' in bytes).  So the ingress
      -- queue of 'txSubmissionOutbound' is bounded by the ingress side of
      -- the 'txSubmissionInbound'
      --
      -- Currently the value of 'txSubmissionMaxUnacked' is '100', for
      -- which the upper bound is `100 * (44 + 65_540) = 6_558_400`, we add
      -- 10% as a safety margin.
      --
      maximumIngressQueue = addSafetyMargin $
          fromIntegral txSubmissionMaxUnacked * (44 + 65_540)
    }

keepAliveProtocolLimits _ =
  MiniProtocolLimits {
      -- One small outstanding message.
      maximumIngressQueue = addSafetyMargin 1280
    }

peerSharingProtocolLimits _ =
  MiniProtocolLimits {
  -- This protocol does not need to be pipelined and a peer can only ask
  -- for a maximum of 255 peers each time. Hence a reply can have up to
  -- 255 IP (IPv4 or IPv6) addresses so 255 * 16 = 4080. TCP has an initial
  -- window size of 4 and a TCP segment is 1440, which gives us 4 * 1440 =
  -- 5760 bytes to fit into a single RTT. So setting the maximum ingress
  -- queue to be a single RTT should be enough to cover for CBOR overhead.
  maximumIngressQueue = 4 * 1440
  }

chainSyncMiniProtocolNum :: MiniProtocolNum
chainSyncMiniProtocolNum = MiniProtocolNum 2

blockFetchMiniProtocolNum :: MiniProtocolNum
blockFetchMiniProtocolNum = MiniProtocolNum 3

txSubmissionMiniProtocolNum :: MiniProtocolNum
txSubmissionMiniProtocolNum = MiniProtocolNum 4

keepAliveMiniProtocolNum :: MiniProtocolNum
keepAliveMiniProtocolNum = MiniProtocolNum 8

peerSharingMiniProtocolNum :: MiniProtocolNum
peerSharingMiniProtocolNum = MiniProtocolNum 10

-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: Snocket IO Socket.Socket Socket.SockAddr
  -> NetworkConnectTracers Socket.SockAddr NodeToNodeVersion
  -> Versions NodeToNodeVersion
              NodeToNodeVersionData
              (OuroborosApplicationWithMinimalCtx
                 Mx.InitiatorMode Socket.SockAddr BL.ByteString IO a b)
  -> Maybe Socket.SockAddr
  -> Socket.SockAddr
  -> IO (Either SomeException (Either a b))
connectTo sn tr =
    connectToNode sn makeSocketBearer
                  ConnectToArgs {
                    ctaHandshakeCodec      = nodeToNodeHandshakeCodec,
                    ctaHandshakeTimeLimits = timeLimitsHandshake,
                    ctaVersionDataCodec    = cborTermVersionDataCodec nodeToNodeCodecCBORTerm,
                    ctaConnectTracers      = tr,
                    ctaHandshakeCallbacks  = HandshakeCallbacks acceptableVersion queryVersion
                  }
                  configureOutboundSocket
  where
    configureOutboundSocket :: Socket -> IO ()
    configureOutboundSocket sock = do
        Socket.setSocketOption sock Socket.NoDelay 1
        Socket.setSockOpt sock Socket.Linger
                              (StructLinger { sl_onoff  = 1,
                                              sl_linger = 0 })

-- | Node-To-Node protocol connections which negotiated
-- `InitiatorAndResponderDiffusionMode` are `Duplex`.
--
ntnDataFlow :: NodeToNodeVersionData -> DataFlow
ntnDataFlow NodeToNodeVersionData { diffusionMode } =
  case diffusionMode of
    InitiatorAndResponderDiffusionMode -> Duplex
    InitiatorOnlyDiffusionMode         -> Unidirectional

type RemoteAddress      = Socket.SockAddr

instance ShowProxy RemoteAddress where
  showProxy _ = "SockAddr"

type RemoteConnectionId = ConnectionId RemoteAddress
