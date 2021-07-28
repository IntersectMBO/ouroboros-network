{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
    nodeToNodeProtocols
  , NodeToNodeProtocols (..)
  , MiniProtocolParameters (..)
  , defaultMiniProtocolParameters
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , AcceptedConnectionsLimit (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , withServer

  -- * Subscription Workers
  -- ** IP subscriptin worker
  , IPSubscriptionTarget (..)
  , NetworkIPSubscriptionTracers
  , NetworkSubscriptionTracers (..)
  , nullNetworkSubscriptionTracers
  , SubscriptionParams (..)
  , IPSubscriptionParams
  , ipSubscriptionWorker

  -- ** DNS subscription worker
  , DnsSubscriptionTarget (..)
  , DnsSubscriptionParams
  , NetworkDNSSubscriptionTracers (..)
  , nullNetworkDNSSubscriptionTracers
  , dnsSubscriptionWorker

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
  , ConnectionId (..)
  , RemoteAddress
  , RemoteConnectionId
  , ProtocolLimitFailure
  , Handshake
  , LocalAddresses (..)

  -- ** Error Policies and Peer state
  , ErrorPolicies (..)
  , remoteNetworkErrorPolicy
  , localNetworkErrorPolicy
  , nullErrorPolicies
  , ErrorPolicy (..)
  , SuspendDecision (..)

  -- ** Traces
  , AcceptConnectionsPolicyTrace (..)
  , TraceSendRecv (..)
  , SubscriptionTrace (..)
  , DnsTrace (..)
  , ErrorPolicyTrace (..)
  , WithIPList (..)
  , WithDomainName (..)
  , WithAddr (..)
  , HandshakeTr

  -- * For Consensus ThreadNet Tests
  , chainSyncMiniProtocolNum
  , blockFetchMiniProtocolNum
  , txSubmissionMiniProtocolNum
  , keepAliveMiniProtocolNum
  ) where

import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime (DiffTime)
import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)

import qualified Data.ByteString.Lazy as BL
import           Data.Void (Void)
import           Data.Word
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Network.Mux (WithMuxBearer (..))
import           Network.Mux.Types (MuxRuntimeError (..))
import qualified Network.Socket as Socket

import           Network.TypedProtocol.Codec.CBOR

import           Ouroboros.Network.Driver (TraceSendRecv(..))
import           Ouroboros.Network.Driver.Simple (DecoderFailure)
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToNode.Version
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.BlockFetch.Client (BlockFetchProtocolFailure)
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Tracers
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Version hiding (Accept)
import           Ouroboros.Network.Subscription.Ip (IPSubscriptionParams, SubscriptionParams (..))
import qualified Ouroboros.Network.Subscription.Ip as Subscription
import           Ouroboros.Network.Subscription.Ip ( IPSubscriptionTarget (..)
                                                   , WithIPList (..)
                                                   , SubscriptionTrace (..)
                                                   )
import           Ouroboros.Network.Subscription.Dns (DnsSubscriptionParams)
import qualified Ouroboros.Network.Subscription.Dns as Subscription
import           Ouroboros.Network.Subscription.Dns ( DnsSubscriptionTarget (..)
                                                    , DnsTrace (..)
                                                    , WithDomainName (..)
                                                    )
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..), SubscriberError)
import           Ouroboros.Network.Snocket


-- The Handshake tracer types are simply terrible.
type HandshakeTr = WithMuxBearer (ConnectionId Socket.SockAddr)
    (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term))

-- | 'Hanshake' codec for the @node-to-node@ protocol suite.
--
nodeToNodeHandshakeCodec :: MonadST m
                         => Codec (Handshake NodeToNodeVersion CBOR.Term)
                                  CBOR.DeserialiseFailure m BL.ByteString
nodeToNodeHandshakeCodec = codecHandshake nodeToNodeVersionCodec


data NodeToNodeProtocols appType bytes m a b = NodeToNodeProtocols {
    -- | chain-sync mini-protocol
    --
    chainSyncProtocol    :: RunMiniProtocol appType bytes m a b,

    -- | block-fetch mini-protocol
    --
    blockFetchProtocol   :: RunMiniProtocol appType bytes m a b,

    -- | tx-submission mini-protocol
    --
    txSubmissionProtocol :: RunMiniProtocol appType bytes m a b,

    -- | keep-alive mini-protocol
    --
    keepAliveProtocol :: RunMiniProtocol appType bytes m a b

  }


data MiniProtocolParameters = MiniProtocolParameters {
      chainSyncPipeliningHighMark :: !Word32,
      -- ^ high threshold for pipelining (we will never exceed that many
      -- messages pipelined).

      chainSyncPipeliningLowMark  :: !Word32,
      -- ^ low threshold: if we hit the 'chainSyncPipeliningHighMark' we will
      -- listen for responses until there are at most
      -- 'chainSyncPipeliningLowMark' pipelined message
      --
      -- Must be smaller than 'chainSyncPipeliningHighMark'.
      --
      -- Note: 'chainSyncPipeliningLowMark' and 'chainSyncPipeliningLowMark'
      -- are passed to 'pipelineDecisionLowHighMark'.

      blockFetchPipeliningMax     :: !Word,
      -- ^ maximal number of pipelined messages in 'block-fetch' mini-protocol.

      txSubmissionMaxUnacked       :: !Word16
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
-- messages.  Only when the handshake protocol suceedes, we will know which
-- protocols to run / multiplex.
--
-- These are chosen to not overlap with the node to client protocol numbers (and
-- the handshake protocol number).  This is not essential for correctness, but
-- is helpful to allow a single shared implementation of tools that can analyse
-- both protocols, e.g.  wireshark plugins.
--
nodeToNodeProtocols
  :: MiniProtocolParameters
  -> (ConnectionId addr -> STM m ControlMessage -> NodeToNodeProtocols appType bytes m a b)
  -> NodeToNodeVersion
  -> OuroborosApplication appType addr bytes m a b
nodeToNodeProtocols MiniProtocolParameters {
                        chainSyncPipeliningHighMark,
                        blockFetchPipeliningMax,
                        txSubmissionMaxUnacked
                      }
                    protocols _version =
  OuroborosApplication $ \connectionId controlMessageSTM ->
    case protocols connectionId controlMessageSTM of
      NodeToNodeProtocols {
          chainSyncProtocol,
          blockFetchProtocol,
          txSubmissionProtocol,
          keepAliveProtocol
        } ->
        [ chainSyncMiniProtocol chainSyncProtocol
        , blockFetchMiniProtocol blockFetchProtocol
        , txSubmissionMiniProtocol txSubmissionProtocol
        , keepAliveMiniProtocol keepAliveProtocol
        ]
   where
    chainSyncMiniProtocol chainSyncProtocol = MiniProtocol {
        miniProtocolNum    = chainSyncMiniProtocolNum
      , miniProtocolLimits = chainSyncProtocolLimits
      , miniProtocolRun    = chainSyncProtocol
      }
    blockFetchMiniProtocol blockFetchProtocol = MiniProtocol {
        miniProtocolNum    = blockFetchMiniProtocolNum
      , miniProtocolLimits = blockFetchProtocolLimits
      , miniProtocolRun    = blockFetchProtocol
      }
    txSubmissionMiniProtocol txSubmissionProtocol = MiniProtocol {
        miniProtocolNum    = txSubmissionMiniProtocolNum
      , miniProtocolLimits = txSubmissionProtocolLimits
      , miniProtocolRun    = txSubmissionProtocol
      }
    keepAliveMiniProtocol keepAliveProtocol = MiniProtocol {
        miniProtocolNum    = keepAliveMiniProtocolNum
      , miniProtocolLimits = keepAliveProtocolLimits
      , miniProtocolRun    = keepAliveProtocol
      }

    addSafetyMargin :: Int -> Int
    addSafetyMargin x = x + x `div` 10

    chainSyncProtocolLimits
      , blockFetchProtocolLimits
      , txSubmissionProtocolLimits
      , keepAliveProtocolLimits :: MiniProtocolLimits

    chainSyncProtocolLimits =
      MiniProtocolLimits {
          -- The largest message over ChainSync is @MsgRollForward@ which mainly
          -- consists of a BlockHeader.
          -- TODO: 1400 comes from maxBlockHeaderSize in genesis, but should come
          -- from consensus rather than beeing hardcoded.
          maximumIngressQueue = addSafetyMargin $
            fromIntegral chainSyncPipeliningHighMark * 1400
        }

    blockFetchProtocolLimits = MiniProtocolLimits {
        -- block-fetch client can pipeline at most 'blockFetchPipeliningMax'
        -- blocks (currently '10').  This is currently hard coded in
        -- 'Ouroboros.Network.BlockFetch.blockFetchLogic' (where
        -- @maxInFlightReqsPerPeer = 100@ is specified).  In the future the
        -- block fetch client will count bytes rather than blocks.  By far
        -- the largest (and the only pipelined message) in 'block-fetch'
        -- protocol is 'MsgBlock'.  Current block size limit is 64KB and
        -- `blockFetchPipeliningMax` below is set to `100`.  This means that
        -- overall queue limit must be:
        --
        -- ```
        -- 100 * 64KB = 6.4MB
        -- ```
        --
        -- In the byron era this limit was set to `10 * 2MB`, we keep the more
        -- relaxed limit here.
        --
        maximumIngressQueue = addSafetyMargin $ fromIntegral $
          max (10 * 2_097_154) (blockFetchPipeliningMax * 65535)
      }

    txSubmissionProtocolLimits = MiniProtocolLimits {
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
          --  + 65_536 -- 64kb transaction
          --  + 1      -- encodeBreak
          --  = 65_540
          -- ```
          --
          -- On the ingress side of 'txSubmissionOutbound' we can have at most
          -- `MaxUnacked' 'MsgRequestTxsIds' and the same ammount of
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

    keepAliveProtocolLimits =
      MiniProtocolLimits {
          -- One small outstanding message.
          maximumIngressQueue = addSafetyMargin 1280
        }

chainSyncMiniProtocolNum :: MiniProtocolNum
chainSyncMiniProtocolNum = MiniProtocolNum 2

blockFetchMiniProtocolNum :: MiniProtocolNum
blockFetchMiniProtocolNum = MiniProtocolNum 3

txSubmissionMiniProtocolNum :: MiniProtocolNum
txSubmissionMiniProtocolNum = MiniProtocolNum 4

keepAliveMiniProtocolNum :: MiniProtocolNum
keepAliveMiniProtocolNum = MiniProtocolNum 8

-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: Snocket IO Socket.Socket Socket.SockAddr
  -> NetworkConnectTracers Socket.SockAddr NodeToNodeVersion
  -> Versions NodeToNodeVersion
              NodeToNodeVersionData
              (OuroborosApplication InitiatorMode Socket.SockAddr BL.ByteString IO a b)
  -> Maybe Socket.SockAddr
  -> Socket.SockAddr
  -> IO ()
connectTo sn tr =
    connectToNode sn nodeToNodeHandshakeCodec timeLimitsHandshake
                  (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
                  tr acceptableVersion


-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@.
-- It forks a thread which runs an accept loop (server thread):
--
-- * when the server thread throws an exception the main thread rethrows
--   it (by 'Async.wait')
-- * when an async exception is thrown to kill the main thread the server thread
--   will be cancelled as well (by 'withAsync')
--
withServer
  :: SocketSnocket
  -> NetworkServerTracers Socket.SockAddr NodeToNodeVersion
  -> NetworkMutableState Socket.SockAddr
  -> AcceptedConnectionsLimit
  -> Socket.Socket
  -> Versions NodeToNodeVersion
              NodeToNodeVersionData
              (OuroborosApplication ResponderMode Socket.SockAddr BL.ByteString IO a b)
  -> ErrorPolicies
  -> IO Void
withServer sn tracers networkState acceptedConnectionsLimit sd versions errPolicies =
  withServerNode'
    sn
    tracers
    networkState
    acceptedConnectionsLimit
    sd
    nodeToNodeHandshakeCodec
    timeLimitsHandshake
    (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
    acceptableVersion
    (SomeResponderApplication <$> versions)
    errPolicies
    (\_ async -> Async.wait async)


-- | 'ipSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ipSubscriptionWorker
    :: forall mode x y.
       ( HasInitiator mode ~ True )
    => SocketSnocket
    -> NetworkIPSubscriptionTracers Socket.SockAddr NodeToNodeVersion
    -> NetworkMutableState Socket.SockAddr
    -> IPSubscriptionParams ()
    -> Versions
        NodeToNodeVersion
        NodeToNodeVersionData
        (OuroborosApplication mode Socket.SockAddr BL.ByteString IO x y)
    -> IO Void
ipSubscriptionWorker
  sn
  NetworkSubscriptionTracers
    { nsSubscriptionTracer
    , nsMuxTracer
    , nsHandshakeTracer
    , nsErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions
    = Subscription.ipSubscriptionWorker
        sn
        nsSubscriptionTracer
        nsErrorPolicyTracer
        networkState
        subscriptionParams
        (connectToNode'
          sn
          nodeToNodeHandshakeCodec
          timeLimitsHandshake
          (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
          (NetworkConnectTracers nsMuxTracer nsHandshakeTracer)
          acceptableVersion
          versions)


-- | 'dnsSubscriptionWorker' which starts given application versions on each
-- established connection.
--
dnsSubscriptionWorker
    :: forall mode x y.
       ( HasInitiator mode ~ True )
    => SocketSnocket
    -> NetworkDNSSubscriptionTracers NodeToNodeVersion Socket.SockAddr
    -> NetworkMutableState Socket.SockAddr
    -> DnsSubscriptionParams ()
    -> Versions
        NodeToNodeVersion
        NodeToNodeVersionData
        (OuroborosApplication mode Socket.SockAddr BL.ByteString IO x y)
    -> IO Void
dnsSubscriptionWorker
  sn
  NetworkDNSSubscriptionTracers
    { ndstSubscriptionTracer
    , ndstDnsTracer
    , ndstMuxTracer
    , ndstHandshakeTracer
    , ndstErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions =
    Subscription.dnsSubscriptionWorker
      sn
      ndstSubscriptionTracer
      ndstDnsTracer
      ndstErrorPolicyTracer
      networkState
      subscriptionParams
      (connectToNode'
        sn
        nodeToNodeHandshakeCodec
        timeLimitsHandshake
        (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
        (NetworkConnectTracers ndstMuxTracer ndstHandshakeTracer)
        acceptableVersion
        versions)


-- | A minimal error policy for remote peers, which only handles exceptions
-- raised by `ouroboros-network`.
--
remoteNetworkErrorPolicy :: ErrorPolicies
remoteNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- Handshake client protocol error: we either did not recognise received
          -- version or we refused it.  This is only for outbound connections,
          -- thus we suspend the consumer.
          ErrorPolicy
            $ \(_ :: HandshakeProtocolError NodeToNodeVersion)
                  -> Just misconfiguredPeer

          -- deserialisation failure; this means that the remote peer is either
          -- buggy, adversarial, or the connection return garbage.  In the last
          -- case it's also good to shutdown both the consumer and the
          -- producer, as it's likely that the other side of the connection
          -- will return grabage as well.
        , ErrorPolicy
           $ \(_ :: DecoderFailure)
                 -> Just theyBuggyOrEvil

        , ErrorPolicy
          $ \(msg :: ProtocolLimitFailure)
                  -> case msg of
                      ExceededSizeLimit{} -> Just theyBuggyOrEvil
                      ExceededTimeLimit{} -> Just (SuspendConsumer shortDelay)

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
            $ \(e :: MuxError)
                  -> case errorType e of
                        MuxUnknownMiniProtocol  -> Just theyBuggyOrEvil
                        MuxDecodeError          -> Just theyBuggyOrEvil
                        MuxIngressQueueOverRun  -> Just theyBuggyOrEvil
                        MuxInitiatorOnly        -> Just theyBuggyOrEvil

                        -- in case of bearer closed / or IOException we suspend
                        -- the peer for a short time
                        --
                        -- TODO: an exponential backoff would be nicer than a fixed 20s
                        -- TODO: right now we cannot suspend just the
                        -- 'responder'.  If a 'responder' throws 'MuxError' we
                        -- might not want to shutdown the consumer (which is
                        -- using different connection), as we do below:
                        MuxBearerClosed              -> Just (SuspendPeer shortDelay shortDelay)
                        MuxIOException{}             -> Just (SuspendPeer shortDelay shortDelay)
                        MuxSDUReadTimeout            -> Just (SuspendPeer shortDelay shortDelay)
                        MuxSDUWriteTimeout           -> Just (SuspendPeer shortDelay shortDelay)
                        MuxShutdown {}               -> Just (SuspendPeer shortDelay shortDelay)
                        MuxCleanShutdown             -> Just (SuspendPeer shortDelay shortDelay)

        , ErrorPolicy
            $ \(e :: MuxRuntimeError)
                  -> case e of
                       ProtocolAlreadyRunning       {} -> Just (SuspendPeer shortDelay shortDelay)
                       UnknownProtocolInternalError {} -> Just Throw
                       MuxBlockedOnCompletionVar    {} -> Just (SuspendPeer shortDelay shortDelay)

          -- Error policy for TxSubmission protocol: outbound side (client role)
        , ErrorPolicy
            $ \(_ :: TxOutbound.TxSubmissionProtocolError)
                  -> Just theyBuggyOrEvil

          -- Error policy for TxSubmission protocol: inbound side (server role)
        , ErrorPolicy
            $ \(_ :: TxInbound.TxSubmissionProtocolError)
                  -> Just theyBuggyOrEvil

          -- Error policy for BlockFetch protocol: consumer side (client role)
        , ErrorPolicy
            $ \(_ :: BlockFetchProtocolFailure)
                  -> Just theyBuggyOrEvil

          -- Error thrown by 'IOManager', this is fatal on Windows, and it will
          -- never fire on other platofrms.
        , ErrorPolicy
            $ \(_ :: IOManagerError)
                  -> Just Throw
        ],

      -- Exception raised during connect; suspend connecting to that peer for
      -- a 'shortDelay'
      epConErrorPolicies = [
          ErrorPolicy $ \(_ :: IOException) -> Just $
            SuspendConsumer shortDelay

        , ErrorPolicy
            $ \(_ :: IOManagerError)
                  -> Just Throw
        , ErrorPolicy
          -- Multiple connection attempts are run in parallel and the last to
          -- finish are cancelled. There may be nothing wrong with the peer,
          -- it could just be slow to respond.
          $ \(_ :: SubscriberError)
                -> Just (SuspendConsumer veryShortDelay)
        ]
    }
  where
    theyBuggyOrEvil :: SuspendDecision DiffTime
    theyBuggyOrEvil = SuspendPeer defaultDelay defaultDelay

    misconfiguredPeer :: SuspendDecision DiffTime
    misconfiguredPeer = SuspendConsumer defaultDelay

    defaultDelay :: DiffTime
    defaultDelay = 200 -- seconds

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

    veryShortDelay :: DiffTime
    veryShortDelay = 1 -- seconds

-- | Error policy for local clients.  This is equivalent to
-- 'nullErrorPolicies', but explicit in the errors which can be catched.
--
-- We are very permissive here, and very strict in the
-- `NodeToClient.networkErrorPolicy`.  After any failure the client will be
-- killed and not penalised by this policy.  This allows to restart the local
-- client without a delay.
--
localNetworkErrorPolicy :: ErrorPolicies
localNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- exception thrown by `runPeerWithLimits`
          ErrorPolicy
            $ \(_ :: ProtocolLimitFailure)
                  -> Nothing

          -- deserialisation failure
        , ErrorPolicy
            $ \(_ :: CBOR.DeserialiseFailure) -> Nothing

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
          $ \(_ :: MuxError) -> Nothing
        ],

      -- The node never connects to a local client
      epConErrorPolicies = []
    }

type RemoteAddress      = Socket.SockAddr
type RemoteConnectionId = ConnectionId RemoteAddress
