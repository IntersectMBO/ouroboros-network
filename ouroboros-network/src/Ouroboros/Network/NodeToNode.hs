{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This is the starting point for a module that will bring together the
-- overall node to node protocol, as a collection of mini-protocols.
--
module Ouroboros.Network.NodeToNode (
    NodeToNodeProtocols(..)
  , NodeToNodeVersion (..)
  , NodeToNodeVersionData (..)
  , DictVersion (..)
  , nodeToNodeCodecCBORTerm

  , NetworkConnectTracers (..)
  , nullNetworkConnectTracers
  , connectTo
  , connectTo_V1

  , NetworkServerTracers (..)
  , nullNetworkServerTracers
  , NetworkMutableState (..)
  , newNetworkMutableState
  , newNetworkMutableStateSTM
  , cleanNetworkMutableState
  , withServer
  , withServer_V1

  -- * Subscription Workers
  -- ** IP subscriptin worker
  , IPSubscriptionTarget (..)
  , NetworkIPSubscriptionTracers (..)
  , nullNetworkIPSubscriptionTracers
  , SubscriptionParams (..)
  , IPSubscriptionParams
  , ipSubscriptionWorker
  , ipSubscriptionWorker_V1

  -- ** DNS subscription worker
  , DnsSubscriptionTarget (..)
  , DnsSubscriptionParams
  , NetworkDNSSubscriptionTracers (..)
  , nullNetworkDNSSubscriptionTracers
  , dnsSubscriptionWorker
  , dnsSubscriptionWorker_V1

  -- * Re-exports
  , ConnectionId (..)
  , DecoderFailureOrTooMuchInput
  , Handshake
  , LocalAddresses (..)

  -- ** Protocol start and restart logic
  , simpleResponderControl
  , simpleInitiatorControl

  -- ** Error Policies and Peer state
  , ErrorPolicies (..)
  , remoteNetworkErrorPolicy
  , localNetworkErrorPolicy
  , nullErrorPolicies
  , ErrorPolicy (..)
  , SuspendDecision (..)

  -- ** Traces
  , TraceSendRecv (..)
  , SubscriptionTrace (..)
  , DnsTrace (..)
  , ErrorPolicyTrace (..)
  , WithIPList (..)
  , WithDomainName (..)
  , WithAddr (..)
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import           Control.Monad (void, forever)
import qualified Data.ByteString.Lazy as BL
import           Data.Time.Clock (DiffTime)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Codec.Serialise (Serialise (..), DeserialiseFailure)
import           Codec.SerialiseTerm
import qualified Network.Socket as Socket

import           Control.Monad.Class.MonadAsync (async, MonadAsync)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTimer

import           Network.Mux.Types
import           Network.Mux.Interface
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput)
import           Network.TypedProtocol.Driver (TraceSendRecv (..))

import           Ouroboros.Network.Magic
import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.BlockFetch.Client (BlockFetchProtocolFailure)
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound
import           Ouroboros.Network.Socket
import           Ouroboros.Network.Tracers
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
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))


-- | An index type used with the mux to enumerate all the mini-protocols that
-- make up the overall node-to-node protocol.
--
data NodeToNodeProtocols = ChainSyncWithHeadersPtcl
                         | BlockFetchPtcl
                         | TxSubmissionPtcl
  deriving (Eq, Ord, Enum, Bounded, Show)

-- These protocol numbers end up in the wire format so it is vital that they
-- are stable, even as they are upgraded. So we use custom Enum instances here.
-- This allows us to retire old versions and add new, which may leave some
-- holes in the numbering space.

-- | These are the actual wire format protocol numbers.
--
-- The application specific protocol numbers start from 2 because of the two
-- mux built-in protocols.
--
-- These are chosen to not overlap with the node to client protocol numbers.
-- This is not essential for correctness, but is helpful to allow a single
-- shared implementation of tools that can analyse both protocols, e.g.
-- wireshark plugins.
--
instance ProtocolEnum NodeToNodeProtocols where

  fromProtocolEnum ChainSyncWithHeadersPtcl = 2
  fromProtocolEnum BlockFetchPtcl           = 3
  fromProtocolEnum TxSubmissionPtcl         = 4

  toProtocolEnum 2 = Just ChainSyncWithHeadersPtcl
  toProtocolEnum 3 = Just BlockFetchPtcl
  toProtocolEnum 4 = Just TxSubmissionPtcl
  toProtocolEnum _ = Nothing

instance MiniProtocolLimits NodeToNodeProtocols where
  -- TODO: provide sensible limits
  -- https://github.com/input-output-hk/ouroboros-network/issues/575
  maximumMessageSize  _ = 0xffffffff
  maximumIngressQueue _ = 0xffffffff

-- | Enumeration of node to node protocol versions.
--
data NodeToNodeVersion = NodeToNodeV_1
  deriving (Eq, Ord, Enum, Show, Typeable)

instance Serialise NodeToNodeVersion where
    encode NodeToNodeV_1 = CBOR.encodeWord 1
    decode = do
      tag <- CBOR.decodeWord
      case tag of
        1 -> return NodeToNodeV_1
        _ -> fail "decode NodeToNodeVersion: unknown tag"

-- | Version data for NodeToNode protocol v1
--
newtype NodeToNodeVersionData = NodeToNodeVersionData
  { networkMagic :: NetworkMagic }
  deriving (Eq, Show, Typeable)

nodeToNodeCodecCBORTerm :: CodecCBORTerm Text NodeToNodeVersionData
nodeToNodeCodecCBORTerm = CodecCBORTerm {encodeTerm, decodeTerm}
    where
      encodeTerm :: NodeToNodeVersionData -> CBOR.Term
      encodeTerm NodeToNodeVersionData { networkMagic } =
        CBOR.TInt (fromIntegral $ unNetworkMagic networkMagic)

      decodeTerm :: CBOR.Term -> Either Text NodeToNodeVersionData
      decodeTerm (CBOR.TInt x) | x >= 0 && x <= 0xffffffff = Right (NodeToNodeVersionData $ NetworkMagic $ fromIntegral x)
                               | otherwise                 = Left $ T.pack $ "networkMagic out of bound: " <> show x
      decodeTerm t             = Left $ T.pack $ "unknown encoding: " ++ show t


-- | A specialised version of @'Ouroboros.Network.Socket.connectToNode'@.
--
connectTo
  :: NetworkConnectTracers NodeToNodeProtocols NodeToNodeVersion
  -> Versions NodeToNodeVersion
              DictVersion
              (OuroborosApplication InitiatorApp ConnectionId NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo = connectToNode cborTermVersionDataCodec


-- | Like 'connectTo' but specific to 'NodeToNodeV_1'.
--
connectTo_V1
  :: NetworkConnectTracers NodeToNodeProtocols NodeToNodeVersion
  -> NodeToNodeVersionData
  -> (OuroborosApplication InitiatorApp ConnectionId NodeToNodeProtocols IO BL.ByteString a b)
  -> Maybe Socket.AddrInfo
  -> Socket.AddrInfo
  -> IO ()
connectTo_V1 tracers versionData application localAddr remoteAddr =
    connectTo
      tracers
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)
      localAddr
      remoteAddr

-- | A specialised version of @'Ouroboros.Network.Socket.withServerNode'@.
-- It forks a thread which runs an accept loop (server thread):
--
-- * when the server thread throws an exception the main thread rethrows
--   it (by 'Async.wait')
-- * when an async exception is thrown to kill the main thread the server thread
--   will be cancelled as well (by 'withAsync')
--
withServer
  :: ( HasResponder appType ~ True)
  => NetworkServerTracers NodeToNodeProtocols NodeToNodeVersion
  -> NetworkMutableState
  -> Socket.AddrInfo
  -> Versions NodeToNodeVersion DictVersion (OuroborosApplication appType ConnectionId NodeToNodeProtocols IO BL.ByteString a b)
  -> ErrorPolicies Socket.SockAddr ()
  -> IO Void
withServer tracers networkState addr versions errPolicies =
  withServerNode
    tracers
    networkState
    addr
    cborTermVersionDataCodec
    (\(DictVersion _) -> acceptEq)
    versions
    errPolicies
    (\_ as -> Async.wait as)


-- | Like 'withServer' but specific to 'NodeToNodeV_1'.
--
withServer_V1
  :: ( HasResponder appType ~ True )
  => NetworkServerTracers NodeToNodeProtocols NodeToNodeVersion
  -> NetworkMutableState
  -> Socket.AddrInfo
  -> NodeToNodeVersionData
  -> (OuroborosApplication appType ConnectionId NodeToNodeProtocols IO BL.ByteString x y)
  -> ErrorPolicies Socket.SockAddr ()
  -> IO Void
withServer_V1 tracers networkState addr versionData application =
    withServer
      tracers networkState addr
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)


-- | 'ipSubscriptionWorker' which starts given application versions on each
-- established connection.
--
ipSubscriptionWorker
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToNodeProtocols NodeToNodeVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          ConnectionId
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO Void
ipSubscriptionWorker
  NetworkIPSubscriptionTracers
    { nistSubscriptionTracer
    , nistMuxTracer
    , nistHandshakeTracer
    , nistErrorPolicyTracer
    }
  networkState
  subscriptionParams
  versions
    = Subscription.ipSubscriptionWorker
        nistSubscriptionTracer
        nistErrorPolicyTracer
        networkState
        subscriptionParams
        (connectToNode'
          cborTermVersionDataCodec
          (NetworkConnectTracers nistMuxTracer nistHandshakeTracer)
          versions)


-- | Like 'ipSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
ipSubscriptionWorker_V1
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkIPSubscriptionTracers NodeToNodeProtocols NodeToNodeVersion
    -> NetworkMutableState
    -> IPSubscriptionParams ()
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          ConnectionId
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO Void
ipSubscriptionWorker_V1
  tracers
  networkState
  subscriptionParams
  versionData
  application
    = ipSubscriptionWorker
        tracers
        networkState
        subscriptionParams
        (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)


-- | 'dnsSubscriptionWorker' which starts given application versions on each
-- established connection.
--
dnsSubscriptionWorker
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkDNSSubscriptionTracers NodeToNodeProtocols NodeToNodeVersion ConnectionId
    -> NetworkMutableState
    -> DnsSubscriptionParams ()
    -> Versions
        NodeToNodeVersion
        DictVersion
        (OuroborosApplication
          appType
          ConnectionId
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO Void
dnsSubscriptionWorker
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
      ndstSubscriptionTracer
      ndstDnsTracer
      ndstErrorPolicyTracer
      networkState
      subscriptionParams
      (connectToNode'
        cborTermVersionDataCodec
        (NetworkConnectTracers ndstMuxTracer ndstHandshakeTracer)
        versions)


-- | Like 'dnsSubscriptionWorker' but specific to 'NodeToNodeV_1'.
--
dnsSubscriptionWorker_V1
    :: forall appType x y.
       ( HasInitiator appType ~ True )
    => NetworkDNSSubscriptionTracers NodeToNodeProtocols NodeToNodeVersion ConnectionId
    -> NetworkMutableState
    -> DnsSubscriptionParams ()
    -> NodeToNodeVersionData
    -> (OuroborosApplication
          appType
          ConnectionId
          NodeToNodeProtocols
          IO BL.ByteString x y)
    -> IO Void
dnsSubscriptionWorker_V1
  tracers
  networkState
  subscriptionParams
  versionData
  application =
     dnsSubscriptionWorker
      tracers
      networkState
      subscriptionParams
      (simpleSingletonVersions
          NodeToNodeV_1
          versionData
          (DictVersion nodeToNodeCodecCBORTerm)
          application)

-- | A minimal error policy for remote peers, which only handles exceptions
-- raised by `ouroboros-network`.
--
remoteNetworkErrorPolicy :: ErrorPolicies Socket.SockAddr a
remoteNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- Handshake client protocol error: we either did not recognise received
          -- version or we refused it.  This is only for outbound connections,
          -- thus we suspend the consumer.
          ErrorPolicy
            $ \(_ :: HandshakeClientProtocolError NodeToNodeVersion)
                  -> Just misconfiguredPeer
        
          -- exception thrown by `runDecoderWithByteLimit`
        , ErrorPolicy
            $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                   -> Just theyBuggyOrEvil

          -- deserialisation failure; this means that the remote peer is either
          -- buggy, adversarial, or the connection return garbage.  In the last
          -- case it's also good to shutdown both the consumer and the
          -- producer, as it's likely that the other side of the connection
          -- will return grabage as well.
        , ErrorPolicy
            $ \(_ :: DeserialiseFailure)
                  -> Just theyBuggyOrEvil

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
            $ \(e :: MuxError)
                  -> case errorType e of
                        MuxUnknownMiniProtocol  -> Just theyBuggyOrEvil
                        MuxDecodeError          -> Just theyBuggyOrEvil
                        MuxIngressQueueOverRun  -> Just theyBuggyOrEvil
                        MuxControlProtocolError -> Just theyBuggyOrEvil
                        MuxTooLargeMessage      -> Just theyBuggyOrEvil

                        -- in case of bearer closed / or IOException we suspend
                        -- the peer for a short time
                        --
                        -- TODO: an exponential backoff would be nicer than a fixed 20s
                        -- TODO: right now we cannot suspend just the
                        -- 'responder'.  If a 'responder' throws 'MuxError' we
                        -- might not want to shutdown the consumer (which is
                        -- using different connection), as we do below:
                        MuxBearerClosed         -> Just (SuspendPeer shortDelay shortDelay)
                        MuxIOException{}        -> Just (SuspendPeer shortDelay shortDelay)

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
        ],

      -- Exception raised during connect; suspend connecting to that peer for
      -- a 'shortDelay'
      epConErrorPolicies = [
          ErrorPolicy $ \(_ :: IOException) -> Just $
            SuspendConsumer shortDelay
        ],

      epReturnCallback = \_ _ _ -> ourBug
    }
  where
    theyBuggyOrEvil :: SuspendDecision DiffTime
    theyBuggyOrEvil = SuspendPeer defaultDelay defaultDelay

    misconfiguredPeer :: SuspendDecision DiffTime
    misconfiguredPeer = SuspendConsumer defaultDelay

    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

    defaultDelay :: DiffTime
    defaultDelay = 200 -- seconds

    shortDelay :: DiffTime
    shortDelay = 20 -- seconds

-- | Error policy for local clients.  This is equivalent to
-- 'nullErrorPolicies', but explicit in the errors which can be catched.
--
-- We are very permissive here, and very strict in the
-- `NodeToClient.networkErrorPolicy`.  After any failure the client will be
-- killed and not penalised by this policy.  This allows to restart the local
-- client without a delay.
--
localNetworkErrorPolicy :: ErrorPolicies Socket.SockAddr a
localNetworkErrorPolicy = ErrorPolicies {
      epAppErrorPolicies = [
          -- exception thrown by `runDecoderWithByteLimit`
          ErrorPolicy
            $ \(_ :: DecoderFailureOrTooMuchInput DeserialiseFailure)
                  -> Nothing

          -- deserialisation failure
        , ErrorPolicy
            $ \(_ :: DeserialiseFailure) -> Nothing

          -- the connection was unexpectedly closed, we suspend the peer for
          -- a 'shortDelay'
        , ErrorPolicy
          $ \(_ :: MuxError) -> Nothing
        ],

      -- The node never connects to a local client
      epConErrorPolicies = [],

      epReturnCallback = \_ _ _ -> ourBug
    }
  where
    ourBug :: SuspendDecision DiffTime
    ourBug = Throw

-- | Simple responder control function that discards the Protocol result.
simpleResponderControl :: forall m a. (MonadSTM m)
                       => (NodeToNodeProtocols -> MiniProtocolResponderControl m a)
                       -> m ()
simpleResponderControl rspFn = forever $
    -- Fetch, but ignore the result of any miniprotocol so that it can
    -- be restarted incase the initiator decides to talk to us again.
    void $ atomically $ fetchResponderResult ChainSyncWithHeadersPtcl
               `orElse` fetchResponderResult BlockFetchPtcl
               `orElse` fetchResponderResult TxSubmissionPtcl
  where
    fetchResponderResult :: MonadSTM m => NodeToNodeProtocols -> STM m a
    fetchResponderResult ptcl =
        let (MiniProtocolResponderControl action) = rspFn ptcl in
        action

-- | Result of waiting on a list of miniprotocols to finish. Only used by simpleInitiatorControl
data MiniProtocolClientResult = MpcRestart NodeToNodeProtocols
                              | MpcResult  NodeToNodeProtocols

-- | Simple initiator control function that starts all NodeToNodeProtocols and if one of them
-- finishes it will wait for restartDelay seconds before restarting the protocol.
simpleInitiatorControl :: forall m a.
                          ( MonadAsync m
                          , MonadSTM m
                          , MonadTimer m
                          )
                       => DiffTime
                       -> (NodeToNodeProtocols -> MiniProtocolInitiatorControl m a)
                       -> m ()
simpleInitiatorControl restartDelay ctrlFn = do
    startQ <- atomically $ newTBQueue 3
    atomically $ do
        writeTBQueue startQ ChainSyncWithHeadersPtcl
        writeTBQueue startQ BlockFetchPtcl
        writeTBQueue startQ TxSubmissionPtcl
    clientK startQ []

  where
    clientK :: TBQueue m NodeToNodeProtocols -> [(NodeToNodeProtocols, STM m a)] -> m ()
    clientK restartQ resultActions = do
        cr <- atomically $ waitOnRestartQueue restartQ
                `orElse` foldr (orElse . waitOnMiniProtocolClientResult) retry resultActions
        case cr of
                (MpcRestart ptcl) -> do
                    let (MiniProtocolInitiatorControl restart) = ctrlFn ptcl
                    resultAction <- atomically restart
                    clientK restartQ ((ptcl, resultAction):resultActions)
                (MpcResult ptcl) -> do
                    -- The return type is ignored, but one could act on the result here

                    -- Schedule a restart of the miniprotocol in the future
                    void $ async $ delayRestart restartQ ptcl
                    clientK restartQ $ filter (\(mp, _) -> mp /= ptcl) resultActions

    delayRestart :: TBQueue m NodeToNodeProtocols -> NodeToNodeProtocols -> m ()
    delayRestart q ptcl = do
        threadDelay restartDelay
        atomically $ writeTBQueue q ptcl

    waitOnRestartQueue :: TBQueue m NodeToNodeProtocols -> STM m MiniProtocolClientResult
    waitOnRestartQueue q = do
        ptcl <- readTBQueue q
        return $ MpcRestart ptcl

    waitOnMiniProtocolClientResult :: (NodeToNodeProtocols, STM m a)
                                    -> STM m MiniProtocolClientResult
    waitOnMiniProtocolClientResult (ptcl, fetchResult) = do
        void fetchResult
        return $ MpcResult ptcl
