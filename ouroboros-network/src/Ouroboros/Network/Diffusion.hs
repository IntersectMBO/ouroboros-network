{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs          #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , DiffusionApplications (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * re-exports
  , simpleSingletonVersions
  , IPSubscriptionTarget (..)
  , DnsSubscriptionTarget (..)
  , ConnectionId (..)
  )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import           Control.Tracer (Tracer, traceWith)
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Term as CBOR
import           Data.Functor (void)
import           Data.Functor.Contravariant (contramap)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Network.TypedProtocol.Driver.ByteLimit (DecoderFailureOrTooMuchInput (..))
import           Network.Mux.Types (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Connections.Types (Provenance (..))
import           Ouroboros.Network.Connections.Socket.Types (SockAddr (..),
                   withSockType)
import           Ouroboros.Network.Connections.Socket.Server (acceptLoopOn)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient ( NodeToClientProtocols (..)
                                                , NodeToClientVersion (..)
                                                )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( NodeToNodeProtocols (..)
                                              , NodeToNodeVersion (..)
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Socket ( ConnectionId (..)
                                          , NetworkMutableState
                                          , newNetworkMutableState
                                          , cleanNetworkMutableState
                                          , NetworkServerTracers (..)
                                          , NetworkConnectTracers (..)
                                          , SomeVersionedApplication (..)
                                          )
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import           Ouroboros.Network.Tracers

data DiffusionTracers = DiffusionTracers {
      dtIpSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
       -- ^ IP subscription tracer
    , dtDnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
      -- ^ DNS subscription tracer
    , dtDnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)
      -- ^ DNS resolver tracer
    , dtMuxTracer             :: Tracer IO (WithMuxBearer ConnectionId (MuxTrace NodeToNodeProtocols))
      -- ^ Mux tracer
    , dtMuxLocalTracer        :: Tracer IO (WithMuxBearer ConnectionId (MuxTrace NodeToClientProtocols))
      -- ^ Mux tracer for local clients
    , dtHandshakeTracer       :: Tracer IO (TraceSendRecv
                                              (Handshake NodeToNodeVersion CBOR.Term)
                                              ConnectionId
                                              (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
      -- ^ Handshake protocol tracer
    , dtHandshakeLocalTracer  :: Tracer IO (TraceSendRecv
                                              (Handshake NodeToClientVersion CBOR.Term)
                                              ConnectionId
                                              (DecoderFailureOrTooMuchInput CBOR.DeserialiseFailure))
      -- ^ Handshake protocol tracer for local clients
    , dtErrorPolicyTracer     :: Tracer IO (WithAddr Socket.SockAddr ErrorPolicyTrace)
    }


-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daAddresses    :: [AddrInfo]
      -- ^ diffusion addresses
    , daLocalAddress :: AddrInfo
      -- ^ address for local clients
    , daIpProducers  :: IPSubscriptionTarget
      -- ^ ip subscription addresses
    , daDnsProducers :: [DnsSubscriptionTarget]
      -- ^ list of domain names to subscribe to
    }

data DiffusionApplications = DiffusionApplications {

      daResponderApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion
                                       (OuroborosApplication
                                         'ResponderApp
                                         ConnectionId
                                         NodeToNodeProtocols
                                         IO
                                         ByteString
                                         Void
                                         ())
      -- ^ NodeToNode reposnder application (server role)

    , daInitiatorApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion 
                                       (OuroborosApplication
                                         'InitiatorApp
                                         ConnectionId
                                         NodeToNodeProtocols
                                         IO
                                         ByteString
                                         () Void)
      -- ^ NodeToNode initiator application (client role)

    , daLocalResponderApplication :: Versions
                                       NodeToClientVersion
                                       DictVersion
                                       (OuroborosApplication
                                          'ResponderApp
                                          ConnectionId
                                          NodeToClientProtocols
                                          IO
                                          ByteString
                                          Void
                                          ())
      -- ^ NodeToClient responder applicaton (server role)

    , daErrorPolicies :: ErrorPolicies Socket.SockAddr ()
      -- ^ error policies
      --
      -- TODO: one cannot use `forall a. ErrorPolicies SockAddr a`
    }

-- | The local server only accepts incoming requests (from clients in the
-- node-to-client protocol.)
data LocalRequest (p :: Provenance) where
  ClientConnection :: LocalRequest Remote

-- | The node-to-node server admits locally- and remotely-initiated connections.
-- Remotely come from the server accept loops. Locally come from IP and DNS
-- subscription systems.
data Request (p :: Provenance) where
  PeerConnection            :: Request Remote
  IpSubscriptionConnection  :: Request Local
  DnsSubscriptionConnection :: Request Local

runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments 
    -> DiffusionApplications
    -> IO ()
runDataDiffusion tracers
                 DiffusionArguments { daAddresses
                                    , daLocalAddress
                                    , daIpProducers
                                    , daDnsProducers
                                    }
                 applications@DiffusionApplications { daErrorPolicies } = do
    -- networking mutable state
    networkState <- newNetworkMutableState
    networkLocalState <- newNetworkMutableState

    -- Define how to fulfill node-to-client and node-to-node connection
    -- requests. The GADTs `LocalRequest` and `Request` describe all of the
    -- possibilities. In particular, the node-to-client protocol is restricted
    -- to responder only, i.e. this node will never try to initiate a
    -- node-to-client protocol.
    let localConnectionRequest
          :: LocalRequest provenance
          -> SomeVersionedApplication NodeToClientProtocols NodeToClientVersion DictVersion provenance
        localConnectionRequest ClientConnection = SomeVersionedResponderApp
          (NetworkServerTracers
            dtMuxLocalTracer
            dtHandshakeLocalTracer
            dtErrorPolicyTracer)
          (daLocalResponderApplication applications)

        connectionRequest
          :: Request provenance
          -> SomeVersionedApplication NodeToNodeProtocols NodeToNodeVersion DictVersion provenance
        connectionRequest PeerConnection = SomeVersionedResponderApp
          (NetworkServerTracers
            dtMuxTracer
            dtHandshakeTracer
            dtErrorPolicyTracer)
          (daResponderApplication applications)
        -- IP or DNS subscription requests are locally-initiated (requests
        -- from subscribers to _us_ are `PeerConnection` above.
        connectionRequest IpSubscriptionConnection  = SomeVersionedInitiatorApp
          (NetworkConnectTracers dtMuxTracer dtHandshakeTracer)
          (daInitiatorApplication applications)
        connectionRequest DnsSubscriptionConnection = SomeVersionedInitiatorApp
          (NetworkConnectTracers dtMuxTracer dtHandshakeTracer)
          (daInitiatorApplication applications)

        -- How to make requests for the accept loops for the local and remote
        -- server.
        localClassifyRequest :: SockAddr sockType -> LocalRequest Remote
        localClassifyRequest _ = ClientConnection

        classifyRequest :: SockAddr sockType -> Request Remote
        classifyRequest _ = PeerConnection

        acceptException :: Socket.SockAddr -> IOException -> IO ()
        acceptException a e = do
          traceWith (WithAddr a `contramap` dtErrorPolicyTracer) $ ErrorPolicyAcceptException e

        runLocalServer n2cConnections = withSockType addr $ \addr' ->
          acceptLoopOn addr' localClassifyRequest (acceptException addr)
            n2cConnections
          where
          addr = Socket.addrAddress daLocalAddress

        runServer n2nConnections addrInfo = withSockType addr $ \addr' ->
          acceptLoopOn addr' classifyRequest (acceptException addr)
            n2nConnections
          where
          addr = Socket.addrAddress addrInfo

    NodeToClient.withConnections networkLocalState localErrorPolicy localConnectionRequest $ \n2cConnections ->
      NodeToNode.withConnections networkState      errorPolicy      connectionRequest $ \n2nConnections -> 
        -- The n2cConnection and n2nConnections 
        void $
          -- clean state thread
          Async.withAsync (cleanNetworkMutableState networkState) $ \cleanNetworkStateThread ->

            -- clean local state thread
            Async.withAsync (cleanNetworkMutableState networkLocalState) $ \cleanLocalNetworkStateThread ->

              -- fork server for local clients
              Async.withAsync (runLocalServer n2cConnections) $ \_ ->

                -- fork servers for remote peers
                withAsyncs (runServer n2nConnections <$> daAddresses) $ \_ ->

                  -- fork ip subscription
                  Async.withAsync (runIpSubscriptionWorker networkState) $ \_ ->

                    -- fork dns subscriptions
                    withAsyncs (runDnsSubscriptionWorker networkState <$> daDnsProducers) $ \_ ->

                      -- If any other threads throws 'cleanNetowrkStateThread' and
                      -- 'cleanLocalNetworkStateThread' threads will will finish.
                      Async.waitEither_ cleanNetworkStateThread cleanLocalNetworkStateThread

  where

    DiffusionTracers { dtIpSubscriptionTracer
                     , dtDnsSubscriptionTracer
                     , dtDnsResolverTracer
                     , dtMuxTracer
                     , dtMuxLocalTracer
                     , dtHandshakeTracer
                     , dtHandshakeLocalTracer
                     , dtErrorPolicyTracer
                     } = tracers

    initiatorLocalAddresses :: LocalAddresses Socket.SockAddr
    initiatorLocalAddresses = LocalAddresses
      { laIpv4 =
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET) daAddresses
            then Just (Socket.SockAddrInet 0 0)
            else Nothing
      , laIpv6 =
          -- IPv6 address
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET6) daAddresses
            then Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
            else Nothing
      , laUnix = Nothing
      }

    errorPolicy, localErrorPolicy :: ErrorPolicies Socket.SockAddr ()
    errorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    -- TODO make DNS and IP subscription workers use a Connections term to
    -- simply initiate connections (which may already be up).

    runIpSubscriptionWorker :: NetworkMutableState -> IO Void
    runIpSubscriptionWorker networkState = NodeToNode.ipSubscriptionWorker
      (NetworkIPSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtIpSubscriptionTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = initiatorLocalAddresses
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = errorPolicy
        , spSubscriptionTarget     = daIpProducers
        }
      (daInitiatorApplication applications)

    runDnsSubscriptionWorker :: NetworkMutableState -> DnsSubscriptionTarget -> IO Void
    runDnsSubscriptionWorker networkState dnsProducer = NodeToNode.dnsSubscriptionWorker
      (NetworkDNSSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtDnsSubscriptionTracer
        dtDnsResolverTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = initiatorLocalAddresses
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = errorPolicy
        , spSubscriptionTarget     = dnsProducer
        }
      (daInitiatorApplication applications)


--
-- Auxilary functions
--

-- | Structural fold using 'Async.withAsync'.
--
withAsyncs :: [IO a] -> ([Async.Async a] -> IO b) -> IO b
withAsyncs as0 k = go [] as0
  where
    go threads []       = k threads
    go threads (a : as) = Async.withAsync a $ \thread -> go (thread : threads) as
