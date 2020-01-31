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
import qualified Codec.CBOR.Term as CBOR
import           Data.Functor (void)
import           Data.Functor.Contravariant (contramap)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.TypedProtocol.Driver (TraceSendRecv (..))
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Connections.Types (Connections,
                   Provenance (..))
import           Ouroboros.Network.Connections.Concurrent as Concurrent
import           Ouroboros.Network.Connections.Socket.Types (SockAddr (..),
                   withSockType)
import qualified Ouroboros.Network.Connections.Socket.Types as Connections
import           Ouroboros.Network.Connections.Socket.Client as Socket (client)
import           Ouroboros.Network.Connections.Socket.Server as Server (acceptLoopOn)
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
                                          , ConnectionHandle
                                          , NetworkServerTracers (..)
                                          , NetworkConnectTracers (..)
                                          , SomeVersionedApplication (..)
                                          )
import qualified Ouroboros.Network.Subscription as Subscription (worker)
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns

data DiffusionTracers = DiffusionTracers {
      dtIpSubscriptionTracer  :: Tracer IO (WithIPList (SubscriptionTrace Socket.SockAddr))
       -- ^ IP subscription tracer
    , dtDnsSubscriptionTracer :: Tracer IO (WithDomainName (SubscriptionTrace Socket.SockAddr))
      -- ^ DNS subscription tracer
    , dtDnsResolverTracer     :: Tracer IO (WithDomainName DnsTrace)
      -- ^ DNS resolver tracer
    , dtMuxTracer             :: Tracer IO (WithMuxBearer ConnectionId MuxTrace)
      -- ^ Mux tracer
    , dtMuxLocalTracer        :: Tracer IO (WithMuxBearer ConnectionId MuxTrace)
      -- ^ Mux tracer for local clients
    , dtHandshakeTracer       :: Tracer IO (WithMuxBearer ConnectionId
                                             (TraceSendRecv (Handshake NodeToNodeVersion CBOR.Term)))
      -- ^ Handshake protocol tracer
    , dtHandshakeLocalTracer  :: Tracer IO (WithMuxBearer ConnectionId
                                             (TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)))
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

        -- Note-to-node connections request: for a PeerConnection we do the
        -- responder app `daResponderApplication`, and for the others we choose
        -- the `daInitiatorApplication`. The types leave little room for error.
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

        -- How to run a local server: take the `daLocalAddress` and run an
        -- accept loop on it against the node-to-client connetions (yet to
        -- be constructed).
        runLocalServer n2cConnections = withSockType addr $ \addr' ->
          Server.acceptLoopOn addr' localClassifyRequest (acceptException addr)
            n2cConnections
          where
          addr = Socket.addrAddress daLocalAddress

        -- A node-to-node server will be run against a common connections
        -- manager (yet to be constructed) but on potentially many different
        -- bind addresses. This function will be mapped over the `daAddresses`.
        runServer n2nConnections addrInfo = withSockType addr $ \addr' ->
          Server.acceptLoopOn addr' classifyRequest (acceptException addr)
            n2nConnections
          where
          addr = Socket.addrAddress addrInfo

    -- Get 2 connection managers: one for node-to-client (n2c) and one for
    -- node-to-node (n2n).
    --
    -- These connections managers will throwTo any exceptions thrown by their
    -- connection handlers (whether incoming or outgoing) to this thread,
    -- wrapped in a special type `ExceptionInHandler`. Since we don't catch
    -- these, an exception in a handler will therefore bring down the whole
    -- node. This is apparently what we want, and `ErrorPolicies` is used to
    -- determine precisely when that should happen.
    NodeToClient.withConnections localErrorPolicy localConnectionRequest $ \n2cConnections ->
      NodeToNode.withConnections errorPolicy      connectionRequest      $ \n2nConnections -> do
        -- Run every thread concurrently such that an exception from any of
        -- them will kill the others and be re-thrown here.
        -- The application terminates when they are all done.
        let threads :: [Async.Concurrently ()]
            threads = fmap Async.Concurrently $ mconcat
              [ [ void (runLocalServer n2cConnections) ]
              , ( void . runServer n2nConnections) <$> daAddresses
              , [ void (runIpSubscriptionWorker n2nConnections) ]
              , ( void . runDnsSubscriptionWorker n2nConnections) <$> daDnsProducers
              ]
        -- mconcat'ing the `Concurrently ()`s runs them concurrently.
        _ <- Async.runConcurrently (mconcat threads)
        pure ()

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

    initiatorLocalAddresses :: LocalAddresses
    initiatorLocalAddresses = LocalAddresses
      { laIpv4 =
          -- IPv4 address
          --
          -- We can't share portnumber with our server since we run separate
          -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
          -- applications instead of a 'MuxInitiatorAndResponderApplication'.
          -- This means we don't utilise full duplex connection.
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET) daAddresses
            then Just (SockAddrIPv4 0 0)
            else Nothing
      , laIpv6 =
          -- IPv6 address
          if any (\ai -> Socket.addrFamily ai == Socket.AF_INET6) daAddresses
            then Just (SockAddrIPv6 0 0 (0, 0, 0, 0) 0)
            else Nothing
      , laUnix = Nothing
      }

    errorPolicy, localErrorPolicy :: ErrorPolicies Socket.SockAddr ()
    errorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runIpSubscriptionWorker
      :: Connections Connections.ConnectionId Socket.Socket Request (Concurrent.Reject reject) (Concurrent.Accept (ConnectionHandle IO))  IO
      -> IO ()
    runIpSubscriptionWorker connections =
      case ipSubscriptionTargets (ispIps daIpProducers) initiatorLocalAddresses of
        -- There were no addresses in the config (ispIps) which have an
        -- address in initiatorLocalAddresses of the same family.
        Nothing -> pure ()
        Just connIds -> Subscription.worker
          (contramap (WithIPList initiatorLocalAddresses (ispIps daIpProducers)) dtIpSubscriptionTracer)
          dtErrorPolicyTracer
          connIds
          (ispValency daIpProducers)
          ipRetryDelay
          (\connId -> Socket.client connId IpSubscriptionConnection)
          connections

    -- FIXME probably not ideal that we make a resolver `mkResolverIO` for
    -- each DNS target. Should make one (not have it take a port) and use it
    -- for all of the tagets.
    runDnsSubscriptionWorker
      :: Connections Connections.ConnectionId Socket.Socket Request (Concurrent.Reject reject) (Concurrent.Accept (ConnectionHandle IO))  IO
      -> DnsSubscriptionTarget
      -> IO ()
    runDnsSubscriptionWorker connections target = mkResolverIO (dstPort target) $ \resolver -> do
      addrs <- dnsResolve
        (contramap (WithDomainName (dstDomain target)) dtDnsResolverTracer)
        resolver
        (dstDomain target)
      case ipSubscriptionTargets addrs initiatorLocalAddresses of
        Nothing -> pure ()
        Just connIds -> Subscription.worker
          (contramap (WithDomainName (dstDomain target)) dtDnsSubscriptionTracer)
          dtErrorPolicyTracer
          connIds
          (dstValency target)
          ipRetryDelay
          (\connId -> Socket.client connId DnsSubscriptionConnection)
          connections
