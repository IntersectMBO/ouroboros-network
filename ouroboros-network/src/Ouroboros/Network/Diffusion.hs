{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , AcceptedConnectionsLimit (..)
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
import           Control.Exception
import           Control.Tracer (Tracer)
import           Data.Functor (void)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (SockAddr, AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket (LocalAddress, SocketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..) )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( NodeToNodeVersion (..)
                                              , AcceptedConnectionsLimit (..)
                                              , AcceptConnectionsPolicyTrace (..)
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Socket ( ConnectionId (..)
                                          , NetworkMutableState
                                          , newNetworkMutableState
                                          , cleanNetworkMutableState
                                          , NetworkServerTracers (..)
                                          )
import           Ouroboros.Network.Subscription.Ip
import           Ouroboros.Network.Subscription.Dns
import           Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import           Ouroboros.Network.Tracers

data DiffusionTracers = DiffusionTracers {
      dtIpSubscriptionTracer   :: Tracer IO (WithIPList (SubscriptionTrace SockAddr))
       -- ^ IP subscription tracer
    , dtDnsSubscriptionTracer  :: Tracer IO (WithDomainName (SubscriptionTrace SockAddr))
      -- ^ DNS subscription tracer
    , dtDnsResolverTracer      :: Tracer IO (WithDomainName DnsTrace)
      -- ^ DNS resolver tracer
    , dtMuxTracer              :: Tracer IO (WithMuxBearer (ConnectionId SockAddr) MuxTrace)
      -- ^ Mux tracer
    , dtMuxLocalTracer         :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)
      -- ^ Mux tracer for local clients
    , dtHandshakeTracer        :: Tracer IO NodeToNode.HandshakeTr
      -- ^ Handshake protocol tracer
    , dtHandshakeLocalTracer   :: Tracer IO NodeToClient.HandshakeTr
      -- ^ Handshake protocol tracer for local clients
    , dtErrorPolicyTracer      :: Tracer IO (WithAddr SockAddr     ErrorPolicyTrace)
    , dtLocalErrorPolicyTracer :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)
    , dtAcceptPolicyTracer     :: Tracer IO AcceptConnectionsPolicyTrace
      -- ^ Trace rate limiting of accepted connections
    }


-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daAddresses    :: Either [Socket.Socket] [AddrInfo]
      -- ^ sockets ready to accept connections or diffusion addresses
    , daLocalAddress :: Either Socket.Socket FilePath
      -- ^ AF_UNIX socket ready to accept connections or address for local clients
    , daIpProducers  :: IPSubscriptionTarget
      -- ^ ip subscription addresses
    , daDnsProducers :: [DnsSubscriptionTarget]
      -- ^ list of domain names to subscribe to
    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit
      -- ^ parameters for limiting number of accepted connections
    }

data DiffusionApplications = DiffusionApplications {

      daResponderApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion
                                       (OuroborosApplication
                                         ResponderApp SockAddr
                                         ByteString IO Void ())
      -- ^ NodeToNode reposnder application (server role)

    , daInitiatorApplication      :: Versions
                                       NodeToNodeVersion
                                       DictVersion 
                                       (OuroborosApplication
                                         InitiatorApp SockAddr
                                         ByteString IO () Void)
      -- ^ NodeToNode initiator application (client role)

    , daLocalResponderApplication :: Versions
                                       NodeToClientVersion
                                       DictVersion
                                       (OuroborosApplication
                                         ResponderApp LocalAddress
                                         ByteString IO Void ())
      -- ^ NodeToClient responder applicaton (server role)

    , daErrorPolicies :: ErrorPolicies
      -- ^ error policies
    }

data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
  deriving (Eq, Show)

instance Exception DiffusionFailure

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
                                    , daAcceptedConnectionsLimit
                                    }
                 applications@DiffusionApplications { daErrorPolicies } =
    withIOManager $ \iocp -> do

    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp

        daAddresses' = case daAddresses of
                            Left sds -> map Left sds
                            Right as -> map (Right . Socket.addrAddress) as

    -- networking mutable state
    networkState <- newNetworkMutableState
    networkLocalState <- newNetworkMutableState

    lias <- getInitiatorLocalAddresses snocket

    void $
      -- clean state thread
      Async.withAsync (cleanNetworkMutableState networkState) $ \cleanNetworkStateThread ->

        -- clean local state thread
        Async.withAsync (cleanNetworkMutableState networkLocalState) $ \cleanLocalNetworkStateThread ->

          -- fork server for local clients
          Async.withAsync (runLocalServer iocp networkLocalState) $ \_ ->

            -- fork servers for remote peers
            withAsyncs (runServer snocket networkState <$> daAddresses') $ \_ ->

              -- fork ip subscription
              Async.withAsync (runIpSubscriptionWorker snocket networkState lias) $ \_ ->

                -- fork dns subscriptions
                withAsyncs (runDnsSubscriptionWorker snocket networkState lias <$> daDnsProducers) $ \_ ->

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
                     , dtLocalErrorPolicyTracer
                     , dtAcceptPolicyTracer
                     } = tracers

    --
    -- We can't share portnumber with our server since we run separate
    -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
    -- applications instead of a 'MuxInitiatorAndResponderApplication'.
    -- This means we don't utilise full duplex connection.
    getInitiatorLocalAddresses :: SocketSnocket -> IO (LocalAddresses SockAddr)
    getInitiatorLocalAddresses sn =
      case daAddresses of
           Left sds -> do
               sockAddrs <- mapM (Snocket.getLocalAddr sn) sds

               return $ LocalAddresses
                 { laIpv4 = anyIPv4Addr sockAddrs
                 , laIpv6 = anyIPv6Addr sockAddrs
                 , laUnix = Nothing
                 }
           Right as ->
             return $ LocalAddresses
               { laIpv4 = anyIPv4Addr $ map Socket.addrAddress as
               , laIpv6 = anyIPv6Addr $ map Socket.addrAddress as
               , laUnix = Nothing
               }
      where
        -- Return an IPv4 address with an emphemeral portnumber if we use IPv4
        anyIPv4Addr :: [SockAddr] -> Maybe SockAddr
        anyIPv4Addr [] = Nothing
        anyIPv4Addr ((Socket.SockAddrInet _ _) : _) = Just (Socket.SockAddrInet 0 0)
        anyIPv4Addr (_ : sas) = anyIPv4Addr sas

        -- Return an IPv6 address with an emphemeral portnumber if we use IPv6
        anyIPv6Addr :: [SockAddr] -> Maybe SockAddr
        anyIPv6Addr [] = Nothing
        anyIPv6Addr ((Socket.SockAddrInet6 _ _ _ _ ) : _) = Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
        anyIPv6Addr (_ : sas) = anyIPv6Addr sas

    remoteErrorPolicy, localErrorPolicy :: ErrorPolicies
    remoteErrorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runLocalServer :: IOManager
                   -> NetworkMutableState LocalAddress
                   -> IO Void
    runLocalServer iocp networkLocalState =
      bracket
        (
          case daLocalAddress of
#if defined(mingw32_HOST_OS)
               -- Windows uses named pipes so can't take advantage of existing sockets
               Left _ -> throwIO UnsupportedReadySocket
#else
               Left sd -> do
                   a <- Socket.getSocketName sd
                   case a of
                        (Socket.SockAddrUnix path) ->
                          return (sd, Snocket.localSnocket iocp path)
                        _                          ->
                            -- TODO: This should be logged.
                            throwIO UnsupportedLocalSocketType
#endif
               Right a -> do
                   let sn = Snocket.localSnocket iocp a
                   sd <- Snocket.open sn (Snocket.addrFamily sn $ Snocket.localAddressFromPath a)
                   return (sd, sn)
        )
        (\(sd,sn) -> Snocket.close sn sd) -- We close the socket here, even if it was provided for us.
        (\(sd,sn) -> do

          case daLocalAddress of
               Left _ -> pure () -- If a socket was provided it should be ready to accept
               Right a -> do
                 Snocket.bind sn sd $ Snocket.localAddressFromPath a
                 Snocket.listen sn sd

          NodeToClient.withServer
            sn
            (NetworkServerTracers
              dtMuxLocalTracer
              dtHandshakeLocalTracer
              dtLocalErrorPolicyTracer
              dtAcceptPolicyTracer)
            networkLocalState
            sd
            (daLocalResponderApplication applications)
            localErrorPolicy
         )

    runServer :: SocketSnocket -> NetworkMutableState SockAddr -> Either Socket.Socket SockAddr -> IO Void
    runServer sn networkState address =
      bracket
        (
          case address of
               Left sd -> return sd
               Right a -> Snocket.open sn (Snocket.addrFamily sn a)
        )
        (Snocket.close sn) -- We close the socket here, even if it was provided for us.
        (\sd -> do

          case address of
               Left _ -> pure () -- If a socket was provided it should be ready to accept
               Right a -> do
                 Snocket.bind sn sd a
                 Snocket.listen sn sd

          NodeToNode.withServer
            sn
            (NetworkServerTracers
              dtMuxTracer
              dtHandshakeTracer
              dtErrorPolicyTracer
              dtAcceptPolicyTracer)
            networkState
            daAcceptedConnectionsLimit
            sd
            (daResponderApplication applications)
            remoteErrorPolicy
        )
    runIpSubscriptionWorker :: SocketSnocket
                            -> NetworkMutableState SockAddr
                            -> LocalAddresses SockAddr
                            -> IO Void
    runIpSubscriptionWorker sn networkState la = NodeToNode.ipSubscriptionWorker
      sn
      (NetworkSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtIpSubscriptionTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = la
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = remoteErrorPolicy
        , spSubscriptionTarget     = daIpProducers
        }
      (daInitiatorApplication applications)

    runDnsSubscriptionWorker :: SocketSnocket
                             -> NetworkMutableState SockAddr
                             -> LocalAddresses SockAddr
                             -> DnsSubscriptionTarget
                             -> IO Void
    runDnsSubscriptionWorker sn networkState la dnsProducer = NodeToNode.dnsSubscriptionWorker
      sn
      (NetworkDNSSubscriptionTracers
        dtMuxTracer
        dtHandshakeTracer
        dtErrorPolicyTracer
        dtDnsSubscriptionTracer
        dtDnsResolverTracer)
      networkState
      SubscriptionParams
        { spLocalAddresses         = la
        , spConnectionAttemptDelay = const Nothing
        , spErrorPolicies          = remoteErrorPolicy
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
