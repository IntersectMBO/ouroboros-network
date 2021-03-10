{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , DiffusionInitializationTracer (..)
  , LedgerPeersConsensusInterface (..)
  )
  where

import qualified Control.Concurrent.Async as Async
import           Control.Exception
import           Control.Tracer (Tracer, traceWith)
import           Data.Functor (void)
import           Data.Maybe (maybeToList)
import           Data.Foldable (asum)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import           Network.Socket (AddrInfo, SockAddr)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket ( FileDescriptor
                                           , LocalAddress
                                           , LocalSnocket
                                           , LocalSocket (..)
                                           , SocketSnocket
                                           , localSocketFileDescriptor
                                           )
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ErrorPolicy
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Mux
import           Ouroboros.Network.NodeToClient ( NodeToClientVersion (..)
                                                , NodeToClientVersionData )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( NodeToNodeVersion (..)
                                              , NodeToNodeVersionData
                                              , AcceptedConnectionsLimit (..)
                                              , AcceptConnectionsPolicyTrace (..)
                                              , DiffusionMode (..)
                                              , RemoteAddress
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.PeerSelection.LedgerPeers ( LedgerPeersConsensusInterface (..)
                                                             , TraceLedgerPeers)
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

data DiffusionInitializationTracer
  = RunServer !SockAddr
  | RunLocalServer !LocalAddress
  | UsingSystemdSocket !FilePath
  | CreateSystemdSocketForSnocketPath !FilePath
  | CreatedLocalSocket !FilePath
  | ConfiguringLocalSocket !FilePath !FileDescriptor
  | ListeningLocalSocket !FilePath !FileDescriptor
  | LocalSocketUp  !FilePath !FileDescriptor
  | CreatingServerSocket !SockAddr
  | ConfiguringServerSocket !SockAddr
  | ListeningServerSocket !SockAddr
  | ServerSocketUp !SockAddr
  | UnsupportedLocalSystemdSocket !SockAddr
  | UnsupportedReadySocketCase
  | DiffusionErrored SomeException
    deriving Show

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
    , dtDiffusionInitializationTracer :: Tracer IO DiffusionInitializationTracer
    , dtLedgerPeersTracer      :: Tracer IO TraceLedgerPeers
    }


-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daIPv4Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ IPv4 socket ready to accept connections or diffusion addresses
    , daIPv6Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ IPV4 socket ready to accept connections or diffusion addresses
    , daLocalAddress :: Maybe (Either Socket.Socket FilePath)
      -- ^ AF_UNIX socket ready to accept connections or address for local clients
    , daIpProducers   :: IPSubscriptionTarget
      -- ^ ip subscription addresses
    , daDnsProducers  :: [DnsSubscriptionTarget]
      -- ^ list of domain names to subscribe to
    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit
      -- ^ parameters for limiting number of accepted connections
    , daDiffusionMode :: DiffusionMode
      -- ^ run in initiator only mode
    }

data DiffusionApplications ntnAddr ntcAddr ntnVersionData ntcVersionData m = DiffusionApplications {

      daResponderApplication      :: Versions
                                       NodeToNodeVersion
                                       ntnVersionData
                                       (OuroborosApplication
                                         ResponderMode ntnAddr
                                         ByteString m Void ())
      -- ^ NodeToNode reposnder application (server role)

    , daInitiatorApplication      :: Versions
                                       NodeToNodeVersion
                                       ntnVersionData
                                       (OuroborosApplication
                                         InitiatorMode ntnAddr
                                         ByteString m () Void)
      -- ^ NodeToNode initiator application (client role)

    , daLocalResponderApplication :: Versions
                                       NodeToClientVersion
                                       ntcVersionData
                                       (OuroborosApplication
                                         ResponderMode ntcAddr
                                         ByteString m Void ())
      -- ^ NodeToClient responder applicaton (server role)

    , daErrorPolicies :: ErrorPolicies
      -- ^ error policies

    ,  daLedgerPeersCtx :: LedgerPeersConsensusInterface m
      -- ^ Interface used to get peers from the current ledger.
    }

data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
  deriving (Eq, Show)

instance Exception DiffusionFailure

runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments
    -> DiffusionApplications
         RemoteAddress LocalAddress
         NodeToNodeVersionData NodeToClientVersionData
         IO
    -> IO ()
runDataDiffusion tracers
                 DiffusionArguments { daIPv4Address
                                    , daIPv6Address
                                    , daLocalAddress
                                    , daIpProducers
                                    , daDnsProducers
                                    , daAcceptedConnectionsLimit
                                    , daDiffusionMode
                                    }
                 applications@DiffusionApplications { daErrorPolicies } =
  traceException . withIOManager $ \iocp -> do
    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp
        addresses = maybeToList daIPv4Address
                 ++ maybeToList daIPv6Address

    -- networking mutable state
    networkState <- newNetworkMutableState
    networkLocalState <- newNetworkMutableState

    lias <- getInitiatorLocalAddresses snocket

    let
        dnsSubActions = runDnsSubscriptionWorker snocket networkState lias
          <$> daDnsProducers

        serverActions = case daDiffusionMode of
          InitiatorAndResponderDiffusionMode ->
            runServer snocket networkState . fmap Socket.addrAddress
              <$> addresses
          InitiatorOnlyDiffusionMode -> []

        localServerAction = runLocalServer iocp networkLocalState
          <$> maybeToList daLocalAddress

        actions =
          [ -- clean state thread
            cleanNetworkMutableState networkState
          , -- clean local state thread
            cleanNetworkMutableState networkLocalState
          , -- fork ip subscription
            runIpSubscriptionWorker snocket networkState lias
          ]
          -- fork dns subscriptions
          ++ dnsSubActions
          -- fork servers for remote peers
          ++ serverActions
          -- fork server for local clients
          ++ localServerAction

    -- Runs all threads in parallel, using Async.Concurrently's Alternative instance
    Async.runConcurrently $ asum $ Async.Concurrently <$> actions

  where
    traceException :: IO a -> IO a
    traceException f = catch f $ \(e :: SomeException) -> do
      traceWith dtDiffusionInitializationTracer (DiffusionErrored e) 
      throwIO e

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
                     , dtDiffusionInitializationTracer
                     } = tracers

    --
    -- We can't share portnumber with our server since we run separate
    -- 'MuxInitiatorApplication' and 'MuxResponderApplication'
    -- applications instead of a 'MuxInitiatorAndResponderApplication'.
    -- This means we don't utilise full duplex connection.
    getInitiatorLocalAddresses :: SocketSnocket -> IO (LocalAddresses SockAddr)
    getInitiatorLocalAddresses sn = do
        localIpv4 <-
          case daIPv4Address of
              Just (Right ipv4) -> do
                return LocalAddresses
                  { laIpv4 = anyIPv4Addr (Socket.addrAddress ipv4)
                  , laIpv6 = Nothing
                  , laUnix = Nothing
                  }

              Just (Left ipv4Sock) -> do
                 ipv4Addrs <- Snocket.getLocalAddr sn ipv4Sock
                 return LocalAddresses
                   { laIpv4 = anyIPv4Addr ipv4Addrs
                   , laIpv6 = Nothing
                   , laUnix = Nothing
                   }

              Nothing -> do
                 return LocalAddresses
                   { laIpv4 = Nothing
                   , laIpv6 = Nothing
                   , laUnix = Nothing
                   }

        localIpv6 <-
          case daIPv6Address of
            Just (Right ipv6) -> do
              return LocalAddresses
                { laIpv4 = Nothing
                , laIpv6 = anyIPv6Addr (Socket.addrAddress ipv6)
                , laUnix = Nothing
                }

            Just (Left ipv6Sock) -> do
              ipv6Addrs <- Snocket.getLocalAddr sn ipv6Sock
              return LocalAddresses
                { laIpv4 = Nothing
                , laIpv6 = anyIPv6Addr ipv6Addrs
                , laUnix = Nothing
                }

            Nothing -> do
              return LocalAddresses
                { laIpv4 = Nothing
                , laIpv6 = Nothing
                , laUnix = Nothing
                  }

        return (localIpv4 <> localIpv6)
      where
        -- Return an IPv4 address with an emphemeral portnumber if we use IPv4
        anyIPv4Addr :: SockAddr -> Maybe SockAddr
        anyIPv4Addr (Socket.SockAddrInet _ _) = Just (Socket.SockAddrInet 0 0)
        anyIPv4Addr _ = Nothing

        -- Return an IPv6 address with an emphemeral portnumber if we use IPv6
        anyIPv6Addr :: SockAddr -> Maybe SockAddr
        anyIPv6Addr (Socket.SockAddrInet6 _ _ _ _ ) = Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
        anyIPv6Addr _ = Nothing

    remoteErrorPolicy, localErrorPolicy :: ErrorPolicies
    remoteErrorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runLocalServer :: IOManager
                   -> NetworkMutableState LocalAddress
                   -> Either Socket.Socket FilePath
                   -> IO ()
    runLocalServer iocp networkLocalState localAddress =
      bracket
        localServerInit
        localServerCleanup
        localServerBody
      where
        localServerInit :: IO (LocalSocket, LocalSnocket)
        localServerInit =
          case localAddress of
#if defined(mingw32_HOST_OS)
               -- Windows uses named pipes so can't take advantage of existing sockets
               Left _ -> do
                   traceWith dtDiffusionInitializationTracer UnsupportedReadySocketCase
                   throwIO UnsupportedReadySocket
#else
               Left sd -> do
                   a <- Socket.getSocketName sd
                   case a of
                        (Socket.SockAddrUnix path) -> do
                          traceWith dtDiffusionInitializationTracer $ UsingSystemdSocket path
                          return (LocalSocket sd, Snocket.localSnocket iocp path)
                        unsupportedAddr -> do
                          traceWith dtDiffusionInitializationTracer $ UnsupportedLocalSystemdSocket unsupportedAddr
                          throwIO UnsupportedLocalSocketType
#endif
               Right addr -> do
                   let sn = Snocket.localSnocket iocp addr
                   traceWith dtDiffusionInitializationTracer $ CreateSystemdSocketForSnocketPath addr
                   sd <- Snocket.open sn (Snocket.addrFamily sn $ Snocket.localAddressFromPath addr)
                   traceWith dtDiffusionInitializationTracer $ CreatedLocalSocket addr
                   return (sd, sn)

        -- We close the socket here, even if it was provided for us.
        localServerCleanup :: (LocalSocket, LocalSnocket) -> IO ()
        localServerCleanup (sd, sn) = Snocket.close sn sd

        localServerBody :: (LocalSocket, LocalSnocket) -> IO ()
        localServerBody (sd, sn) = do
          case localAddress of
               -- If a socket was provided it should be ready to accept
               Left _ -> pure ()
               Right path -> do
                 traceWith dtDiffusionInitializationTracer . ConfiguringLocalSocket path
                    =<< localSocketFileDescriptor sd

                 Snocket.bind sn sd $ Snocket.localAddressFromPath path

                 traceWith dtDiffusionInitializationTracer . ListeningLocalSocket path
                    =<< localSocketFileDescriptor sd

                 Snocket.listen sn sd

                 traceWith dtDiffusionInitializationTracer . LocalSocketUp path
                    =<< localSocketFileDescriptor sd

          traceWith dtDiffusionInitializationTracer . RunLocalServer =<< Snocket.getLocalAddr sn sd

          void $ NodeToClient.withServer
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

    runServer :: SocketSnocket -> NetworkMutableState SockAddr -> Either Socket.Socket SockAddr -> IO ()
    runServer sn networkState address =
      bracket
        (
          case address of
               Left sd -> return sd
               Right addr -> do
                 traceWith dtDiffusionInitializationTracer $ CreatingServerSocket addr
                 Snocket.open sn (Snocket.addrFamily sn addr)
        )
        (Snocket.close sn) -- We close the socket here, even if it was provided for us.
        (\sd -> do

          addr <- case address of
               -- If a socket was provided it should be ready to accept
               Left _ -> Snocket.getLocalAddr sn sd
               Right addr -> do
                 traceWith dtDiffusionInitializationTracer $ ConfiguringServerSocket addr
                 Snocket.bind sn sd addr
                 traceWith dtDiffusionInitializationTracer $ ListeningServerSocket addr
                 Snocket.listen sn sd
                 traceWith dtDiffusionInitializationTracer $ ServerSocketUp addr
                 return addr

          traceWith dtDiffusionInitializationTracer $ RunServer addr

          void $ NodeToNode.withServer
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
                            -> IO ()
    runIpSubscriptionWorker sn networkState la = void $ NodeToNode.ipSubscriptionWorker
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
                             -> IO ()
    runDnsSubscriptionWorker sn networkState la dnsProducer = void $ NodeToNode.dnsSubscriptionWorker
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
