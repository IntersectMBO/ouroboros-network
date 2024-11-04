{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.P2P").
--
module Ouroboros.Network.Diffusion.NonP2P
  ( TracersExtra (..)
  , nullTracers
  , ApplicationsExtra (..)
  , ArgumentsExtra (..)
  , run
  ) where

import Control.Concurrent.Async qualified as Async
import Control.Exception
import Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import System.Exit (ExitCode)

import Network.Socket (SockAddr, Socket)
import Network.Socket qualified as Socket

import Ouroboros.Network.Snocket (LocalAddress, LocalSnocket, LocalSocket (..),
           SocketSnocket, localSocketFileDescriptor)
import Ouroboros.Network.Snocket qualified as Snocket
import Ouroboros.Network.Socket (NetworkMutableState, NetworkServerTracers (..),
           cleanNetworkMutableState, configureSocket, configureSystemdSocket,
           newNetworkMutableState)

import Ouroboros.Network.Context (ExpandedInitiatorContext (..),
           IsBigLedgerPeer (..), MinimalInitiatorContext (..))
import Ouroboros.Network.ControlMessage (continueForever)
import Ouroboros.Network.Diffusion.Common hiding (nullTracers)
import Ouroboros.Network.ErrorPolicy
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (NodeToClientVersion,
           NodeToClientVersionData)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (AcceptConnectionsPolicyTrace (..),
           DiffusionMode (..), NodeToNodeVersion, NodeToNodeVersionData,
           RemoteAddress)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.Subscription.Dns
import Ouroboros.Network.Subscription.Ip
import Ouroboros.Network.Subscription.Worker (LocalAddresses (..))
import Ouroboros.Network.Tracers

-- | NonP2P DiffusionTracers Extras
--
data TracersExtra = TracersExtra {
      -- | IP subscription tracer
      --
      dtIpSubscriptionTracer
        :: Tracer IO (WithIPList (SubscriptionTrace SockAddr))

      -- | DNS subscription tracer
      --
    , dtDnsSubscriptionTracer
        :: Tracer IO (WithDomainName (SubscriptionTrace SockAddr))

      -- | DNS resolver tracer
      --
    , dtDnsResolverTracer
        :: Tracer IO (WithDomainName DnsTrace)

    , dtErrorPolicyTracer
        :: Tracer IO (WithAddr SockAddr     ErrorPolicyTrace)

    , dtLocalErrorPolicyTracer
        :: Tracer IO (WithAddr LocalAddress ErrorPolicyTrace)

      -- | Trace rate limiting of accepted connections
      --
    , dtAcceptPolicyTracer
        :: Tracer IO AcceptConnectionsPolicyTrace
    }

nullTracers :: TracersExtra
nullTracers = nonP2PNullTracers
  where
    nonP2PNullTracers =
      TracersExtra {
        dtIpSubscriptionTracer   = nullTracer
      , dtDnsSubscriptionTracer  = nullTracer
      , dtDnsResolverTracer      = nullTracer
      , dtErrorPolicyTracer      = nullTracer
      , dtLocalErrorPolicyTracer = nullTracer
      , dtAcceptPolicyTracer     = nullTracer
      }

-- | NonP2P extra arguments
--
data ArgumentsExtra = ArgumentsExtra {
      -- | ip subscription addresses
      --
      daIpProducers  :: IPSubscriptionTarget

      -- | list of domain names to subscribe to
      --
    , daDnsProducers :: [DnsSubscriptionTarget]
    }

-- | NonP2P extra applications
--
newtype ApplicationsExtra = ApplicationsExtra {
      -- | Error policies
      --
      daErrorPolicies :: ErrorPolicies
    }

-- | Converts between OuroborosBundle and OuroborosApplication.
-- Converts from InitiatorResponderMode to ResponderMode.
--
-- Useful for sharing the same Applications modes.
--
mkResponderApp
    :: OuroborosBundleWithExpandedCtx     InitiatorResponderMode addr bs m a    b
    -> OuroborosApplicationWithMinimalCtx ResponderMode          addr bs m Void b
mkResponderApp bundle =
    OuroborosApplication $
      foldMap (fmap f) bundle
  where
    f :: MiniProtocolWithExpandedCtx InitiatorResponderMode bs addr m a    b
      -> MiniProtocolWithMinimalCtx  ResponderMode          bs addr m Void b
    f MiniProtocol { miniProtocolNum
                   , miniProtocolLimits
                   , miniProtocolRun = InitiatorAndResponderProtocol _initiator
                                                                      responder
                   } =
      MiniProtocol { miniProtocolNum
                   , miniProtocolLimits
                   , miniProtocolRun = ResponderProtocolOnly responder
                   }

run
    :: Tracers
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
         IO
    -> TracersExtra
    -> Arguments
         IO
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra
    -> Applications
         RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress  NodeToClientVersion NodeToClientVersionData
         () IO a
    -> ApplicationsExtra
    -> IO ()
run Tracers
      { dtMuxTracer
      , dtLocalMuxTracer
      , dtHandshakeTracer
      , dtLocalHandshakeTracer
      , dtDiffusionTracer
      }
    TracersExtra
      { dtIpSubscriptionTracer
      , dtDnsSubscriptionTracer
      , dtDnsResolverTracer
      , dtErrorPolicyTracer
      , dtLocalErrorPolicyTracer
      , dtAcceptPolicyTracer
      }
    Arguments
      { daIPv4Address
      , daIPv6Address
      , daLocalAddress
      , daAcceptedConnectionsLimit
      , daMode = diffusionMode
      }
     ArgumentsExtra
       { daIpProducers
       , daDnsProducers
       }
    applications
    ApplicationsExtra
      { daErrorPolicies } =
  traceException . withIOManager $ \iocp -> do
    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp
        localSnocket :: LocalSnocket
        localSnocket = Snocket.localSnocket iocp
        addresses = maybeToList daIPv4Address
                 ++ maybeToList daIPv6Address

    -- networking mutable state
    networkState <- newNetworkMutableState
    networkLocalState <- newNetworkMutableState

    lias <- getInitiatorLocalAddresses snocket

    let
        dnsSubActions = runDnsSubscriptionWorker snocket networkState lias
          <$> daDnsProducers

        serverActions = case diffusionMode of
          InitiatorAndResponderDiffusionMode ->
            runServer snocket networkState <$> addresses
          InitiatorOnlyDiffusionMode -> []

        localServerAction = runLocalServer localSnocket networkLocalState
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
    traceException f = catchJust
        (\e -> case fromException e :: Maybe ExitCode of
            Nothing -> Just e
            Just {} -> Nothing)
        f $ \(e :: SomeException) -> do
            traceWith dtDiffusionTracer (DiffusionErrored e)
            throwIO (DiffusionError e)

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
                  { laIpv4 = anyIPv4Addr ipv4
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
                , laIpv6 = anyIPv6Addr ipv6
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
        -- Return an IPv4 address with an ephemeral port number if we use IPv4
        anyIPv4Addr :: SockAddr -> Maybe SockAddr
        anyIPv4Addr Socket.SockAddrInet {} = Just (Socket.SockAddrInet 0 0)
        anyIPv4Addr _                      = Nothing

        -- Return an IPv6 address with an ephemeral port number if we use IPv6
        anyIPv6Addr :: SockAddr -> Maybe SockAddr
        anyIPv6Addr Socket.SockAddrInet6 {} =
          Just (Socket.SockAddrInet6 0 0 (0, 0, 0, 0) 0)
        anyIPv6Addr _ = Nothing

    remoteErrorPolicy, localErrorPolicy :: ErrorPolicies
    remoteErrorPolicy = NodeToNode.remoteNetworkErrorPolicy <> daErrorPolicies
    localErrorPolicy  = NodeToNode.localNetworkErrorPolicy <> daErrorPolicies

    runLocalServer :: LocalSnocket
                   -> NetworkMutableState LocalAddress
                   -> Either LocalSocket  LocalAddress
                   -> IO ()
    runLocalServer sn networkLocalState localAddress =
      bracket
        localServerInit
        localServerCleanup
        localServerBody
      where
        localServerInit :: IO LocalSocket
        localServerInit =
          case localAddress of
#if defined(mingw32_HOST_OS)
            -- Windows uses named pipes so can't take advantage of existing sockets
            Left _ -> do
              traceWith dtDiffusionTracer UnsupportedReadySocketCase
              throwIO UnsupportedReadySocket
#else
            Left sd -> do
              addr <- Snocket.getLocalAddr sn sd
              traceWith dtDiffusionTracer
                $ UsingSystemdSocket addr
              return sd
#endif
            Right addr -> do
              traceWith dtDiffusionTracer
                $ CreateSystemdSocketForSnocketPath addr
              sd <- Snocket.open
                    sn
                    (Snocket.addrFamily sn addr)
              traceWith dtDiffusionTracer
                $ CreatedLocalSocket addr
              return sd

        -- We close the socket here, even if it was provided for us.
        localServerCleanup :: LocalSocket -> IO ()
        localServerCleanup = Snocket.close sn

        localServerBody :: LocalSocket -> IO ()
        localServerBody sd = do
          case localAddress of
               -- If a socket was provided it should be ready to accept
               Left _ -> pure ()
               Right addr -> do
                 traceWith dtDiffusionTracer
                  . ConfiguringLocalSocket addr
                    =<< localSocketFileDescriptor sd

                 Snocket.bind sn sd addr

                 traceWith dtDiffusionTracer
                  . ListeningLocalSocket addr
                    =<< localSocketFileDescriptor sd

                 Snocket.listen sn sd

                 traceWith dtDiffusionTracer
                  . LocalSocketUp addr
                    =<< localSocketFileDescriptor sd

          traceWith dtDiffusionTracer
            . RunLocalServer =<< Snocket.getLocalAddr sn sd

          void $ NodeToClient.withServer
            sn
            (NetworkServerTracers
              dtLocalMuxTracer
              dtLocalHandshakeTracer
              dtLocalErrorPolicyTracer
              dtAcceptPolicyTracer)
            networkLocalState
            sd
            (daLocalResponderApplication applications)
            localErrorPolicy

    runServer :: SocketSnocket
              -> NetworkMutableState SockAddr
              -> Either Socket.Socket SockAddr
              -> IO ()
    runServer sn networkState address =
      bracket
        (
          case address of
               Left sd -> return sd
               Right addr -> do
                 traceWith dtDiffusionTracer
                  $ CreatingServerSocket addr
                 Snocket.open sn (Snocket.addrFamily sn addr)
        )
        (Snocket.close sn) -- We close the socket here, even if it was provided to us.
        (\sd -> do

          addr <- case address of
               -- If a socket was provided it should be ready to accept
               Left sock -> do
                 addr <- Snocket.getLocalAddr sn sock
                 configureSystemdSocket
                   (SystemdSocketConfiguration `contramap` dtDiffusionTracer)
                   sd addr
                 Snocket.getLocalAddr sn sd
               Right addr -> do
                 traceWith dtDiffusionTracer
                  $ ConfiguringServerSocket addr
                 configureSocket sd (Just addr)
                 Snocket.bind sn sd addr
                 traceWith dtDiffusionTracer
                  $ ListeningServerSocket addr
                 Snocket.listen sn sd
                 traceWith dtDiffusionTracer
                  $ ServerSocketUp addr
                 return addr

          traceWith dtDiffusionTracer $ RunServer (pure addr)

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
            -- NonP2P does not use Peer Sharing so the callback is set to return
            -- [].
            (mkResponderApp
              <$> daApplicationInitiatorResponderMode
                    applications)
            remoteErrorPolicy
        )
    runIpSubscriptionWorker :: SocketSnocket
                            -> NetworkMutableState SockAddr
                            -> LocalAddresses SockAddr
                            -> IO ()
    runIpSubscriptionWorker sn networkState la =
      void
        $ NodeToNode.ipSubscriptionWorker
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
            (contramapInitiatorCtx expandContext . fromOuroborosBundle
              <$> daApplicationInitiatorMode applications)

    runDnsSubscriptionWorker :: SocketSnocket
                             -> NetworkMutableState SockAddr
                             -> LocalAddresses SockAddr
                             -> DnsSubscriptionTarget
                             -> IO ()
    runDnsSubscriptionWorker sn networkState la dnsProducer =
      void
        $ NodeToNode.dnsSubscriptionWorker
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
            (contramapInitiatorCtx expandContext . fromOuroborosBundle
              <$> daApplicationInitiatorMode applications)


-- | Contramap context from `ExpandedInitiatorContext` to `MinimalInitiatorContext`.
--
expandContext :: MinimalInitiatorContext  RemoteAddress
              -> ExpandedInitiatorContext RemoteAddress IO
expandContext MinimalInitiatorContext { micConnectionId = connId } =
              ExpandedInitiatorContext {
                eicConnectionId    = connId,
                eicControlMessage  = continueForever Proxy,
                eicIsBigLedgerPeer = IsNotBigLedgerPeer
              }
