{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , nullTracers
  , DiffusionArguments (..)
  , AcceptedConnectionsLimit (..)
  , DiffusionApplications (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * re-exports
  , simpleSingletonVersions
  , ConnectionId (..)
    -- ** tracers
  , TraceLocalRootPeers (..)
  , TracePublicRootPeers (..)
  , TracePeerSelection (..)
  , DebugPeerSelection (..)
  , PeerSelectionActionsTrace (..)
  , ConnectionManagerTrace (..)
  , ConnectionTrace (..)
  , ServerTrace (..)
  )
  where

import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadFork
import           Control.Exception
import           Control.Tracer (Tracer, nullTracer)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)

import           Network.Mux ( MiniProtocolBundle (..)
                             , MiniProtocolInfo (..)
                             , MiniProtocolDirection (..)
                             , MuxTrace (..)
                             , WithMuxBearer (..)
                             )
import           Network.Mux.Timeout (withTimeoutSerial)
import           Network.Socket (SockAddr (..), AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket ( LocalAddress
                                           , SocketSnocket
                                           , LocalSnocket
                                           , LocalFD
                                           )
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec

import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.RethrowPolicy
import           Ouroboros.Network.ConnectionManager.Server ( ServerArguments (..)
                                                            , ServerTrace (..)
                                                            )
import qualified Ouroboros.Network.ConnectionManager.Server as Server
import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion.Policies
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DomainAddress)
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import           Ouroboros.Network.PeerSelection.Governor.Types ( TracePeerSelection (..)
                                                                , DebugPeerSelection (..)
                                                                )
import           Ouroboros.Network.PeerSelection.Governor.PeerStateActions ( PeerSelectionActionsTrace (..)
                                                                           , PeerStateActionsArguments (..)
                                                                           , PeerConnectionHandle
                                                                           , withPeerStateActions
                                                                           )
import           Ouroboros.Network.PeerSelection.Simple
import           Ouroboros.Network.Mux hiding (MiniProtocol (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( ConnectionId (..)
                                              , MiniProtocolParameters (..)
                                              , NodeToNodeVersion (..)
                                              , AcceptedConnectionsLimit (..)
                                              , chainSyncProtocolLimits
                                              , blockFetchProtocolLimits
                                              , txSubmissionProtocolLimits
                                              , keepAliveProtocolLimits

                                              , nodeToNodeHandshakeCodec
                                              )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.PeerSelection.RootPeersDNS ( TraceLocalRootPeers (..)
                                                              , TracePublicRootPeers (..)
                                                              )


data DiffusionTracers = DiffusionTracers {
      -- | Mux tracer
      dtMuxTracer
        :: Tracer IO (WithMuxBearer (ConnectionId SockAddr) MuxTrace)

      -- | Handshake protocol tracer
    , dtHandshakeTracer
        :: Tracer IO NodeToNode.HandshakeTr

    , dtTraceLocalRootPeersTracer
        :: Tracer IO TraceLocalRootPeers

    , dtTracePublicRootPeersTracer
        :: Tracer IO TracePublicRootPeers

    , dtTracePeerSelectionTracer
        :: Tracer IO (TracePeerSelection SockAddr)

    , dtDebugPeerSelectionTracer
        :: Tracer IO (DebugPeerSelection
                       SockAddr
                         (PeerConnectionHandle
                          InitiatorResponderMode
                          SockAddr
                          ByteString
                          IO () ()))

    , dtPeerSelectionActionsTracer
        :: Tracer IO (PeerSelectionActionsTrace SockAddr)

    , dtConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       SockAddr
                       (ConnectionTrace NodeToNodeVersion))

    , dtServerTracer
        :: Tracer IO (ServerTrace SockAddr)

      --
      -- NodeToClient tracers
      --

      -- | Mux tracer for local clients
    , dtLocalMuxTracer
        :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)

      -- | Handshake protocol tracer for local clients
    , dtLocalHandshakeTracer
        :: Tracer IO NodeToClient.HandshakeTr

      -- | Connection manager tracer for local clients
    , dtLocalConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       LocalAddress
                       (ConnectionTrace NodeToClientVersion))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer IO (ServerTrace LocalAddress)
    }


nullTracers :: DiffusionTracers
nullTracers = DiffusionTracers {
        dtMuxTracer                    = nullTracer
      , dtHandshakeTracer              = nullTracer
      , dtTraceLocalRootPeersTracer    = nullTracer
      , dtTracePublicRootPeersTracer   = nullTracer
      , dtTracePeerSelectionTracer     = nullTracer
      , dtDebugPeerSelectionTracer     = nullTracer
      , dtPeerSelectionActionsTracer   = nullTracer
      , dtConnectionManagerTracer      = nullTracer
      , dtServerTracer                 = nullTracer
      , dtLocalMuxTracer               = nullTracer
      , dtLocalHandshakeTracer         = nullTracer
      , dtLocalConnectionManagerTracer = nullTracer
      , dtLocalServerTracer            = nullTracer
  }

-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daIPv4Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ IPv4 socket ready to accept connections or diffusion addresses
    , daIPv6Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ IPV4 socket ready to accept connections or diffusion addresses
    , daLocalAddress :: Either Socket.Socket FilePath
      -- ^ AF_UNIX socket ready to accept connections or address for local clients
    , daPeerSelectionTargets :: PeerSelectionTargets
      -- ^ selection targets for the peer governor

    , daStaticLocalRootPeers :: [(Socket.SockAddr, PeerAdvertise)]
    , daLocalRootPeers       :: [(DomainAddress, PeerAdvertise)]
    , daPublicRootPeers      :: [DomainAddress]

    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit
      -- ^ parameters for limiting number of accepted connections
    }


-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data DiffusionApplications = DiffusionApplications {

      -- | NodeToNode reposnder and initiator applications for hot peers
      --
      daApplication
        :: Versions NodeToNodeVersion
                    DictVersion
                    (OuroborosBundle
                      InitiatorResponderMode SockAddr
                      ByteString IO () ())

    -- | NodeToClient responder applicaton (server role)
    --
    , daLocalResponderApplication
        :: Versions NodeToClientVersion
                    DictVersion
                    (OuroborosApplication
                      ResponderMode LocalAddress
                      ByteString IO Void ())
    -- | configuration of mini-protocol parameters; they inpact size limits of
    -- mux ingress queues.
    --
    , daMiniProtocolParameters :: MiniProtocolParameters

    -- | node-to-node rethrow policy
    --
    , daRethrowPolicy      :: RethrowPolicy

    -- | node-to-client rethrow policy
    , daLocalRethrowPolicy :: RethrowPolicy
    }

-- TODO: add a tracer for these misconfigurations
data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
                      | UnexpectedIPv4Address
                      | UnexpectedIPv6Address
                      | UnexpectedUnixAddress
                      | NoSocket
  deriving (Eq, Show)

instance Exception DiffusionFailure

runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments
    -> DiffusionApplications
    -> IO Void
runDataDiffusion tracers
                 DiffusionArguments { daIPv4Address
                                    , daIPv6Address
                                    , daLocalAddress
                                    , daPeerSelectionTargets
                                    , daStaticLocalRootPeers
                                    , daLocalRootPeers
                                    , daPublicRootPeers
                                    , daAcceptedConnectionsLimit
                                    }
                 DiffusionApplications { daApplication
                                       , daLocalResponderApplication
                                       , daRethrowPolicy
                                       , daMiniProtocolParameters
                                       , daLocalRethrowPolicy
                                       } =
    withIOManager $ \iocp ->
    withTimeoutSerial $ \timeout -> do

    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    connectionManagerIPv4Address
      <- traverse (either Socket.getSocketName (pure . Socket.addrAddress))
                  daIPv4Address
    case connectionManagerIPv4Address of
      Just SockAddrInet  {} -> pure ()
      Just SockAddrInet6 {} -> throwIO UnexpectedIPv6Address
      Just SockAddrUnix  {} -> throwIO UnexpectedUnixAddress
      Nothing               -> pure ()

    connectionManagerIPv6Address
      <- traverse (either Socket.getSocketName (pure . Socket.addrAddress))
                  daIPv6Address
    case connectionManagerIPv6Address of
      Just SockAddrInet {}  -> throwIO UnexpectedIPv4Address
      Just SockAddrInet6 {} -> pure ()
      Just SockAddrUnix {}  -> throwIO UnexpectedUnixAddress
      Nothing               -> pure ()

    serverControlChannel      <- Server.newControlChannel
    localServerControlChannel <- Server.newControlChannel

    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp

    --
    -- local connection manager
    --
    withLocalSocket iocp daLocalAddress $ \(localSnocket, localSocket) -> do
      let localConnectionHandler
            :: MuxConnectionHandler
                 ResponderMode
                 LocalAddress
                 NodeToClientVersion
                 ByteString IO Void ()
          localConnectionHandler =
            makeConnectionHandler
              dtLocalMuxTracer
              SResponderMode
              localMiniProtocolBundle
              HandshakeArguments {
                  haHandshakeTracer = dtLocalHandshakeTracer,
                  haHandshakeCodec = NodeToClient.nodeToClientHandshakeCodec,
                  haVersionDataCodec = cborTermVersionDataCodec,
                  haVersions =
                        (\(OuroborosApplication apps)
                          -> Bundle
                              (WithHot apps)
                              (WithWarm (\_ _ -> []))
                              (WithEstablished (\_ _ -> [])))
                    <$> daLocalResponderApplication
                }
              (Server.newOutboundConnection localServerControlChannel)
              (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

          localConnectionManagerArguments
            :: ConnectionManagerArguments
                 ResponderMode
                 (ConnectionTrace NodeToClientVersion)
                 LocalFD LocalAddress
                 (MuxPromise
                   ResponderMode
                   LocalAddress
                   NodeToClientVersion
                   ByteString
                   IO Void ())
                 IO
          localConnectionManagerArguments =
            ConnectionManagerArguments {
                connectionManagerTracer      = dtLocalConnectionManagerTracer,
                connectionManagerMuxTracer   = dtLocalMuxTracer,
                connectionManagerIPv4Address = Nothing,
                connectionManagerIPv6Address = Nothing,
                connectionManagerAddressType = const Nothing,
                connectionHandler            = localConnectionHandler,
                connectionSnocket            = localSnocket
              }

      withConnectionManager localConnectionManagerArguments $ \localConnectionManager ->

        --
        -- run local server
        --

        Async.withAsync
          (Server.run
            ServerArguments {
                serverSockets           = localSocket :| [],
                serverSnocket           = localSnocket,
                serverTracer            = dtLocalServerTracer,
                serverControlChannel    = localServerControlChannel,
                serverConnectionLimits  = AcceptedConnectionsLimit maxBound maxBound 0,
                serverConnectionManager = localConnectionManager
              }) $ \localServerThread -> do


          --
          -- remote connection manager
          --

          let connectionHandler
                :: MuxConnectionHandler
                     InitiatorResponderMode
                     SockAddr
                     NodeToNodeVersion
                     ByteString IO () ()
              connectionHandler =
                makeConnectionHandler
                  dtMuxTracer
                  SInitiatorResponderMode
                  miniProtocolBundle
                  HandshakeArguments {
                      haHandshakeTracer = dtHandshakeTracer,
                      haHandshakeCodec = nodeToNodeHandshakeCodec,
                      haVersionDataCodec = cborTermVersionDataCodec,
                      haVersions = daApplication
                    }
                  (Server.newOutboundConnection serverControlChannel)
                  (mainThreadId, rethrowPolicy <> daRethrowPolicy)

              connectionManagerArguments
                :: ConnectionManagerArguments
                     InitiatorResponderMode
                     (ConnectionTrace NodeToNodeVersion)
                     Socket.Socket SockAddr
                     (MuxPromise
                       InitiatorResponderMode
                       SockAddr
                       NodeToNodeVersion
                       ByteString
                       IO () ())
                     IO
              connectionManagerArguments =
                ConnectionManagerArguments {
                    connectionManagerTracer       = dtConnectionManagerTracer,
                    connectionManagerMuxTracer    = dtMuxTracer,
                    connectionManagerIPv4Address,
                    connectionManagerIPv6Address,
                    connectionManagerAddressType  =
                      \case
                        SockAddrInet  {} -> Just IPv4Address
                        SockAddrInet6 {} -> Just IPv6Address
                        SockAddrUnix  {} -> Nothing,

                    connectionHandler,
                    connectionSnocket             = snocket
                  }

          withConnectionManager connectionManagerArguments $ \connectionManager -> do
            let mkAddr :: AddrInfo -> (Socket.Family, SockAddr)
                mkAddr addr = (Socket.addrFamily addr, Socket.addrAddress addr)

            withSockets snocket (catMaybes
                                  [ fmap (fmap mkAddr) daIPv4Address
                                  , fmap (fmap mkAddr) daIPv6Address
                                  ])
                                $ \sockets ->

              --
              -- Run server
              --

              Async.withAsync
                (Server.run
                  ServerArguments {
                      serverSockets = sockets,
                      serverSnocket = snocket,
                      serverTracer  = dtServerTracer,
                      serverControlChannel,
                      serverConnectionLimits = daAcceptedConnectionsLimit,
                      serverConnectionManager = connectionManager
                    })
                    $ \serverThread ->

                --
                -- Run peer selection
                --

                withPeerStateActions
                  timeout
                  PeerStateActionsArguments {
                      spsTracer = dtPeerSelectionActionsTracer,
                      spsDeactivateTimeout = Diffusion.Policies.deactivateTimeout,
                      spsCloseConnectionTimeout = Diffusion.Policies.closeConnectionTimeout,
                      spsConnectionManager = connectionManager
                    }
                  $ \peerStateActions ->

                  withPeerSelectionActions
                    dtTraceLocalRootPeersTracer
                    dtTracePublicRootPeersTracer
                    timeout
                    daPeerSelectionTargets
                    (Map.fromList daStaticLocalRootPeers)
                    daLocalRootPeers
                    daPublicRootPeers
                    peerStateActions
                    $ \mbLocalPeerRootProviderThread peerSelectionActions ->

                    Async.withAsync
                      (Governor.peerSelectionGovernor
                        dtTracePeerSelectionTracer
                        dtDebugPeerSelectionTracer
                        peerSelectionActions
                        Diffusion.Policies.simplePeerSelectionPolicy)
                      $ \governorThread ->

                        -- wait for any thread to fail
                        snd <$> Async.waitAny (maybeToList mbLocalPeerRootProviderThread
                                              ++ [ localServerThread
                                                 , serverThread
                                                 , governorThread
                                                 ])

  where
    DiffusionTracers { dtMuxTracer
                     , dtHandshakeTracer
                     , dtTracePeerSelectionTracer
                     , dtDebugPeerSelectionTracer
                     , dtPeerSelectionActionsTracer
                     , dtTraceLocalRootPeersTracer
                     , dtTracePublicRootPeersTracer
                     , dtConnectionManagerTracer
                     , dtServerTracer
                     , dtLocalMuxTracer
                     , dtLocalHandshakeTracer
                     , dtLocalConnectionManagerTracer
                     , dtLocalServerTracer
                     } = tracers


    miniProtocolBundle :: MiniProtocolBundle InitiatorResponderMode
    miniProtocolBundle = MiniProtocolBundle
      [ MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolDir    = InitiatorDirection,
          miniProtocolLimits = chainSyncProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolDir    = ResponderDirection,
          miniProtocolLimits = chainSyncProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 3,
          miniProtocolDir    = InitiatorDirection,
          miniProtocolLimits = blockFetchProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 3,
          miniProtocolDir    = ResponderDirection,
          miniProtocolLimits = blockFetchProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 4,
          miniProtocolDir    = InitiatorDirection,
          miniProtocolLimits = txSubmissionProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 4,
          miniProtocolDir    = ResponderDirection,
          miniProtocolLimits = txSubmissionProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 8,
          miniProtocolDir    = InitiatorDirection,
          miniProtocolLimits = keepAliveProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 8,
          miniProtocolDir    = ResponderDirection,
          miniProtocolLimits = keepAliveProtocolLimits daMiniProtocolParameters
        }
      -- TODO: `tip-sample` protocol
      ]

    localMiniProtocolBundle :: MiniProtocolBundle ResponderMode
    localMiniProtocolBundle = MiniProtocolBundle
        [ MiniProtocolInfo {
            miniProtocolNum    = MiniProtocolNum 5,
            miniProtocolDir    = ResponderDirectionOnly,
            miniProtocolLimits = maximumMiniProtocolLimits
          }
        , MiniProtocolInfo {
            miniProtocolNum    = MiniProtocolNum 6,
            miniProtocolDir    = ResponderDirectionOnly,
            miniProtocolLimits = maximumMiniProtocolLimits
          }
        , MiniProtocolInfo {
            miniProtocolNum    = MiniProtocolNum 7,
            miniProtocolDir    = ResponderDirectionOnly,
            miniProtocolLimits = maximumMiniProtocolLimits
          }
        ]
      where
        maximumMiniProtocolLimits :: MiniProtocolLimits
        maximumMiniProtocolLimits =
            MiniProtocolLimits {
              maximumIngressQueue = 0xffffffff
            }

    -- Only the 'IOManagerError's are fatal, all the other exceptions in the
    -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
    -- this is so).
    rethrowPolicy =
      RethrowPolicy $ \_ctx err ->
        case fromException err of
          Just (_ :: IOManagerError) -> ShutdownNode
          Nothing                    -> mempty


withSockets :: SocketSnocket
            -> [Either Socket.Socket (Socket.Family, SockAddr)]
            -> (NonEmpty Socket.Socket -> IO a)
            -> IO a
withSockets sn addresses k = go [] addresses
  where
    go !acc (a : as) = withSocket a (\sock -> go (sock : acc) as)
    go []   []       = throw NoSocket
    go !acc []       = k $! NonEmpty.fromList (reverse acc)

    withSocket :: Either Socket.Socket (Socket.Family, SockAddr)
               -> (Socket.Socket -> IO a)
               -> IO a
    withSocket (Left sock) f =
      bracket (pure sock) (Snocket.close sn) f
    withSocket (Right (fam, addr)) f =
      bracket
        (Snocket.open sn (Snocket.SocketFamily fam))
        (Snocket.close sn)
        $ \sock -> do
          Snocket.bind sn sock addr
          Snocket.listen sn sock
          f sock


withLocalSocket :: IOManager
                -> Either Socket.Socket FilePath
                -> ((LocalSnocket, LocalFD) -> IO a)
                -> IO a
withLocalSocket iocp localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> throwIO UnsupportedReadySocket
#else
         Left sd -> do
             a <- Socket.getSocketName sd
             case a of
                  (Socket.SockAddrUnix path) ->
                    return (Snocket.localSnocket iocp path, sd)
                  _                          ->
                      -- TODO: This should be logged.
                      throwIO UnsupportedLocalSocketType
#endif
         Right a -> do
             let sn = Snocket.localSnocket iocp a
             sd <- Snocket.open sn (Snocket.addrFamily sn $ Snocket.localAddressFromPath a)
             return (sn, sd)
    )
    (uncurry Snocket.close)
    -- We close the socket here, even if it was provided for us.
    k
