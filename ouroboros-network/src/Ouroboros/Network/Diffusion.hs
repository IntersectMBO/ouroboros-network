{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
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
  , LedgerPeersConsensusInterface (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * re-exports
  , simpleSingletonVersions
  , ConnectionId (..)
    -- ** tracers
  , DiffusionInitializationTracer(..)
  , TraceLocalRootPeers (..)
  , TracePublicRootPeers (..)
  , TracePeerSelection (..)
  , DebugPeerSelection (..)
  , PeerSelectionActionsTrace (..)
  , ConnectionManagerTrace (..)
  , ConnectionHandlerTrace (..)
  , ServerTrace (..)
  )
  where

import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Exception
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           System.Random (newStdGen)

import qualified Data.IP as IP
import           Network.Mux ( MiniProtocolBundle (..)
                             , MiniProtocolInfo (..)
                             , MiniProtocolDirection (..)
                             , MuxTrace (..)
                             , WithMuxBearer (..)
                             )
import           Network.Mux.Timeout (withTimeoutSerial)
import qualified Network.DNS as DNS
import           Network.Socket (SockAddr (..), AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket ( FileDescriptor
                                           , LocalAddress
                                           , LocalSnocket
                                           , LocalSocket (..)
                                           , SocketSnocket
                                           , localSocketFileDescriptor
                                           )
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec

import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Server ( ServerArguments (..)
                                                            , ServerTrace (..)
                                                            )
import qualified Ouroboros.Network.ConnectionManager.Server as Server
import qualified Ouroboros.Network.ConnectionManager.Server.ControlChannel as Server
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.HasIPAddress
import           Ouroboros.Network.RethrowPolicy
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion.Policies
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.PeerSelection.RootPeersDNS ( DomainAddress
                                                              , resolveDomainAddresses
                                                              )
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import           Ouroboros.Network.PeerSelection.Governor.Types ( TracePeerSelection (..)
                                                                , DebugPeerSelection (..)
                                                                )
import           Ouroboros.Network.PeerSelection.LedgerPeers ( LedgerPeersConsensusInterface (..)
                                                             , TraceLedgerPeers
                                                             , NumberOfPeers
                                                             , UseLedgerAfter (..)
                                                             , runLedgerPeers)
import           Ouroboros.Network.PeerSelection.PeerStateActions ( PeerSelectionActionsTrace (..)
                                                                  , PeerStateActionsArguments (..)
                                                                  , PeerConnectionHandle
                                                                  , withPeerStateActions
                                                                  )
import           Ouroboros.Network.PeerSelection.Simple
import           Ouroboros.Network.Mux hiding (MiniProtocol (..))
import           Ouroboros.Network.NodeToClient ( NodeToClientVersion (..)
                                                , NodeToClientVersionData)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode ( ConnectionId (..)
                                              , MiniProtocolParameters (..)
                                              , NodeToNodeVersion (..)
                                              , NodeToNodeVersionData (..)
                                              , AcceptedConnectionsLimit (..)
                                              , DiffusionMode (..)
                                              , RemoteAddress
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


-- TODO: use LocalAddress where appropriate rather than 'path'.
--
data DiffusionInitializationTracer
  = RunServer !(NonEmpty SockAddr)
  | RunLocalServer !LocalAddress
  | UsingSystemdSocket !FilePath
  -- Rename as 'CreateLocalSocket'
  | CreateSystemdSocketForSnocketPath !FilePath
  | CreatedLocalSocket !FilePath
  | ConfiguringLocalSocket !FilePath !FileDescriptor
  | ListeningLocalSocket !FilePath !FileDescriptor
  | LocalSocketUp  !FilePath !FileDescriptor
  -- Rename as 'CreateServerSocket'
  | CreatingServerSocket !SockAddr
  | ConfiguringServerSocket !SockAddr
  | ListeningServerSocket !SockAddr
  | ServerSocketUp !SockAddr
  -- Rename as 'UnsupportedLocalSocketType'
  | UnsupportedLocalSystemdSocket !SockAddr
  -- Remove (this is impossible case), there's no systemd on Windows
  | UnsupportedReadySocketCase
  | DiffusionErrored SomeException
    deriving Show

data DiffusionTracers = DiffusionTracers {
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
                       (ConnectionHandlerTrace NodeToNodeVersion NodeToNodeVersionData))

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
                       (ConnectionHandlerTrace NodeToClientVersion NodeToClientVersionData))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer IO (ServerTrace LocalAddress)

      -- | Diffusion initialisation tracer
    , dtDiffusionInitializationTracer
        :: Tracer IO DiffusionInitializationTracer

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer      :: Tracer IO TraceLedgerPeers
    }


nullTracers :: DiffusionTracers
nullTracers = DiffusionTracers {
        dtMuxTracer                     = nullTracer
      , dtHandshakeTracer               = nullTracer
      , dtTraceLocalRootPeersTracer     = nullTracer
      , dtTracePublicRootPeersTracer    = nullTracer
      , dtTracePeerSelectionTracer      = nullTracer
      , dtDebugPeerSelectionTracer      = nullTracer
      , dtPeerSelectionActionsTracer    = nullTracer
      , dtConnectionManagerTracer       = nullTracer
      , dtServerTracer                  = nullTracer
      , dtLocalMuxTracer                = nullTracer
      , dtLocalHandshakeTracer          = nullTracer
      , dtLocalConnectionManagerTracer  = nullTracer
      , dtLocalServerTracer             = nullTracer
      , dtDiffusionInitializationTracer = nullTracer
      , dtLedgerPeersTracer             = nullTracer
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
    , daDiffusionMode :: DiffusionMode
      -- ^ run in initiator only mode
    }


-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data DiffusionApplications ntnAddr ntcAddr ntnVersionData ntcVersionData m =
    DiffusionApplications {

      -- | NodeToNode reposnder and initiator applications for hot peers
      --
      daApplication
        :: Versions NodeToNodeVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorResponderMode ntnAddr
                      ByteString m () ())

    -- | NodeToClient responder applicaton (server role)
    --
    , daLocalResponderApplication
        :: Versions NodeToClientVersion
                    ntcVersionData
                    (OuroborosApplication
                      ResponderMode ntcAddr
                      ByteString m Void ())
    -- | configuration of mini-protocol parameters; they inpact size limits of
    -- mux ingress queues.
    --
    , daMiniProtocolParameters :: MiniProtocolParameters

    -- | node-to-node rethrow policy
    --
    , daRethrowPolicy      :: RethrowPolicy

    -- | node-to-client rethrow policy
    , daLocalRethrowPolicy :: RethrowPolicy

    ,  daLedgerPeersCtx :: LedgerPeersConsensusInterface m
      -- ^ Interface used to get peers from the current ledger.
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


-- | Run data diffusion service.  It allows to connect to upstream peers,
-- accept connection from downstream peers (if run in
-- 'InitiatorAndResponderDiffusionMode').  It also runs a local service which
-- allows to use node-to-client protocol to obtain information from the running
-- system.  This is used by 'cardano-cli' and a like services.
--
runDataDiffusion
    :: DiffusionTracers
    -> DiffusionArguments
    -> DiffusionApplications
         RemoteAddress LocalAddress
         NodeToNodeVersionData NodeToClientVersionData
         IO
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
                                    , daDiffusionMode
                                    }
                 DiffusionApplications { daApplication
                                       , daLocalResponderApplication
                                       , daRethrowPolicy
                                       , daMiniProtocolParameters
                                       , daLocalRethrowPolicy
                                       , daLedgerPeersCtx
                                       } =
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- nameing convenstion is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handle (\e -> traceWith tracer (DiffusionErrored e)
               >> throwIO e) $
    withIOManager $ \iocp ->
    withTimeoutSerial $ \timeout -> do

    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    cmIPv4Address
      <- traverse (either Socket.getSocketName (pure . Socket.addrAddress))
                  daIPv4Address
    case cmIPv4Address of
      Just SockAddrInet  {} -> pure ()
      Just SockAddrInet6 {} -> throwIO UnexpectedIPv6Address
      Just SockAddrUnix  {} -> throwIO UnexpectedUnixAddress
      Nothing               -> pure ()

    cmIPv6Address
      <- traverse (either Socket.getSocketName (pure . Socket.addrAddress))
                  daIPv6Address
    case cmIPv6Address of
      Just SockAddrInet {}  -> throwIO UnexpectedIPv4Address
      Just SockAddrInet6 {} -> pure ()
      Just SockAddrUnix {}  -> throwIO UnexpectedUnixAddress
      Nothing               -> pure ()

    -- Maybe with a useful context to run the server
    mbServerControlChannel
      <- case daDiffusionMode of
          InitiatorOnlyDiffusionMode ->
            -- action which we pass to connection handler
            pure Nothing
          InitiatorAndResponderDiffusionMode -> do
            -- we pass 'Server.newOutboundConnection serverControlChannel' to
            -- connection handler
            Just <$> Server.newControlChannel

    serverStateVar            <- Server.newStateVarIO
    localServerStateVar       <- Server.newStateVarIO
    localServerControlChannel <- Server.newControlChannel

    -- RNG used for picking random peers from the ledger.
    ledgerPeersRng <- newStdGen
    -- Request interface, supply the number of peers desired.
    ledgerPeersReq <- newEmptyTMVarIO :: IO (StrictTMVar IO NumberOfPeers)
    -- Response interface, returns a Set of peers. Nothing indicates that the
    -- ledger hasn't caught up to `useLedgerAfter`. May return less than
    -- the number of peers requested.
    ledgerPeersRsp <- newEmptyTMVarIO :: IO (StrictTMVar IO (Maybe (Set SockAddr, DiffTime)))
    -- Require the ledger to be passed the provided slot number before it is used as a source of
    -- public root peers.
    -- After 2020-12-08 14:53:36.03 UTC TODO: Should be configurable
    let useLedgerAfter = UseLedgerAfter 15872925

    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp

        localConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0

    --
    -- local connection manager
    --
    withLocalSocket iocp tracer daLocalAddress $ \localSnocket localSocket -> do
      let localConnectionHandler
            :: MuxConnectionHandler
                 ResponderMode
                 LocalAddress
                 NodeToClientVersion
                 NodeToClientVersionData
                 ByteString IO Void ()
          localConnectionHandler =
            makeConnectionHandler
              dtLocalMuxTracer
              SResponderMode
              localMiniProtocolBundle
              HandshakeArguments {
                  haHandshakeTracer = dtLocalHandshakeTracer,
                  haHandshakeCodec = NodeToClient.nodeToClientHandshakeCodec,
                  haVersionDataCodec = cborTermVersionDataCodec NodeToClient.nodeToClientCodecCBORTerm,
                  haVersions =
                        (\(OuroborosApplication apps)
                          -> Bundle
                              (WithHot apps)
                              (WithWarm (\_ _ -> []))
                              (WithEstablished (\_ _ -> [])))
                    <$> daLocalResponderApplication,
                  haAcceptVersion = acceptableVersion
                }
              localDataFlow
              (Server.newOutboundConnection localServerControlChannel)
              (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

          localConnectionManagerArguments
            :: ConnectionManagerArguments
                 ResponderMode
                 (ConnectionHandlerTrace NodeToClientVersion NodeToClientVersionData)
                 LocalSocket LocalAddress
                 Void -- IP addresses are not used by the node-to-client server.
                 (Handle ResponderMode
                         LocalAddress
                         ByteString
                         IO Void ())
                  (HandleError ResponderMode
                               NodeToClientVersion)
                 (NodeToClientVersion, NodeToClientVersionData)
                 IO
          localConnectionManagerArguments =
            ConnectionManagerArguments {
                cmTracer              = dtLocalConnectionManagerTracer,
                cmMuxTracer           = dtLocalMuxTracer,
                cmIPv4Address         = Nothing,
                cmIPv6Address         = Nothing,
                cmSnocket             = localSnocket,
                cmHasIPAddress        = WithResponderMode (),
                connectionHandler     = localConnectionHandler,
                connectionDataFlow    = uncurry localDataFlow,
                cmPrunePolicy         = Server.randomPrunePolicy localServerStateVar,
                cmConnectionsLimits   = localConnectionLimits,
                cmClassifyHandleError = classifyHandleError,
                cmLocalIPs            = return Set.empty
              }

      withConnectionManager localConnectionManagerArguments $ \localConnectionManager -> do

        --
        -- run local server
        --

        traceWith tracer . RunLocalServer
          =<< Snocket.getLocalAddr localSnocket localSocket

        Async.withAsync
          (Server.run
            ServerArguments {
                serverSockets           = localSocket :| [],
                serverSnocket           = localSnocket,
                serverTracer            = dtLocalServerTracer,
                serverControlChannel    = localServerControlChannel,
                serverConnectionLimits  = localConnectionLimits,
                serverConnectionManager = localConnectionManager,
                serverStateVar          = localServerStateVar
              }) $ \localServerThread -> do


          --
          -- remote connection manager
          --

          let connectionHandler
                :: MuxConnectionHandler
                     InitiatorResponderMode
                     SockAddr
                     NodeToNodeVersion
                     NodeToNodeVersionData
                     ByteString IO () ()
              connectionHandler =
                makeConnectionHandler
                  dtMuxTracer
                  SInitiatorResponderMode
                  miniProtocolBundle
                  HandshakeArguments {
                      haHandshakeTracer = dtHandshakeTracer,
                      haHandshakeCodec = nodeToNodeHandshakeCodec,
                      haVersionDataCodec = cborTermVersionDataCodec NodeToNode.nodeToNodeCodecCBORTerm,
                      haVersions = daApplication,
                      haAcceptVersion = acceptableVersion
                    }
                  nodeDataFlow
                  (maybe (\_ _ -> pure ()) Server.newOutboundConnection mbServerControlChannel)
                  (mainThreadId, rethrowPolicy <> daRethrowPolicy)

              connectionManagerArguments
                :: ConnectionManagerArguments
                     InitiatorResponderMode
                     (ConnectionHandlerTrace NodeToNodeVersion NodeToNodeVersionData)
                     Socket.Socket SockAddr
                     IP.IP
                     (Handle InitiatorResponderMode
                             SockAddr
                             ByteString
                             IO () ())
                     (HandleError InitiatorResponderMode
                                  NodeToNodeVersion)
                     (NodeToNodeVersion, NodeToNodeVersionData)
                     IO
              connectionManagerArguments =
                ConnectionManagerArguments {
                    cmTracer              = dtConnectionManagerTracer,
                    cmMuxTracer           = dtMuxTracer,
                    cmIPv4Address,
                    cmIPv6Address,
                    cmSnocket             = snocket,
                    cmHasIPAddress        = WithInitiatorResponderMode sockAddrHasIPAddress (),
                    connectionHandler,
                    connectionDataFlow    = uncurry nodeDataFlow,
                    cmPrunePolicy         = Server.randomPrunePolicy serverStateVar,
                    cmConnectionsLimits   = daAcceptedConnectionsLimit,
                    cmClassifyHandleError = classifyHandleError,
                    -- TODO: pass local IPs!
                    cmLocalIPs            = return Set.empty
                  }

          withConnectionManager connectionManagerArguments $ \connectionManager -> do
              
              --
              -- peer state actions
              --
              -- Peer state actions run a job pool in the background which
              -- tracks threads forked by 'PeerStateActions'
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
                Async.withAsync
                  (runLedgerPeers
                    ledgerPeersRng
                    dtLedgerPeersTracer
                    useLedgerAfter
                    daLedgerPeersCtx
                    (resolveDomainAddresses
                      dtTracePublicRootPeersTracer
                      timeout
                      DNS.defaultResolvConf
                      )
                    (takeTMVar ledgerPeersReq)
                    (putTMVar ledgerPeersRsp)
                  )
                  $ \ledgerPeerThread ->

                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      timeout
                      daPeerSelectionTargets
                      (Map.fromList daStaticLocalRootPeers)
                      daLocalRootPeers
                      daPublicRootPeers
                      peerStateActions
                      (putTMVar ledgerPeersReq)
                      (takeTMVar ledgerPeersRsp)
                      $ \mbLocalPeerRootProviderThread peerSelectionActions ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionTracer
                          peerSelectionActions
                          Diffusion.Policies.simplePeerSelectionPolicy)
                        $ \governorThread ->

                        case mbServerControlChannel of
                          -- 'InitiatorOnlyDiffusionMode'
                          Nothing ->

                            -- wait for any thread to fail
                            snd <$> Async.waitAny
                              (maybeToList mbLocalPeerRootProviderThread
                              ++ [ localServerThread
                                 , governorThread
                                 , ledgerPeerThread
                                 ])

                          -- InitiatorAndResponderDiffusionMode
                          Just serverControlChannel -> do
                            let mkAddr :: AddrInfo -> (Socket.Family, SockAddr)
                                mkAddr addr = (Socket.addrFamily addr, Socket.addrAddress addr)

                            withSockets tracer snocket
                                        (catMaybes
                                          [ fmap (fmap mkAddr) daIPv4Address
                                          , fmap (fmap mkAddr) daIPv6Address
                                          ])
                                        $ \sockets addresses -> do
                              --
                              -- Run server
                              --
                              traceWith tracer (RunServer addresses)
                              Async.withAsync
                                (Server.run
                                  ServerArguments {
                                      serverSockets = sockets,
                                      serverSnocket = snocket,
                                      serverTracer  = dtServerTracer,
                                      serverControlChannel,
                                      serverConnectionLimits = daAcceptedConnectionsLimit,
                                      serverConnectionManager = connectionManager,
                                      serverStateVar = serverStateVar
                                    })
                                    $ \serverThread ->

                                      -- wait for any thread to fail
                                      snd <$> Async.waitAny
                                        (maybeToList mbLocalPeerRootProviderThread
                                        ++ [ localServerThread
                                           , serverThread
                                           , governorThread
                                           , ledgerPeerThread
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
                     , dtLedgerPeersTracer
                     -- the tracer
                     , dtDiffusionInitializationTracer = tracer
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


nodeDataFlow :: NodeToNodeVersion
             -> NodeToNodeVersionData
             -> DataFlow
nodeDataFlow v NodeToNodeVersionData { diffusionMode = InitiatorAndResponderDiffusionMode }
           | v >= NodeToNodeV_6
           = Duplex
nodeDataFlow _ _ = Unidirectional


localDataFlow :: NodeToClientVersion
              -> NodeToClientVersionData
              -> DataFlow
localDataFlow _ _ = Unidirectional


withSockets :: Tracer IO DiffusionInitializationTracer
            -> SocketSnocket
            -> [Either Socket.Socket (Socket.Family, SockAddr)]
            -> (NonEmpty Socket.Socket -> NonEmpty Socket.SockAddr -> IO a)
            -> IO a
withSockets tracer sn addresses k = go [] addresses
  where
    go !acc (a : as) = withSocket a (\sa -> go (sa : acc) as)
    go []   []       = throw NoSocket
    go !acc []       =
      let acc' = NonEmpty.fromList (reverse acc)
      in (k $! (fst <$> acc')) $! (snd <$> acc')

    withSocket :: Either Socket.Socket (Socket.Family, SockAddr)
               -> ((Socket.Socket, Socket.SockAddr) -> IO a)
               -> IO a
    withSocket (Left sock) f =
      bracket
        (pure sock)
        (Snocket.close sn)
        $ \_sock -> do
          !addr <- Socket.getSocketName sock
          f (sock, addr)
    withSocket (Right (fam, !addr)) f =
      bracket
        (do traceWith tracer (CreatingServerSocket addr)
            Snocket.open sn (Snocket.SocketFamily fam))
        (Snocket.close sn)
        $ \sock -> do
          traceWith tracer $ ConfiguringServerSocket addr
          Snocket.bind sn sock addr
          traceWith tracer $ ListeningServerSocket addr
          Snocket.listen sn sock
          traceWith tracer $ ServerSocketUp addr
          f (sock, addr)


withLocalSocket :: IOManager
                -> Tracer IO DiffusionInitializationTracer
                -> Either Socket.Socket FilePath
                -> (LocalSnocket -> LocalSocket -> IO a)
                -> IO a
withLocalSocket iocp tracer localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> traceWith tracer UnsupportedReadySocketCase
                >> throwIO UnsupportedReadySocket
#else
         Left sd -> do
             addr <- Socket.getSocketName sd
             case addr of
                  (Socket.SockAddrUnix path) -> do
                    traceWith tracer (UsingSystemdSocket path)
                    return (Left ( Snocket.localSnocket iocp path
                                 , (LocalSocket sd)
                                 ))
                  _  -> do
                    traceWith tracer $ UnsupportedLocalSystemdSocket addr
                    throwIO UnsupportedLocalSocketType
#endif
         Right addr -> do
             let sn :: LocalSnocket
                 sn = Snocket.localSnocket iocp addr
             traceWith tracer $ CreateSystemdSocketForSnocketPath addr
             sd <- Snocket.open sn Snocket.LocalFamily
             traceWith tracer $ CreatedLocalSocket addr
             return (Right (sn, sd, addr))
    )
    -- We close the socket here, even if it was provided to us.
    (\case
      Left  (sn, sd)    -> Snocket.close sn sd 
      Right (sn, sd, _) -> Snocket.close sn sd)
    $ \case 
      -- unconfigured socket
      Right (sn, sd, addr) -> do
        traceWith tracer . ConfiguringLocalSocket addr
           =<< localSocketFileDescriptor sd
        Snocket.bind sn sd (NodeToClient.LocalAddress addr)
        traceWith tracer . ListeningLocalSocket addr
           =<< localSocketFileDescriptor sd
        Snocket.listen sn sd
        traceWith tracer . LocalSocketUp addr
           =<< localSocketFileDescriptor sd
        k sn sd

      -- pre-configured systemd socket
      Left (sn, sd) -> k sn sd
