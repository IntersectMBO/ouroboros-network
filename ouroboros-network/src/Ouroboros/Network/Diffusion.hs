{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Network.Diffusion
  ( DiffusionTracers (..)
  , DiffusionArguments (..)
  , AcceptedConnectionsLimit (..)
  , DiffusionApplications (..)
  , LedgerPeersConsensusInterface (..)
  , OuroborosApplication (..)
  , runDataDiffusion
    -- * Constants for /node-to-client/ diffusion
  , local_PROTOCOL_IDLE_TIMEOUT
  , local_TIME_WAIT_TIMEOUT
    -- * re-exports
  , simpleSingletonVersions
  , ConnectionId (..)
    -- ** Tracers
  , nullTracers
  , DiffusionInitializationTracer(..)
  , TraceLocalRootPeers (..)
  , TracePublicRootPeers (..)
  , TracePeerSelection (..)
  , DebugPeerSelection (..)
  , PeerSelectionActionsTrace (..)
  , PeerSelectionCounters (..)
  , ConnectionManagerTrace (..)
  , ConnectionHandlerTrace (..)
  , ConnectionManagerCounters (..)
  , ServerTrace (..)
  , InboundGovernorTrace (..)
  )
  where

import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Exception
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Foldable (asum)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Set (Set)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           System.Random (newStdGen, split)

import           Network.Mux ( MiniProtocolBundle (..)
                             , MiniProtocolInfo (..)
                             , MiniProtocolDirection (..)
                             , MuxTrace (..)
                             , WithMuxBearer (..)
                             )
import           Network.Mux.Timeout (withTimeoutSerial)
import qualified Network.DNS as DNS
import           Network.Socket (SockAddr (..), Socket, AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket ( FileDescriptor
                                           , LocalAddress
                                           , LocalSnocket
                                           , LocalSocket (..)
                                           , SocketSnocket
                                           , localSocketFileDescriptor
                                           )
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Protocol.Handshake.Codec

import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.RethrowPolicy
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion.Policies
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.PeerSelection.RootPeersDNS ( DomainAddress
                                                              , resolveDomainAddresses
                                                              , RelayAddress (..)
                                                              )
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..))
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import           Ouroboros.Network.PeerSelection.Governor.Types ( TracePeerSelection (..)
                                                                , DebugPeerSelection (..)
                                                                , PeerSelectionCounters (..)
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
import           Ouroboros.Network.Server2 ( ServerArguments (..)
                                           , ServerTrace (..)
                                           )
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Mux hiding (MiniProtocol (..))
import           Ouroboros.Network.MuxMode
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

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer IO (DebugPeerSelection
                       SockAddr
                         (NodeToNodePeerConnectionHandle InitiatorMode Void))

    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer IO (DebugPeerSelection
                       SockAddr
                         (NodeToNodePeerConnectionHandle InitiatorResponderMode ()))

    , dtTracePeerSelectionCounters
        :: Tracer IO PeerSelectionCounters

    , dtPeerSelectionActionsTracer
        :: Tracer IO (PeerSelectionActionsTrace SockAddr)

    , dtConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       SockAddr
                       (ConnectionHandlerTrace NodeToNodeVersion NodeToNodeVersionData))

    , dtServerTracer
        :: Tracer IO (ServerTrace SockAddr)

    , dtInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace SockAddr)

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

      -- | Inbound protocol governor tracer for local clients
    , dtLocalInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace LocalAddress)

      -- | Diffusion initialisation tracer
    , dtDiffusionInitializationTracer
        :: Tracer IO DiffusionInitializationTracer

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer      :: Tracer IO TraceLedgerPeers
    }


nullTracers :: DiffusionTracers
nullTracers = DiffusionTracers {
    dtMuxTracer                                  = nullTracer
  , dtHandshakeTracer                            = nullTracer
  , dtTraceLocalRootPeersTracer                  = nullTracer
  , dtTracePublicRootPeersTracer                 = nullTracer
  , dtTracePeerSelectionTracer                   = nullTracer
  , dtDebugPeerSelectionInitiatorTracer          = nullTracer
  , dtDebugPeerSelectionInitiatorResponderTracer = nullTracer
  , dtTracePeerSelectionCounters                 = nullTracer
  , dtPeerSelectionActionsTracer                 = nullTracer
  , dtConnectionManagerTracer                    = nullTracer
  , dtServerTracer                               = nullTracer
  , dtInboundGovernorTracer                      = nullTracer
  , dtLocalMuxTracer                             = nullTracer
  , dtLocalHandshakeTracer                       = nullTracer
  , dtLocalConnectionManagerTracer               = nullTracer
  , dtLocalServerTracer                          = nullTracer
  , dtLocalInboundGovernorTracer                 = nullTracer
  , dtDiffusionInitializationTracer              = nullTracer
  , dtLedgerPeersTracer                          = nullTracer
  }

-- | Network Node argumets
--
data DiffusionArguments = DiffusionArguments {
      daIPv4Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
    , daIPv6Address  :: Maybe (Either Socket.Socket AddrInfo)
      -- ^ an @IPV4@ socket ready to accept connections or an @IPv6@ addresses
    , daLocalAddress :: Maybe (Either Socket.Socket FilePath)
      -- ^ an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
      -- socket path
    , daPeerSelectionTargets :: PeerSelectionTargets
      -- ^ selection targets for the peer governor

    , daStaticLocalRootPeers :: [(Socket.SockAddr, PeerAdvertise)]
    , daLocalRootPeers       :: [(DomainAddress, PeerAdvertise)]
    , daPublicRootPeers      :: [DomainAddress]
    , daUseLedgerAfter       :: UseLedgerAfter

    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit
      -- ^ parameters for limiting number of accepted connections
    , daDiffusionMode :: DiffusionMode
      -- ^ run in initiator only mode

    , daProtocolIdleTimeout :: DiffTime
      -- ^ Timeout which starts once all responder protocols are idle. If the
      -- responders stay idle for duration of the timeout, the connection will
      -- be demoted, if it wasn't used by the p2p-governor it will be closed.
      --
      -- Applies to 'Unidirectional' as well as 'Duplex' /node-to-node/
      -- connections.
      --
      -- See 'serverProtocolIdleTimeout'.

    , daTimeWaitTimeout :: DiffTime
      -- ^ Time for which /node-to-node/ connections are kept in
      -- 'TerminatingState', it should correspond to the OS configured @TCP@
      -- @TIME_WAIT@ timeout.
      --
      -- This timeout will apply to after a connection has been closed, its
      -- purpose is to be resilitent for delayed packets in the same way @TCP@
      -- is using @TIME_WAIT@.
    }


--
-- Constants
--

-- | Protocol inactivity timeout for local (e.g. /node-to-client/) connections.
--
local_PROTOCOL_IDLE_TIMEOUT :: DiffTime
local_PROTOCOL_IDLE_TIMEOUT = 2 -- 2 seconds

-- | Used to set 'cmWaitTimeout' for local (e.g. /node-to-client/) connections.
--
local_TIME_WAIT_TIMEOUT :: DiffTime
local_TIME_WAIT_TIMEOUT = 0


socketAddressType :: Socket.SockAddr -> Maybe AddressType
socketAddressType Socket.SockAddrInet {}  = Just IPv4Address
socketAddressType Socket.SockAddrInet6 {} = Just IPv6Address
socketAddressType addr                    = error ("socketAddressType: unexpected address " ++ show addr)


-- | Combine two uni-directional 'MiniProtocolBundle's into one bi-directional
-- one.
--
combineMiniProtocolBundles :: MiniProtocolBundle InitiatorMode
                           -> MiniProtocolBundle ResponderMode
                           -> MiniProtocolBundle InitiatorResponderMode
combineMiniProtocolBundles (MiniProtocolBundle initiators)
                           (MiniProtocolBundle responders)
    = MiniProtocolBundle $
         [ MiniProtocolInfo { miniProtocolNum, miniProtocolLimits, miniProtocolDir = InitiatorDirection }
         | MiniProtocolInfo { miniProtocolNum, miniProtocolLimits } <- initiators
         ]
      ++ [ MiniProtocolInfo { miniProtocolNum, miniProtocolLimits, miniProtocolDir = ResponderDirection }
         | MiniProtocolInfo { miniProtocolNum, miniProtocolLimits } <- responders
         ]


-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data DiffusionApplications ntnAddr ntcAddr ntnVersionData ntcVersionData m =
    DiffusionApplications {

      -- | NodeToNode initiator applications for initiator only mode.
      --
      -- TODO: we should accept one or the other, but not both:
      -- 'daApplicationInitiatorMode', 'daApplicationInitiatorResponderMode'.
      --
      daApplicationInitiatorMode
        :: Versions NodeToNodeVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorMode ntnAddr
                      ByteString m () Void)

      -- | NodeToNode initiator & responder applications for bidirectional mode.
      --
    , daApplicationInitiatorResponderMode
        :: Versions NodeToNodeVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorResponderMode ntnAddr
                      ByteString m () ())


    -- | NodeToClient responder application (server role)
    --
    , daLocalResponderApplication
        :: Versions NodeToClientVersion
                    ntcVersionData
                    (OuroborosApplication
                      ResponderMode ntcAddr
                      ByteString m Void ())
    -- | configuration of mini-protocol parameters; they impact size limits of
    -- mux ingress queues.
    --
    , daMiniProtocolParameters :: MiniProtocolParameters

    -- | /node-to-node/ rethrow policy
    --
    , daRethrowPolicy      :: RethrowPolicy

    -- | /node-to-client/ rethrow policy
    --
    , daLocalRethrowPolicy :: RethrowPolicy

    , daLedgerPeersCtx :: LedgerPeersConsensusInterface m
      -- ^ Interface used to get peers from the current ledger.
    , daBlockFetchMode :: STM m FetchMode
      -- ^ Used by churn-governor
    }


-- TODO: add a tracer for these misconfiguration
data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
                      | UnexpectedIPv4Address
                      | UnexpectedIPv6Address
                      | UnexpectedUnixAddress
                      | NoSocket
  deriving (Eq, Show)

instance Exception DiffusionFailure


-- | Diffusion will always run initiator of node-to-node protocols, but in some
-- configurations, i.e. 'InitiatorOnlyDiffusionMode', it will not run the
-- responder side.  This type allows to reflect this.
--
-- This is only used internally by 'runDataDiffusion'; This type allows to
-- construct configuration upfront, before all services like connection manager
-- or server are initialised \/ started.
--
-- This is an existential wrapper for the higher order type @f :: MuxMode ->
-- Type@, like @'ConnectionManagerDataInMode' (mode :: MuxMode)@ below.
--
data HasMuxMode (f :: MuxMode -> Type) where
    HasInitiator :: !(f InitiatorMode)
                 -> HasMuxMode f

    HasInitiatorResponder
                 :: !(f InitiatorResponderMode)
                 -> HasMuxMode f

-- | Node-To-Node connection manager requires extra data when running in
-- 'InitiatorResponderMode'.
--
data ConnectionManagerDataInMode (mode :: MuxMode) where
    CMDInInitiatorMode
      :: ConnectionManagerDataInMode InitiatorMode

    CMDInInitiatorResponderMode
      :: Server.ControlChannel IO
          (Server.NewConnection
            SockAddr
            (Handle InitiatorResponderMode SockAddr ByteString IO () ()))
      -> StrictTVar IO Server.InboundGovernorObservableState
      -> ConnectionManagerDataInMode InitiatorResponderMode


--
-- Node-To-Client type aliases
--
-- Node-To-Client diffusion is only used in 'ResponderMode'.
--

type NodeToClientHandle =
    Handle ResponderMode LocalAddress ByteString IO Void ()

type NodeToClientHandleError =
    HandleError ResponderMode NodeToClientVersion

type NodeToClientConnectionHandler =
    ConnectionHandler
      ResponderMode
      (ConnectionHandlerTrace NodeToClientVersion NodeToClientVersionData)
      LocalSocket
      LocalAddress
      NodeToClientHandle
      NodeToClientHandleError
      (NodeToClientVersion, NodeToClientVersionData)
      IO

type NodeToClientConnectionManagerArguments =
    ConnectionManagerArguments
      (ConnectionHandlerTrace NodeToClientVersion NodeToClientVersionData)
      LocalSocket
      LocalAddress
      NodeToClientHandle
      NodeToClientHandleError
      (NodeToClientVersion, NodeToClientVersionData)
      IO

type NodeToClientConnectionManager =
    ConnectionManager
      ResponderMode
      LocalSocket
      LocalAddress
      NodeToClientHandle
      NodeToClientHandleError
      IO

--
-- Node-To-Node type aliases
--
-- Node-To-Node diffusion runs in either 'InitiatorMode' or 'InitiatorResponderMode'.
--

type NodeToNodeHandle (mode :: MuxMode) a =
    Handle mode SockAddr ByteString IO () a

type NodeToNodeHandleError (mode :: MuxMode) =
    HandleError mode NodeToNodeVersion

type NodeToNodeConnectionHandler (mode :: MuxMode) a =
    ConnectionHandler
      mode
     (ConnectionHandlerTrace NodeToNodeVersion NodeToNodeVersionData)
     Socket
     SockAddr
      (NodeToNodeHandle mode a)
      (NodeToNodeHandleError mode)
     (NodeToNodeVersion, NodeToNodeVersionData)
     IO

type NodeToNodeConnectionManagerArguments (mode :: MuxMode) a =
    ConnectionManagerArguments
      (ConnectionHandlerTrace NodeToNodeVersion NodeToNodeVersionData)
      Socket
      SockAddr
      (NodeToNodeHandle mode a)
      (NodeToNodeHandleError mode)
      (NodeToNodeVersion, NodeToNodeVersionData)
      IO

type NodeToNodeConnectionManager (mode :: MuxMode) a =
    ConnectionManager
      mode
      Socket
      SockAddr
      (NodeToNodeHandle mode a)
      (NodeToNodeHandleError mode)
      IO

--
-- Governor type aliases
--

type NodeToNodePeerConnectionHandle (mode :: MuxMode) a =
    PeerConnectionHandle
      mode
      SockAddr
      ByteString
      IO () a

type NodeToNodePeerStateActions (mode :: MuxMode) a =
    Governor.PeerStateActions
      SockAddr
      (NodeToNodePeerConnectionHandle mode a)
      IO

type NodeToNodePeerSelectionActions (mode :: MuxMode) a =
    Governor.PeerSelectionActions
      SockAddr
      (NodeToNodePeerConnectionHandle mode a)
      IO


-- | Main entry point for data diffusion service.  It allows to:
--
-- * connect to upstream peers;
-- * accept connection from downstream peers, if run in
--  'InitiatorAndResponderDiffusionMode'.
-- * runs a local service which allows to use node-to-client protocol to obtain
--   information from the running system.  This is used by 'cardano-cli' or
--   a wallet and a like local services.
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
                                    , daUseLedgerAfter
                                    , daAcceptedConnectionsLimit
                                    , daDiffusionMode
                                    , daProtocolIdleTimeout
                                    , daTimeWaitTimeout
                                    }
                 DiffusionApplications { daApplicationInitiatorMode
                                       , daApplicationInitiatorResponderMode
                                       , daLocalResponderApplication
                                       , daRethrowPolicy
                                       , daMiniProtocolParameters
                                       , daLocalRethrowPolicy
                                       , daLedgerPeersCtx
                                       , daBlockFetchMode
                                       } =
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
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

    -- control channel for the server; only required in
    -- @'InitiatorResponderMode' :: 'MuxMode'@
    cmdInMode
      <- case daDiffusionMode of
          InitiatorOnlyDiffusionMode ->
            -- action which we pass to connection handler
            pure (HasInitiator CMDInInitiatorMode)
          InitiatorAndResponderDiffusionMode -> do
            -- we pass 'Server.newOutboundConnection serverControlChannel' to
            -- connection handler
            HasInitiatorResponder <$>
              (CMDInInitiatorResponderMode
                <$> Server.newControlChannel
                <*> Server.newObservableStateVarIO)

    localControlChannel <- Server.newControlChannel
    localServerStateVar <- Server.newObservableStateVarIO

    -- RNGs used for picking random peers from the ledger and for
    -- demoting/promoting peers.
    rng <- newStdGen
    let (ledgerPeersRng, rng') = split rng
        (policyRng, churnRng)  = split rng'
    policyRngVar <- newTVarIO policyRng

    -- Request interface, supply the number of peers desired.
    ledgerPeersReq <- newEmptyTMVarIO :: IO (StrictTMVar IO NumberOfPeers)
    -- Response interface, returns a Set of peers. Nothing indicates that the
    -- ledger hasn't caught up to `useLedgerAfter`. May return less than
    -- the number of peers requested.
    ledgerPeersRsp <- newEmptyTMVarIO :: IO (StrictTMVar IO (Maybe (Set SockAddr, DiffTime)))


    peerSelectionTargetsVar <- newTVarIO $ daPeerSelectionTargets {
        -- Start with a smaller number of active peers, the churn governor will increase
        -- it to the configured value after a delay.
        targetNumberOfActivePeers = min 2 (targetNumberOfActivePeers daPeerSelectionTargets)
      }

    daLocalRootPeersVar <- newTVarIO $
                            ([( 1
                              , Map.fromList $
                                  map (\(d,p) -> (RelayDomain d, p))
                                      daLocalRootPeers)])
    -- ^ TODO: This is just a simple transformation
    daPublicRootPeersVar <- newTVarIO $ map RelayDomain daPublicRootPeers
    daUseLedgerAfterVar <- newTVarIO daUseLedgerAfter

    let -- snocket for remote communication.
        snocket :: SocketSnocket
        snocket = Snocket.socketSnocket iocp

        localConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0

        --
        -- local connection manager
        --
        localThread :: Maybe (IO Void)
        localThread =
          case daLocalAddress of
            Nothing -> Nothing
            Just localAddr ->
               Just $ withLocalSocket iocp tracer localAddr
                       $ \localSnocket localSocket -> do
                let localConnectionHandler :: NodeToClientConnectionHandler
                    localConnectionHandler =
                      makeConnectionHandler
                        dtLocalMuxTracer
                        SingResponderMode
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
                            haAcceptVersion = acceptableVersion,
                            haTimeLimits = noTimeLimitsHandshake
                          }
                        (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

                    localConnectionManagerArguments :: NodeToClientConnectionManagerArguments
                    localConnectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtLocalConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtLocalMuxTracer,
                          cmIPv4Address         = Nothing,
                          cmIPv6Address         = Nothing,
                          cmAddressType         = const Nothing,
                          cmSnocket             = localSnocket,
                          connectionDataFlow    = uncurry localDataFlow,
                          cmPrunePolicy         = Server.randomPrunePolicy localServerStateVar,
                          cmConnectionsLimits   = localConnectionLimits,
                          cmTimeWaitTimeout     = local_TIME_WAIT_TIMEOUT
                        }

                withConnectionManager
                  localConnectionManagerArguments
                  localConnectionHandler
                  classifyHandleError
                  (InResponderMode localControlChannel)
                  $ \(localConnectionManager :: NodeToClientConnectionManager) -> do

                  --
                  -- run local server
                  --

                  traceWith tracer . RunLocalServer
                    =<< Snocket.getLocalAddr localSnocket localSocket

                  Async.withAsync
                    (Server.run
                      ServerArguments {
                          serverSockets               = localSocket :| [],
                          serverSnocket               = localSnocket,
                          serverTracer                = dtLocalServerTracer,
                          serverInboundGovernorTracer = dtLocalInboundGovernorTracer,
                          serverControlChannel        = localControlChannel,
                          serverConnectionLimits      = localConnectionLimits,
                          serverConnectionManager     = localConnectionManager,
                          serverObservableStateVar    = localServerStateVar,
                          serverProtocolIdleTimeout   = local_PROTOCOL_IDLE_TIMEOUT
                        }) Async.wait

        --
        -- remote connection manager
        --

        remoteThread :: IO Void
        remoteThread =
          Async.withAsync
            (runLedgerPeers
              ledgerPeersRng
              dtLedgerPeersTracer
              daUseLedgerAfterVar
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
            case cmdInMode of
              -- InitiatorOnlyMode
              --
              -- Run peer selection only
              HasInitiator CMDInInitiatorMode -> do
                let connectionManagerArguments :: NodeToNodeConnectionManagerArguments InitiatorMode Void
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = socketAddressType,
                          cmSnocket             = snocket,
                          connectionDataFlow    = uncurry nodeDataFlow,
                          cmPrunePolicy         =
                            case cmdInMode of
                              HasInitiator CMDInInitiatorMode ->
                                -- Server is not running, it will not be able to
                                -- advise which connections to prune.  It's also not
                                -- expected that the governor targets will be larger
                                -- than limits imposed by 'cmConnectionsLimits'.
                                simplePrunePolicy,
                          cmConnectionsLimits   = daAcceptedConnectionsLimit,
                          cmTimeWaitTimeout     = daTimeWaitTimeout
                        }

                    connectionHandler :: NodeToNodeConnectionHandler InitiatorMode Void
                    connectionHandler =
                      makeConnectionHandler
                        dtMuxTracer
                        SingInitiatorMode
                        miniProtocolBundleInitiatorMode
                        HandshakeArguments {
                            haHandshakeTracer = dtHandshakeTracer,
                            haHandshakeCodec = nodeToNodeHandshakeCodec,
                            haVersionDataCodec = cborTermVersionDataCodec NodeToNode.nodeToNodeCodecCBORTerm,
                            haVersions = daApplicationInitiatorMode,
                            haAcceptVersion = acceptableVersion,
                            haTimeLimits = timeLimitsHandshake
                          }
                        (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  NotInResponderMode
                  $ \(connectionManager :: NodeToNodeConnectionManager InitiatorMode Void) ->
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
                    $ \(peerStateActions :: NodeToNodePeerStateActions InitiatorMode Void) ->
                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      timeout
                      (readTVar peerSelectionTargetsVar)
                      (Map.fromList daStaticLocalRootPeers)
                      daLocalRootPeersVar
                      daPublicRootPeersVar
                      peerStateActions
                      (putTMVar ledgerPeersReq)
                      (takeTMVar ledgerPeersRsp)
                      $ \mbLocalPeerRootProviderThread
                         (peerSelectionActions
                            :: NodeToNodePeerSelectionActions
                                 InitiatorMode Void) ->

                        Async.withAsync
                          (Governor.peerSelectionGovernor
                            dtTracePeerSelectionTracer
                            dtDebugPeerSelectionInitiatorTracer
                            dtTracePeerSelectionCounters
                            peerSelectionActions
                            (Diffusion.Policies.simplePeerSelectionPolicy policyRngVar))
                          $ \governorThread ->
                            Async.withAsync
                              (Governor.peerChurnGovernor
                                dtTracePeerSelectionTracer
                                churnRng
                                daBlockFetchMode
                                daPeerSelectionTargets
                                peerSelectionTargetsVar)
                              $ \churnGovernorThread ->

                                -- wait for any thread to fail
                                snd <$> Async.waitAny
                                  (maybeToList mbLocalPeerRootProviderThread
                                  ++ [ governorThread
                                     , ledgerPeerThread
                                     , churnGovernorThread
                                     ])


              -- InitiatorResponderMode
              --
              -- Run peer selection and the server.
              --
              HasInitiatorResponder (CMDInInitiatorResponderMode controlChannel observableStateVar) -> do
                let connectionManagerArguments :: NodeToNodeConnectionManagerArguments InitiatorResponderMode ()
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = socketAddressType,
                          cmSnocket             = snocket,
                          connectionDataFlow    = uncurry nodeDataFlow,
                          cmPrunePolicy         =
                            case cmdInMode of
                              HasInitiatorResponder (CMDInInitiatorResponderMode _ serverStateVar) ->
                                Server.randomPrunePolicy serverStateVar,
                          cmConnectionsLimits   = daAcceptedConnectionsLimit,
                          cmTimeWaitTimeout     = daTimeWaitTimeout
                        }

                    connectionHandler :: NodeToNodeConnectionHandler InitiatorResponderMode ()
                    connectionHandler =
                      makeConnectionHandler
                         dtMuxTracer
                         SingInitiatorResponderMode
                         miniProtocolBundleInitiatorResponderMode
                         HandshakeArguments {
                             haHandshakeTracer = dtHandshakeTracer,
                             haHandshakeCodec = nodeToNodeHandshakeCodec,
                             haVersionDataCodec = cborTermVersionDataCodec NodeToNode.nodeToNodeCodecCBORTerm,
                             haVersions = daApplicationInitiatorResponderMode,
                             haAcceptVersion = acceptableVersion,
                             haTimeLimits = timeLimitsHandshake
                           }
                         (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  (InResponderMode controlChannel)
                  $ \(connectionManager :: NodeToNodeConnectionManager InitiatorResponderMode ()) ->
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
                    $ \(peerStateActions
                          :: NodeToNodePeerStateActions
                               InitiatorResponderMode ()) ->

                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      timeout
                      (readTVar peerSelectionTargetsVar)
                      (Map.fromList daStaticLocalRootPeers)
                      daLocalRootPeersVar
                      daPublicRootPeersVar
                      peerStateActions
                      (putTMVar ledgerPeersReq)
                      (takeTMVar ledgerPeersRsp)
                      $ \mbLocalPeerRootProviderThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorResponderMode ()) ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorResponderTracer
                          dtTracePeerSelectionCounters
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy policyRngVar))
                        $ \governorThread -> do
                        let mkAddr :: AddrInfo -> (Socket.Family, SockAddr)
                            mkAddr addr = ( Socket.addrFamily  addr
                                          , Socket.addrAddress addr
                                          )

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
                                  serverSockets               = sockets,
                                  serverSnocket               = snocket,
                                  serverTracer                = dtServerTracer,
                                  serverInboundGovernorTracer = dtInboundGovernorTracer,
                                  serverControlChannel        = controlChannel,
                                  serverConnectionLimits      = daAcceptedConnectionsLimit,
                                  serverConnectionManager     = connectionManager,
                                  serverProtocolIdleTimeout   = daProtocolIdleTimeout,
                                  serverObservableStateVar    = observableStateVar
                                })
                                $ \serverThread ->
                                  Async.withAsync
                                    (Governor.peerChurnGovernor
                                      dtTracePeerSelectionTracer
                                      churnRng
                                      daBlockFetchMode
                                      daPeerSelectionTargets
                                      peerSelectionTargetsVar)
                                    $ \churnGovernorThread ->

                                      -- wait for any thread to fail
                                      snd <$> Async.waitAny
                                        (maybeToList mbLocalPeerRootProviderThread
                                        ++ [ serverThread
                                           , governorThread
                                           , ledgerPeerThread
                                           , churnGovernorThread
                                           ])

    Async.runConcurrently
      $ asum
      $ Async.Concurrently <$>
          ( remoteThread
          : maybeToList localThread
          )

  where
    DiffusionTracers { dtMuxTracer
                     , dtHandshakeTracer
                     , dtTracePeerSelectionTracer
                     , dtDebugPeerSelectionInitiatorTracer
                     , dtDebugPeerSelectionInitiatorResponderTracer
                     , dtTracePeerSelectionCounters
                     , dtPeerSelectionActionsTracer
                     , dtTraceLocalRootPeersTracer
                     , dtTracePublicRootPeersTracer
                     , dtConnectionManagerTracer
                     , dtServerTracer
                     , dtInboundGovernorTracer
                     , dtLocalMuxTracer
                     , dtLocalHandshakeTracer
                     , dtLocalConnectionManagerTracer
                     , dtLocalServerTracer
                     , dtLocalInboundGovernorTracer
                     , dtLedgerPeersTracer
                     -- the tracer
                     , dtDiffusionInitializationTracer = tracer
                     } = tracers


    miniProtocolBundleInitiatorResponderMode :: MiniProtocolBundle InitiatorResponderMode
    miniProtocolBundleInitiatorResponderMode =
      combineMiniProtocolBundles miniProtocolBundleInitiatorMode
                                 miniProtocolBundleResponderMode

    -- node-to-node responder bundle; it is only used in combination with
    -- the node-to-node initiator bundle defined below.
    --
    miniProtocolBundleResponderMode :: MiniProtocolBundle ResponderMode
    miniProtocolBundleResponderMode = MiniProtocolBundle
      [ MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolDir    = ResponderDirectionOnly,
          miniProtocolLimits = chainSyncProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 3,
          miniProtocolDir    = ResponderDirectionOnly,
          miniProtocolLimits = blockFetchProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 4,
          miniProtocolDir    = ResponderDirectionOnly,
          miniProtocolLimits = txSubmissionProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 8,
          miniProtocolDir    = ResponderDirectionOnly,
          miniProtocolLimits = keepAliveProtocolLimits daMiniProtocolParameters
        }
      -- TODO: `tip-sample` protocol
      ]

    -- node-to-node initiator bundle
    miniProtocolBundleInitiatorMode :: MiniProtocolBundle InitiatorMode
    miniProtocolBundleInitiatorMode = MiniProtocolBundle
      [ MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 2,
          miniProtocolDir    = InitiatorDirectionOnly,
          miniProtocolLimits = chainSyncProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 3,
          miniProtocolDir    = InitiatorDirectionOnly,
          miniProtocolLimits = blockFetchProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 4,
          miniProtocolDir    = InitiatorDirectionOnly,
          miniProtocolLimits = txSubmissionProtocolLimits daMiniProtocolParameters
        }
      , MiniProtocolInfo {
          miniProtocolNum    = MiniProtocolNum 8,
          miniProtocolDir    = InitiatorDirectionOnly,
          miniProtocolLimits = keepAliveProtocolLimits daMiniProtocolParameters
        }
      -- TODO: `tip-sample` protocol
      ]

    -- node-to-client protocol bundle
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

--
-- Data flow
--

-- | For Node-To-Node protocol, any connection which negotiated at least
-- 'NodeToNodeV_7' version and did not declared 'InitiatorOnlyDiffusionMode'
-- will run in 'Duplex' mode.   All connections from lower versions or one that
-- declared themselves as 'InitiatorOnly' will run in 'UnidirectionalMode'
--
nodeDataFlow :: NodeToNodeVersion
             -> NodeToNodeVersionData
             -> DataFlow
nodeDataFlow v NodeToNodeVersionData { diffusionMode = InitiatorAndResponderDiffusionMode }
                 | v >= NodeToNodeV_7
                 = Duplex
nodeDataFlow _ _ = Unidirectional


-- | For Node-To-Client protocol all connection are considered 'Unidrectional'.
--
localDataFlow :: NodeToClientVersion
              -> NodeToClientVersionData
              -> DataFlow
localDataFlow _ _ = Unidirectional


--
-- Socket utility functions
--

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
