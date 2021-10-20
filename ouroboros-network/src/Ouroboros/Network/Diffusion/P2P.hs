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

#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | This module is expected to be imported qualified (it will clash
-- with the "Ouroboros.Network.Diffusion.NonP2P").
--
module Ouroboros.Network.Diffusion.P2P
  ( TracersExtra (..)
  , nullTracers
  , ArgumentsExtra (..)
  , AcceptedConnectionsLimit (..)
  , ApplicationsExtra (..)
  , run

  , NodeToNodePeerConnectionHandle
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
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           System.Random (newStdGen, split)

import           Network.Mux
                  ( MiniProtocolBundle (..)
                  , MiniProtocolInfo (..)
                  , MiniProtocolDirection (..)
                  )
import qualified Network.DNS as DNS
import           Network.Socket (SockAddr (..), Socket, AddrInfo)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket
                  ( LocalAddress
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
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..))
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS
                  ( resolveDomainAccessPoint
                  , RelayAccessPoint(..)
                  , TraceLocalRootPeers(..)
                  , TracePublicRootPeers(..)
                  )
import           Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions
                  ( ioDNSActions
                  )
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import           Ouroboros.Network.PeerSelection.Governor.Types
                  ( TracePeerSelection (..)
                  , DebugPeerSelection (..)
                  , PeerSelectionCounters (..)
                  , ChurnMode (ChurnModeNormal)
                  )
import           Ouroboros.Network.PeerSelection.LedgerPeers
                  ( UseLedgerAfter (..)
                  , withLedgerPeers
                  )
import           Ouroboros.Network.PeerSelection.PeerStateActions
                  ( PeerSelectionActionsTrace (..)
                  , PeerStateActionsArguments (..)
                  , PeerConnectionHandle
                  , withPeerStateActions
                  )
import           Ouroboros.Network.PeerSelection.Simple
import           Ouroboros.Network.Server2
                  ( ServerArguments (..)
                  , ServerTrace (..)
                  )
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.Mux hiding (MiniProtocol (..))
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.NodeToClient
                  ( NodeToClientVersion (..)
                  , NodeToClientVersionData
                  )
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode
                  ( MiniProtocolParameters (..)
                  , NodeToNodeVersion (..)
                  , NodeToNodeVersionData (..)
                  , AcceptedConnectionsLimit (..)
                  , DiffusionMode (..)
                  , chainSyncProtocolLimits
                  , blockFetchProtocolLimits
                  , txSubmissionProtocolLimits
                  , keepAliveProtocolLimits
                  , nodeToNodeHandshakeCodec
                  )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Diffusion.Common hiding (nullTracers)

-- | P2P DiffusionTracers Extras
--
data TracersExtra = TracersExtra {
      dtTraceLocalRootPeersTracer
        :: Tracer IO (TraceLocalRootPeers IOException)

    , dtTracePublicRootPeersTracer
        :: Tracer IO TracePublicRootPeers

    , dtTracePeerSelectionTracer
        :: Tracer IO (TracePeerSelection SockAddr)

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer IO (DebugPeerSelection
                       SockAddr
                         (NodeToNodePeerConnectionHandle
                            InitiatorMode
                            Void))

    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer IO (DebugPeerSelection
                       SockAddr
                         (NodeToNodePeerConnectionHandle
                            InitiatorResponderMode
                            ()))

    , dtTracePeerSelectionCounters
        :: Tracer IO PeerSelectionCounters

    , dtPeerSelectionActionsTracer
        :: Tracer IO (PeerSelectionActionsTrace SockAddr)

    , dtConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       SockAddr
                       (ConnectionHandlerTrace
                          NodeToNodeVersion
                          NodeToNodeVersionData))

    , dtServerTracer
        :: Tracer IO (ServerTrace SockAddr)

    , dtInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace SockAddr)

      --
      -- NodeToClient tracers
      --

      -- | Connection manager tracer for local clients
    , dtLocalConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       LocalAddress
                       (ConnectionHandlerTrace
                          NodeToClientVersion
                          NodeToClientVersionData))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer IO (ServerTrace LocalAddress)

      -- | Inbound protocol governor tracer for local clients
    , dtLocalInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace LocalAddress)
    }

nullTracers :: TracersExtra
nullTracers = p2pNullTracers
  where
  p2pNullTracers =
    TracersExtra {
        dtTraceLocalRootPeersTracer                  = nullTracer
      , dtTracePublicRootPeersTracer                 = nullTracer
      , dtTracePeerSelectionTracer                   = nullTracer
      , dtDebugPeerSelectionInitiatorTracer          = nullTracer
      , dtDebugPeerSelectionInitiatorResponderTracer = nullTracer
      , dtTracePeerSelectionCounters                 = nullTracer
      , dtPeerSelectionActionsTracer                 = nullTracer
      , dtConnectionManagerTracer                    = nullTracer
      , dtServerTracer                               = nullTracer
      , dtInboundGovernorTracer                      = nullTracer
      , dtLocalConnectionManagerTracer               = nullTracer
      , dtLocalServerTracer                          = nullTracer
      , dtLocalInboundGovernorTracer                 = nullTracer
    }

-- | P2P DiffusionArguments Extras
--
data ArgumentsExtra = ArgumentsExtra {
      -- | selection targets for the peer governor
      --
      daPeerSelectionTargets :: PeerSelectionTargets

    , daReadLocalRootPeers  :: STM IO [(Int, Map RelayAccessPoint PeerAdvertise)]
    , daReadPublicRootPeers :: STM IO [RelayAccessPoint]
    , daReadUseLedgerAfter  :: STM IO UseLedgerAfter

      -- | Timeout which starts once all responder protocols are idle. If the
      -- responders stay idle for duration of the timeout, the connection will
      -- be demoted, if it wasn't used by the p2p-governor it will be closed.
      --
      -- Applies to 'Unidirectional' as well as 'Duplex' /node-to-node/
      -- connections.
      --
      -- See 'serverProtocolIdleTimeout'.
      --
    , daProtocolIdleTimeout :: DiffTime

      -- | Time for which /node-to-node/ connections are kept in
      -- 'TerminatingState', it should correspond to the OS configured @TCP@
      -- @TIME_WAIT@ timeout.
      --
      -- This timeout will apply to after a connection has been closed, its
      -- purpose is to be resilient for delayed packets in the same way @TCP@
      -- is using @TIME_WAIT@.
      --
    , daTimeWaitTimeout :: DiffTime
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
socketAddressType addr                    =
  error ("socketAddressType: unexpected address " ++ show addr)


-- | Combine two uni-directional 'MiniProtocolBundle's into one bi-directional
-- one.
--
combineMiniProtocolBundles :: MiniProtocolBundle InitiatorMode
                           -> MiniProtocolBundle ResponderMode
                           -> MiniProtocolBundle InitiatorResponderMode
combineMiniProtocolBundles (MiniProtocolBundle initiators)
                           (MiniProtocolBundle responders)
    = MiniProtocolBundle $
         [ MiniProtocolInfo
            { miniProtocolNum
            , miniProtocolLimits
            , miniProtocolDir = InitiatorDirection
            }
         | MiniProtocolInfo { miniProtocolNum, miniProtocolLimits } <- initiators
         ]
      ++ [ MiniProtocolInfo
            { miniProtocolNum
            , miniProtocolLimits
            , miniProtocolDir = ResponderDirection
            }
         | MiniProtocolInfo { miniProtocolNum, miniProtocolLimits } <- responders
         ]


-- | P2P DiffusionApplications Extras
--
-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data ApplicationsExtra =
    ApplicationsExtra {
    -- | configuration of mini-protocol parameters; they impact size limits of
    -- mux ingress queues.
    --
      daMiniProtocolParameters :: MiniProtocolParameters

    -- | /node-to-node/ rethrow policy
    --
    , daRethrowPolicy          :: RethrowPolicy

    -- | /node-to-client/ rethrow policy
    --
    , daLocalRethrowPolicy     :: RethrowPolicy

    -- | 'PeerMetrics' used by peer selection policy (see
    -- 'simplePeerSelectionPolicy')
    --
    , daPeerMetrics            :: PeerMetrics IO SockAddr

    -- | Used by churn-governor
    --
    , daBlockFetchMode         :: STM IO FetchMode
  }

-- | Diffusion will always run initiator of node-to-node protocols, but in some
-- configurations, i.e. 'InitiatorOnlyDiffusionMode', it will not run the
-- responder side.  This type allows to reflect this.
--
-- This is only used internally by 'run'; This type allows to
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
run
    :: Tracers
    -> TracersExtra
    -> Arguments
    -> ArgumentsExtra
    -> Applications
    -> ApplicationsExtra
    -> IO Void
run tracers
    tracersExtra
    Arguments
      { daIPv4Address
      , daIPv6Address
      , daLocalAddress
      , daAcceptedConnectionsLimit
      , daMode = diffusionMode
      }
    ArgumentsExtra
      { daPeerSelectionTargets
      , daReadLocalRootPeers
      , daReadPublicRootPeers
      , daReadUseLedgerAfter
      , daProtocolIdleTimeout
      , daTimeWaitTimeout
      }
    Applications
      { daApplicationInitiatorMode
      , daApplicationInitiatorResponderMode
      , daLocalResponderApplication
      , daLedgerPeersCtx
      }
    ApplicationsExtra
        { daMiniProtocolParameters
        , daRethrowPolicy
        , daLocalRethrowPolicy
        , daPeerMetrics
        , daBlockFetchMode
        } =
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handle (\e -> traceWith tracer (DiffusionErrored e)
               >> throwIO e) $
    withIOManager $ \iocp -> do

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
      <- case diffusionMode of
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
        (policyRng, rng'')  = split rng'
        (churnRng, fuzzRng) = split rng''
    policyRngVar <- newTVarIO policyRng

    churnModeVar <- newTVarIO ChurnModeNormal

    peerSelectionTargetsVar <- newTVarIO $ daPeerSelectionTargets {
        -- Start with a smaller number of active peers, the churn governor will increase
        -- it to the configured value after a delay.
        targetNumberOfActivePeers =
          min 2 (targetNumberOfActivePeers daPeerSelectionTargets)
      }

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
                            haHandshakeCodec =
                              NodeToClient.nodeToClientHandshakeCodec,
                            haVersionDataCodec =
                              cborTermVersionDataCodec
                                NodeToClient.nodeToClientCodecCBORTerm,
                            haAcceptVersion = acceptableVersion,
                            haTimeLimits = noTimeLimitsHandshake
                          }
                        (     (\(OuroborosApplication apps)
                                -> Bundle
                                    (WithHot apps)
                                    (WithWarm (\_ _ -> []))
                                    (WithEstablished (\_ _ -> [])))
                          <$> daLocalResponderApplication)
                        (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

                    localConnectionManagerArguments
                      :: NodeToClientConnectionManagerArguments
                    localConnectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtLocalConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtLocalMuxTracer,
                          cmIPv4Address         = Nothing,
                          cmIPv6Address         = Nothing,
                          cmAddressType         = const Nothing,
                          cmSnocket             = localSnocket,
                          cmTimeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                          cmOutboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                          connectionDataFlow    = uncurry localDataFlow,
                          cmPrunePolicy         = Server.randomPrunePolicy
                                                    localServerStateVar,
                          cmConnectionsLimits   = localConnectionLimits
                        }

                withConnectionManager
                  localConnectionManagerArguments
                  localConnectionHandler
                  classifyHandleError
                  (InResponderMode localControlChannel)
                  $ \(localConnectionManager :: NodeToClientConnectionManager)
                    -> do

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
                          serverInboundIdleTimeout    = local_PROTOCOL_IDLE_TIMEOUT,
                          serverConnectionLimits      = localConnectionLimits,
                          serverConnectionManager     = localConnectionManager,
                          serverControlChannel        = localControlChannel,
                          serverObservableStateVar    = localServerStateVar
                        }) Async.wait

        --
        -- remote connection manager
        --

        remoteThread :: IO Void
        remoteThread =
          withLedgerPeers
            ledgerPeersRng
            dtLedgerPeersTracer
            daReadUseLedgerAfter
            daLedgerPeersCtx
            (resolveDomainAccessPoint
              dtTracePublicRootPeersTracer
              DNS.defaultResolvConf
              ioDNSActions)
            $ \requestLedgerPeers ledgerPeerThread ->
            case cmdInMode of
              -- InitiatorOnlyMode
              --
              -- Run peer selection only
              HasInitiator CMDInInitiatorMode -> do
                let connectionManagerArguments
                      :: NodeToNodeConnectionManagerArguments InitiatorMode Void
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
                          cmTimeWaitTimeout     = daTimeWaitTimeout,
                          cmOutboundIdleTimeout = daProtocolIdleTimeout
                        }

                    connectionHandler
                      :: NodeToNodeConnectionHandler InitiatorMode Void
                    connectionHandler =
                      makeConnectionHandler
                        dtMuxTracer
                        SingInitiatorMode
                        miniProtocolBundleInitiatorMode
                        HandshakeArguments {
                            haHandshakeTracer = dtHandshakeTracer,
                            haHandshakeCodec = nodeToNodeHandshakeCodec,
                            haVersionDataCodec =
                              cborTermVersionDataCodec
                                NodeToNode.nodeToNodeCodecCBORTerm,
                            haAcceptVersion = acceptableVersion,
                            haTimeLimits = timeLimitsHandshake
                          }
                        daApplicationInitiatorMode
                        (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  NotInResponderMode
                  $ \(connectionManager
                      :: NodeToNodeConnectionManager InitiatorMode Void) -> do

                  --
                  -- peer state actions
                  --
                  -- Peer state actions run a job pool in the background which
                  -- tracks threads forked by 'PeerStateActions'
                  --

                  withPeerStateActions
                    PeerStateActionsArguments {
                        spsTracer = dtPeerSelectionActionsTracer,
                        spsDeactivateTimeout = Diffusion.Policies.deactivateTimeout,
                        spsCloseConnectionTimeout =
                          Diffusion.Policies.closeConnectionTimeout,
                        spsConnectionManager = connectionManager
                      }
                    $ \(peerStateActions
                          :: NodeToNodePeerStateActions InitiatorMode Void) ->
                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerSelectionActionsThread
                         (peerSelectionActions
                            :: NodeToNodePeerSelectionActions
                                 InitiatorMode Void) ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorTracer
                          dtTracePeerSelectionCounters
                          fuzzRng
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy
                            policyRngVar (readTVar churnModeVar) daPeerMetrics))
                        $ \governorThread ->
                          Async.withAsync
                            (Governor.peerChurnGovernor
                              dtTracePeerSelectionTracer
                              daPeerMetrics
                              churnModeVar
                              churnRng
                              daBlockFetchMode
                              daPeerSelectionTargets
                              peerSelectionTargetsVar)
                            $ \churnGovernorThread ->

                              -- wait for any thread to fail
                              snd <$> Async.waitAny
                                (maybeToList mbLocalPeerSelectionActionsThread
                                ++ [ governorThread
                                   , ledgerPeerThread
                                   , churnGovernorThread
                                   ])


              -- InitiatorResponderMode
              --
              -- Run peer selection and the server.
              --
              HasInitiatorResponder
                (CMDInInitiatorResponderMode controlChannel observableStateVar) -> do
                let connectionManagerArguments
                      :: NodeToNodeConnectionManagerArguments
                          InitiatorResponderMode
                          ()
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
                          cmTimeWaitTimeout     = daTimeWaitTimeout,
                          cmOutboundIdleTimeout = daProtocolIdleTimeout
                        }

                    connectionHandler
                      :: NodeToNodeConnectionHandler
                          InitiatorResponderMode
                          ()
                    connectionHandler =
                      makeConnectionHandler
                         dtMuxTracer
                         SingInitiatorResponderMode
                         miniProtocolBundleInitiatorResponderMode
                         HandshakeArguments {
                             haHandshakeTracer = dtHandshakeTracer,
                             haHandshakeCodec = nodeToNodeHandshakeCodec,
                             haVersionDataCodec =
                              cborTermVersionDataCodec
                                NodeToNode.nodeToNodeCodecCBORTerm,
                             haAcceptVersion = acceptableVersion,
                             haTimeLimits = timeLimitsHandshake
                           }
                         daApplicationInitiatorResponderMode
                         (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  (InResponderMode controlChannel)
                  $ \(connectionManager
                        :: NodeToNodeConnectionManager InitiatorResponderMode ()) -> do
                  --
                  -- peer state actions
                  --
                  -- Peer state actions run a job pool in the background which
                  -- tracks threads forked by 'PeerStateActions'
                  --

                  withPeerStateActions
                    PeerStateActionsArguments {
                        spsTracer = dtPeerSelectionActionsTracer,
                        spsDeactivateTimeout = Diffusion.Policies.deactivateTimeout,
                        spsCloseConnectionTimeout =
                          Diffusion.Policies.closeConnectionTimeout,
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
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerRootProviderThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorResponderMode ()) ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorResponderTracer
                          dtTracePeerSelectionCounters
                          fuzzRng
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy
                            policyRngVar (readTVar churnModeVar) daPeerMetrics))
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
                                  serverConnectionLimits      = daAcceptedConnectionsLimit,
                                  serverConnectionManager     = connectionManager,
                                  serverInboundIdleTimeout    = daProtocolIdleTimeout,
                                  serverControlChannel        = controlChannel,
                                  serverObservableStateVar    = observableStateVar
                                })
                                $ \serverThread ->
                                  Async.withAsync
                                    (Governor.peerChurnGovernor
                                      dtTracePeerSelectionTracer
                                      daPeerMetrics
                                      churnModeVar
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
    Tracers {
        dtMuxTracer
      , dtLocalMuxTracer
      , dtHandshakeTracer
      , dtLocalHandshakeTracer
      , dtLedgerPeersTracer
      -- the tracer
      , dtDiffusionInitializationTracer = tracer
      } = tracers
    TracersExtra {
        dtTracePeerSelectionTracer
      , dtDebugPeerSelectionInitiatorTracer
      , dtDebugPeerSelectionInitiatorResponderTracer
      , dtTracePeerSelectionCounters
      , dtPeerSelectionActionsTracer
      , dtTraceLocalRootPeersTracer
      , dtTracePublicRootPeersTracer
      , dtConnectionManagerTracer
      , dtServerTracer
      , dtInboundGovernorTracer
      , dtLocalConnectionManagerTracer
      , dtLocalServerTracer
      , dtLocalInboundGovernorTracer
      } = tracersExtra


    miniProtocolBundleInitiatorResponderMode
      :: MiniProtocolBundle InitiatorResponderMode
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
-- 'NodeToNodeV_8' version and did not declared 'InitiatorOnlyDiffusionMode'
-- will run in 'Duplex' mode.   All connections from lower versions or one that
-- declared themselves as 'InitiatorOnly' will run in 'UnidirectionalMode'
--
nodeDataFlow :: NodeToNodeVersion
             -> NodeToNodeVersionData
             -> DataFlow
nodeDataFlow v NodeToNodeVersionData { diffusionMode = InitiatorAndResponderDiffusionMode }
                 | v >= NodeToNodeV_8
                 = Duplex
nodeDataFlow _ _ = Unidirectional


-- | For Node-To-Client protocol all connection are considered 'Unidirectional'.
--
localDataFlow :: NodeToClientVersion
              -> NodeToClientVersionData
              -> DataFlow
localDataFlow _ _ = Unidirectional


--
-- Socket utility functions
--

withSockets :: Tracer IO InitializationTracer
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
                -> Tracer IO InitializationTracer
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
                    return (Left ( Snocket.localSnocket iocp
                                 , LocalSocket sd
                                 ))
                  _  -> do
                    traceWith tracer $ UnsupportedLocalSystemdSocket addr
                    throwIO UnsupportedLocalSocketType
#endif
         Right addr -> do
             let sn :: LocalSnocket
                 sn = Snocket.localSnocket iocp
             traceWith tracer $ CreateSystemdSocketForSnocketPath addr
             sd <- Snocket.open sn (Snocket.LocalFamily (Snocket.LocalAddress addr))
             traceWith tracer $ CreatedLocalSocket addr
             return (Right (sn, sd, addr))
    )
    -- We close the socket here, even if it was provided to us.
    (\case
      Left  (sn, sd)    -> Snocket.close sn sd
      Right (sn, sd, _) -> Snocket.close sn sd
    )
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
