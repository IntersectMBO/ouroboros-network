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

-- `withLocalSocket` has some constraints that are only required on Windows.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Exception (IOException)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Foldable (asum)
import           Data.IP (IP)
import qualified Data.IP as IP
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.ByteString.Lazy (ByteString)
import           Data.Kind (Type)
import           System.Random (newStdGen, split)
#ifdef POSIX
import qualified System.Posix.Signals as Signals
#endif

import           Network.Mux
                  ( MiniProtocolBundle (..)
                  , MiniProtocolInfo (..)
                  , MiniProtocolDirection (..)
                  )
import qualified Network.DNS as DNS
import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket
                  ( LocalAddress
                  , LocalSnocket
                  , LocalSocket (..)
                  , FileDescriptor
                  , Snocket
                  , SocketSnocket
                  , localSocketFileDescriptor
                  )
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.ConnectionId
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
                  , DomainAccessPoint
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
                  , RemoteAddress
                  , chainSyncProtocolLimits
                  , blockFetchProtocolLimits
                  , txSubmissionProtocolLimits
                  , keepAliveProtocolLimits
                  )
import qualified Ouroboros.Network.NodeToNode   as NodeToNode
import           Ouroboros.Network.Diffusion.Common hiding (nullTracers)

-- | P2P DiffusionTracers Extras
--
data TracersExtra ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData =
    TracersExtra {
      dtTraceLocalRootPeersTracer
        :: Tracer IO (TraceLocalRootPeers ntnAddr IOException)

    , dtTracePublicRootPeersTracer
        :: Tracer IO TracePublicRootPeers

    , dtTracePeerSelectionTracer
        :: Tracer IO (TracePeerSelection ntnAddr)

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer IO (DebugPeerSelection
                       ntnAddr
                       (PeerConnectionHandle
                         InitiatorMode
                         ntnAddr
                         ByteString
                         IO () Void))

    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer IO (DebugPeerSelection
                       ntnAddr
                       (PeerConnectionHandle
                         InitiatorResponderMode
                         ntnAddr
                         ByteString
                         IO () ()))

    , dtTracePeerSelectionCounters
        :: Tracer IO PeerSelectionCounters

    , dtPeerSelectionActionsTracer
        :: Tracer IO (PeerSelectionActionsTrace ntnAddr)

    , dtConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                      ntnAddr
                      (ConnectionHandlerTrace
                         ntnVersion
                         ntnVersionData))

    , dtServerTracer
        :: Tracer IO (ServerTrace ntnAddr)

    , dtInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace ntnAddr)

      --
      -- NodeToClient tracers
      --

      -- | Connection manager tracer for local clients
    , dtLocalConnectionManagerTracer
        :: Tracer IO (ConnectionManagerTrace
                       ntcAddr
                       (ConnectionHandlerTrace
                          ntcVersion
                          ntcVersionData))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer IO (ServerTrace ntcAddr)

      -- | Inbound protocol governor tracer for local clients
    , dtLocalInboundGovernorTracer
        :: Tracer IO (InboundGovernorTrace ntcAddr)
    }

nullTracers :: TracersExtra ntnAddr ntnVersion ntnVersionData
                            ntcAddr ntcVersion ntcVersionData
nullTracers = 
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


-- | P2P Applications Extras
--
-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data ApplicationsExtra ntnAddr =
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
    , daPeerMetrics            :: PeerMetrics IO ntnAddr

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
data ConnectionManagerDataInMode peerAddr (mode :: MuxMode) where
    CMDInInitiatorMode
      :: ConnectionManagerDataInMode peerAddr InitiatorMode

    CMDInInitiatorResponderMode
      :: Server.ControlChannel IO
          (Server.NewConnection
            peerAddr
            (Handle InitiatorResponderMode peerAddr ByteString IO () ()))
      -> StrictTVar IO Server.InboundGovernorObservableState
      -> ConnectionManagerDataInMode peerAddr InitiatorResponderMode


--
-- Node-To-Client type aliases
--
-- Node-To-Client diffusion is only used in 'ResponderMode'.
--

type NodeToClientHandle ntcAddr =
    Handle ResponderMode ntcAddr ByteString IO Void ()

type NodeToClientHandleError ntcVersion =
    HandleError ResponderMode ntcVersion

type NodeToClientConnectionHandler
      ntcFd ntcAddr ntcVersion ntcVersionData =
    ConnectionHandler
      ResponderMode
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr)
      (NodeToClientHandleError ntcVersion)
      (ntcVersion, ntcVersionData)
      IO

type NodeToClientConnectionManagerArguments
      ntcFd ntcAddr ntcVersion ntcVersionData =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr)
      (NodeToClientHandleError ntcVersion)
      (ntcVersion, ntcVersionData)
      IO

type NodeToClientConnectionManager
      ntcFd ntcAddr ntcVersion ntcVersionData =
    ConnectionManager
      ResponderMode
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr)
      (NodeToClientHandleError ntcVersion)
      IO

--
-- Node-To-Node type aliases
--
-- Node-To-Node diffusion runs in either 'InitiatorMode' or 'InitiatorResponderMode'.
--

type NodeToNodeHandle
       (mode :: MuxMode)
       ntnAddr
       a =
    Handle mode ntnAddr ByteString IO () a

type NodeToNodeConnectionHandler
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData
       a =
    ConnectionHandler
      mode
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr a)
      (HandleError mode ntnVersion)
      (ntnVersion, ntnVersionData)
      IO

type NodeToNodeConnectionManagerArguments
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData
       a =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr a)
      (HandleError mode ntnVersion)
      (ntnVersion, ntnVersionData)
      IO

type NodeToNodeConnectionManager
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion
       a =
    ConnectionManager
      mode
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr a)
      (HandleError mode ntnVersion)
      IO

--
-- Governor type aliases
--

type NodeToNodePeerConnectionHandle (mode :: MuxMode) ntnAddr a =
    PeerConnectionHandle
      mode
      ntnAddr
      ByteString
      IO () a

type NodeToNodePeerStateActions (mode :: MuxMode) ntnAddr a =
    Governor.PeerStateActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr a)
      IO

type NodeToNodePeerSelectionActions (mode :: MuxMode) ntnAddr a =
    Governor.PeerSelectionActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr a)
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
         RemoteAddress NodeToNodeVersion
         LocalAddress  NodeToClientVersion
    -> TracersExtra
         RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress  NodeToClientVersion NodeToClientVersionData
    -> Arguments
         Socket      RemoteAddress
         LocalSocket LocalAddress
    -> ArgumentsExtra
    -> Applications
         RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress  NodeToClientVersion NodeToClientVersionData
    -> ApplicationsExtra RemoteAddress
    -> IO Void
run tracers tracersExtra
    args    argsExtra
    apps    appsExtra =
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handle (\e -> traceWith (dtDiffusionInitializationTracer tracers)
                            (DiffusionErrored e)
               >> throwIO e)
         $ withIOManager $ \iocp -> do
             let ntnSnocket :: SocketSnocket
                 ntnSnocket = Snocket.socketSnocket iocp

                 ntcSnocket :: LocalSnocket
                 ntcSnocket = Snocket.localSnocket iocp

                 ntnHandshakeArgs =
                   HandshakeArguments {
                       haHandshakeTracer = dtHandshakeTracer tracers,
                       haHandshakeCodec  = NodeToNode.nodeToNodeHandshakeCodec,
                       haVersionDataCodec =
                         cborTermVersionDataCodec
                           NodeToNode.nodeToNodeCodecCBORTerm,
                       haAcceptVersion = acceptableVersion,
                       haTimeLimits = timeLimitsHandshake
                     }

                 ntcHandshakeArgs =
                   HandshakeArguments {
                       haHandshakeTracer  = dtLocalHandshakeTracer tracers,
                       haHandshakeCodec   = NodeToClient.nodeToClientHandshakeCodec,
                       haVersionDataCodec =
                         cborTermVersionDataCodec
                           NodeToClient.nodeToClientCodecCBORTerm,
                       haAcceptVersion = acceptableVersion,
                       haTimeLimits = noTimeLimitsHandshake
                     }

                 domainResolver :: [DomainAccessPoint]
                                -> IO (Map DomainAccessPoint (Set Socket.SockAddr))
                 domainResolver =
                   resolveDomainAccessPoint
                     (dtTracePublicRootPeersTracer tracersExtra)
                     DNS.defaultResolvConf
                     ioDNSActions

             runDataDiffusionM
               ntnSnocket
               ntnHandshakeArgs
               socketAddressType
               nodeDataFlow
               (curry IP.toSockAddr)
               domainResolver
               ntcSnocket
               ntcHandshakeArgs
               localSocketFileDescriptor
               tracers tracersExtra
               args argsExtra
               apps appsExtra


runDataDiffusionM
    :: forall ntnFd ntnAddr ntnVersion ntnVersionData
              ntcFd ntcAddr ntcVersion ntcVersionData m.
       ( Monad m
       , Typeable ntnAddr
       , Ord      ntnAddr
       , Show     ntnAddr
       , Typeable ntnVersion
       , Ord      ntnVersion
       , Show     ntnVersion
       , Typeable ntcAddr
       , Ord      ntcAddr
       , Show     ntcAddr
       , Ord      ntcVersion
       , m ~ IO
       )
    => Snocket m ntnFd ntnAddr
    -> HandshakeArguments (ConnectionId ntnAddr) ntnVersion ntnVersionData m
    -> (ntnAddr -> Maybe AddressType)
    -> (ntnVersion -> ntnVersionData -> DataFlow)
    -> (IP -> Socket.PortNumber -> ntnAddr)
    -> ([DomainAccessPoint] -> m (Map DomainAccessPoint (Set ntnAddr)))
    -> Snocket m ntcFd ntcAddr
    -> HandshakeArguments (ConnectionId ntcAddr) ntcVersion ntcVersionData m
    -> (ntcFd -> m FileDescriptor)
    -> Tracers ntnAddr ntnVersion
               ntcAddr ntcVersion
    -> TracersExtra ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
    -> Arguments ntnFd ntnAddr
                 ntcFd ntcAddr
    -> ArgumentsExtra
    -> Applications
         ntnAddr ntnVersion ntnVersionData
         ntcAddr ntcVersion ntcVersionData
    -> ApplicationsExtra ntnAddr
    -> m Void
runDataDiffusionM ntnSnocket ntnHandshakeArgs
                  ntnAddressType ntnDataFlow
                  ntnToPeerAddr
                  domainResolver
                  ntcSnocket ntcHandshakeArgs
                  ntcGetFileDescriptor
                  tracers
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
                  } = do
    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    cmIPv4Address
      <- traverse (either (Snocket.getLocalAddr ntnSnocket) pure)
                  daIPv4Address
    case cmIPv4Address of
      Just addr | Just IPv4Address <- ntnAddressType addr
                -> pure ()
                | otherwise
                -> throwIO (UnexpectedIPv4Address addr)
      Nothing   -> pure ()

    cmIPv6Address
      <- traverse (either (Snocket.getLocalAddr ntnSnocket) pure)
                  daIPv6Address
    case cmIPv6Address of
      Just addr | Just IPv6Address <- ntnAddressType addr
                -> pure ()
                | otherwise
                -> throwIO (UnexpectedIPv6Address addr)
      Nothing   -> pure ()

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

    let localConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0

        --
        -- local connection manager
        --
        localThread :: Maybe (m Void)
        localThread =
          case daLocalAddress of
            Nothing -> Nothing
            Just localAddr ->
              Just $ withLocalSocket tracer ntcGetFileDescriptor ntcSnocket localAddr
                       $ \localSocket -> do
                let localConnectionHandler :: NodeToClientConnectionHandler
                                                ntcFd ntcAddr ntcVersion ntcVersionData
                    localConnectionHandler =
                      makeConnectionHandler
                        dtLocalMuxTracer
                        SingResponderMode
                        localMiniProtocolBundle
                        ntcHandshakeArgs
                        ( ( \ (OuroborosApplication apps)
                           -> Bundle
                                (WithHot apps)
                                (WithWarm (\_ _ -> []))
                                (WithEstablished (\_ _ -> []))
                          ) <$> daLocalResponderApplication )
                        (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

                    localConnectionManagerArguments
                      :: NodeToClientConnectionManagerArguments
                           ntcFd ntcAddr ntcVersion ntcVersionData
                    localConnectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtLocalConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtLocalMuxTracer,
                          cmIPv4Address         = Nothing,
                          cmIPv6Address         = Nothing,
                          cmAddressType         = const Nothing,
                          cmSnocket             = ntcSnocket,
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
                  $ \(localConnectionManager :: NodeToClientConnectionManager
                                                  ntcFd ntcAddr ntcVersion ntcVersionData) 
                    -> do

                  --
                  -- run local server
                  --

                  traceWith tracer . RunLocalServer
                    =<< Snocket.getLocalAddr ntcSnocket localSocket

                  Async.withAsync
                    (Server.run
                      ServerArguments {
                          serverSockets               = localSocket :| [],
                          serverSnocket               = ntcSnocket,
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

        remoteThread :: m Void
        remoteThread =
          withLedgerPeers
            ledgerPeersRng
            ntnToPeerAddr
            dtLedgerPeersTracer
            daReadUseLedgerAfter
            daLedgerPeersCtx
            domainResolver
            $ \requestLedgerPeers ledgerPeerThread ->
            case cmdInMode of
              -- InitiatorOnlyMode
              --
              -- Run peer selection only
              HasInitiator CMDInInitiatorMode -> do
                let connectionManagerArguments
                      :: NodeToNodeConnectionManagerArguments
                           InitiatorMode
                           ntnFd ntnAddr ntnVersion ntnVersionData
                           Void
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = ntnAddressType,
                          cmSnocket             = ntnSnocket,
                          connectionDataFlow    = uncurry ntnDataFlow,
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
                      :: NodeToNodeConnectionHandler
                           InitiatorMode
                           ntnFd ntnAddr ntnVersion ntnVersionData
                           Void
                    connectionHandler =
                      makeConnectionHandler
                        dtMuxTracer
                        SingInitiatorMode
                        miniProtocolBundleInitiatorMode
                        ntnHandshakeArgs
                        daApplicationInitiatorMode
                        (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  NotInResponderMode
                  $ \(connectionManager
                      :: NodeToNodeConnectionManager
                           InitiatorMode ntnFd ntnAddr ntnVersion Void)
                    -> do
#ifdef POSIX
                  -- TODO: this can be removed once trace reporting will be
                  -- merged.
                  _ <- Signals.installHandler
                    Signals.sigUSR1
                    (Signals.Catch
                      (do state <- readState connectionManager
                          traceWith dtConnectionManagerTracer
                                    (TrState state)
                      )
                    )
                    Nothing
#endif

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
                          :: NodeToNodePeerStateActions InitiatorMode ntnAddr Void) ->
                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      ntnToPeerAddr
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerSelectionActionsThread
                      (peerSelectionActions
                        :: NodeToNodePeerSelectionActions
                        InitiatorMode ntnAddr Void) ->

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
                          ntnFd ntnAddr ntnVersion ntnVersionData
                          ()
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = ntnAddressType,
                          cmSnocket             = ntnSnocket,
                          connectionDataFlow    = uncurry ntnDataFlow,
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
                          ntnFd ntnAddr ntnVersion ntnVersionData
                          ()
                    connectionHandler =
                      makeConnectionHandler
                         dtMuxTracer
                         SingInitiatorResponderMode
                         miniProtocolBundleInitiatorResponderMode
                         ntnHandshakeArgs
                         daApplicationInitiatorResponderMode
                         (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  (InResponderMode controlChannel)
                  $ \(connectionManager
                        :: NodeToNodeConnectionManager InitiatorResponderMode ntnFd ntnAddr ntnVersion ()) -> do
#ifdef POSIX
                  _ <- Signals.installHandler
                    Signals.sigUSR1
                    (Signals.Catch
                      (do state <- readState connectionManager
                          traceWith dtConnectionManagerTracer
                                    (TrState state)
                      )
                    )
                    Nothing
#endif
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
                               InitiatorResponderMode ntnAddr ()) ->

                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      ntnToPeerAddr
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerRootProviderThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorResponderMode ntnAddr ()) ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorResponderTracer
                          dtTracePeerSelectionCounters
                          fuzzRng
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy
                            policyRngVar (readTVar churnModeVar) daPeerMetrics))
                        $ \governorThread ->
                        withSockets tracer ntnSnocket
                                    ( catMaybes
                                        [ daIPv4Address
                                        , daIPv6Address
                                        ]
                                    )
                                    $ \sockets addresses -> do
                          --
                          -- Run server
                          --
                          traceWith tracer (RunServer addresses)
                          Async.withAsync
                            (Server.run
                              ServerArguments {
                                  serverSockets               = sockets,
                                  serverSnocket               = ntnSnocket,
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
localDataFlow :: ntcVersion
              -> ntcVersionData
              -> DataFlow
localDataFlow _ _ = Unidirectional


--
-- Socket utility functions
--

withSockets :: forall m ntnFd ntnAddr ntcAddr a.
               ( MonadThrow m
               , Typeable ntnAddr
               , Show     ntnAddr
               )
            => Tracer m (InitializationTracer ntnAddr ntcAddr)
            -> Snocket m ntnFd ntnAddr
            -> [Either ntnFd ntnAddr]
            -> (NonEmpty ntnFd -> NonEmpty ntnAddr -> m a)
            -> m a
withSockets tracer sn addresses k = go [] addresses
  where
    go !acc (a : as) = withSocket a (\sa -> go (sa : acc) as)
    go []   []       = throwIO (NoSocket :: Failure ntnAddr)
    go !acc []       =
      let acc' = NonEmpty.fromList (reverse acc)
      in (k $! (fst <$> acc')) $! (snd <$> acc')

    withSocket :: Either ntnFd ntnAddr
               -> ((ntnFd, ntnAddr) -> m a)
               -> m a
    withSocket (Left sock) f =
      bracket
        (pure sock)
        (Snocket.close sn)
        $ \_sock -> do
          !addr <- Snocket.getLocalAddr sn sock
          f (sock, addr)
    withSocket (Right addr) f =
      bracket
        (do traceWith tracer (CreatingServerSocket addr)
            Snocket.open sn (Snocket.addrFamily sn addr))
        (Snocket.close sn)
        $ \sock -> do
          traceWith tracer $ ConfiguringServerSocket addr
          Snocket.bind sn sock addr
          traceWith tracer $ ListeningServerSocket addr
          Snocket.listen sn sock
          traceWith tracer $ ServerSocketUp addr
          f (sock, addr)


withLocalSocket :: forall ntnAddr ntcFd ntcAddr a.
                   ( -- Win32 only constraints:
                     Typeable ntnAddr
                   , Show     ntnAddr
                   )
                => Tracer IO (InitializationTracer ntnAddr ntcAddr)
                -> (ntcFd -> IO FileDescriptor)
                -> Snocket IO ntcFd ntcAddr
                -> Either ntcFd ntcAddr
                -> (ntcFd -> IO a)
                -> IO a
withLocalSocket tracer getFileDescriptor sn localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> traceWith tracer (UnsupportedReadySocketCase
                                       :: DiffusionInitializationTracer ntnAddr ntcAddr)
                >> throwIO (UnsupportedReadySocket :: DiffusionFailure ntnAddr)
#else
         Left sd -> do
             addr <- Snocket.getLocalAddr sn sd
             traceWith tracer (UsingSystemdSocket addr)
             return (Left sd)
#endif
         Right addr -> do
             traceWith tracer $ CreateSystemdSocketForSnocketPath addr
             sd <- Snocket.open sn (Snocket.addrFamily sn addr)
             traceWith tracer $ CreatedLocalSocket addr
             return (Right (sd, addr))
    )
    -- We close the socket here, even if it was provided to us.
    (\case
      Right (sd, _) -> Snocket.close sn sd
      Left   sd     -> Snocket.close sn sd
    )
    $ \case
      -- unconfigured socket
      Right (sd, addr) -> do
        traceWith tracer . ConfiguringLocalSocket addr
           =<< getFileDescriptor sd
        Snocket.bind sn sd addr
        traceWith tracer . ListeningLocalSocket addr
           =<< getFileDescriptor sd
        Snocket.listen sn sd
        traceWith tracer . LocalSocketUp addr
           =<< getFileDescriptor sd
        k sd

      -- pre-configured systemd socket
      Left sd -> k sd
