{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

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
  , Interfaces (..)
  , runM
  , NodeToNodePeerConnectionHandle
  , AbstractTransitionTrace
  , RemoteTransitionTrace
  ) where


import           Control.Exception (IOException)
import           Control.Monad.Class.MonadAsync (Async, MonadAsync)
import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (asum)
import           Data.IP (IP)
import qualified Data.IP as IP
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Set (Set)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           System.Random (StdGen, newStdGen, split)
#ifdef POSIX
import qualified System.Posix.Signals as Signals
#endif

import qualified Network.DNS as DNS
import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import           Ouroboros.Network.Snocket (FileDescriptor, LocalAddress,
                     LocalSnocket, LocalSocket (..), Snocket, SocketSnocket,
                     localSocketFileDescriptor)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Version

import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Diffusion.Common hiding (nullTracers)
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion.Policies
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                     RemoteTransitionTrace)
import           Ouroboros.Network.Mux hiding (MiniProtocol (..))
import           Ouroboros.Network.MuxMode
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
                     NodeToClientVersionData)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..),
                     DiffusionMode (..), NodeToNodeVersion (..),
                     NodeToNodeVersionData (..), RemoteAddress)
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Network.PeerSelection.Governor as Governor
import           Ouroboros.Network.PeerSelection.Governor.Types
                     (ChurnMode (ChurnModeNormal), DebugPeerSelection (..),
                     PeerSelectionCounters (..), TracePeerSelection (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (UseLedgerAfter (..), withLedgerPeers)
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics (..))
import           Ouroboros.Network.PeerSelection.PeerStateActions
                     (PeerConnectionHandle, PeerSelectionActionsTrace (..),
                     PeerStateActionsArguments (..), withPeerStateActions)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DNSActions,
                     DomainAccessPoint, LookupReqs (..), RelayAccessPoint (..),
                     TraceLocalRootPeers (..), TracePublicRootPeers (..),
                     ioDNSActions, resolveDomainAccessPoint)
import           Ouroboros.Network.PeerSelection.Simple
import           Ouroboros.Network.RethrowPolicy
import           Ouroboros.Network.Server2 (ServerArguments (..),
                     ServerTrace (..))
import qualified Ouroboros.Network.Server2 as Server

-- | P2P DiffusionTracers Extras
--
data TracersExtra ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData
                  resolverError m =
    TracersExtra {
      dtTraceLocalRootPeersTracer
        :: Tracer m (TraceLocalRootPeers ntnAddr resolverError)

    , dtTracePublicRootPeersTracer
        :: Tracer m TracePublicRootPeers

    , dtTracePeerSelectionTracer
        :: Tracer m (TracePeerSelection ntnAddr)

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer m (DebugPeerSelection
                       ntnAddr
                       (PeerConnectionHandle
                         InitiatorMode
                         ntnAddr
                         ByteString
                         m () Void))

    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer m (DebugPeerSelection
                       ntnAddr
                       (PeerConnectionHandle
                         InitiatorResponderMode
                         ntnAddr
                         ByteString
                         m () ()))

    , dtTracePeerSelectionCounters
        :: Tracer m PeerSelectionCounters

    , dtPeerSelectionActionsTracer
        :: Tracer m (PeerSelectionActionsTrace ntnAddr)

    , dtConnectionManagerTracer
        :: Tracer m (ConnectionManagerTrace
                      ntnAddr
                      (ConnectionHandlerTrace
                         ntnVersion
                         ntnVersionData))

    , dtConnectionManagerTransitionTracer
        :: Tracer m (AbstractTransitionTrace ntnAddr)

    , dtServerTracer
        :: Tracer m (ServerTrace ntnAddr)

    , dtInboundGovernorTracer
        :: Tracer m (InboundGovernorTrace ntnAddr)

    , dtInboundGovernorTransitionTracer
        :: Tracer m (RemoteTransitionTrace ntnAddr)

      --
      -- NodeToClient tracers
      --

      -- | Connection manager tracer for local clients
    , dtLocalConnectionManagerTracer
        :: Tracer m (ConnectionManagerTrace
                       ntcAddr
                       (ConnectionHandlerTrace
                          ntcVersion
                          ntcVersionData))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer m (ServerTrace ntcAddr)

      -- | Inbound protocol governor tracer for local clients
    , dtLocalInboundGovernorTracer
        :: Tracer m (InboundGovernorTrace ntcAddr)
    }

nullTracers :: Applicative m
            => TracersExtra ntnAddr ntnVersion ntnVersionData
                            ntcAddr ntcVersion ntcVersionData
                            resolverError m
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
      , dtConnectionManagerTransitionTracer          = nullTracer
      , dtServerTracer                               = nullTracer
      , dtInboundGovernorTracer                      = nullTracer
      , dtInboundGovernorTransitionTracer            = nullTracer
      , dtLocalConnectionManagerTracer               = nullTracer
      , dtLocalServerTracer                          = nullTracer
      , dtLocalInboundGovernorTracer                 = nullTracer
    }

-- | P2P Arguments Extras
--
data ArgumentsExtra m = ArgumentsExtra {
      -- | selection targets for the peer governor
      --
      daPeerSelectionTargets :: PeerSelectionTargets

    , daReadLocalRootPeers  :: STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
    , daReadPublicRootPeers :: STM m [RelayAccessPoint]
    , daReadUseLedgerAfter  :: STM m UseLedgerAfter

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


-- | P2P Applications Extras
--
-- TODO: we need initiator only mode for Deadalus, there's no reason why it
-- should run a node-to-node server side.
--
data ApplicationsExtra ntnAddr m =
    ApplicationsExtra {
    -- | /node-to-node/ rethrow policy
    --
      daRethrowPolicy          :: RethrowPolicy

    -- | /node-to-client/ rethrow policy
    --
    , daLocalRethrowPolicy     :: RethrowPolicy

    -- | 'PeerMetrics' used by peer selection policy (see
    -- 'simplePeerSelectionPolicy')
    --
    , daPeerMetrics            :: PeerMetrics m ntnAddr

    -- | Used by churn-governor
    --
    , daBlockFetchMode         :: STM m FetchMode
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
data ConnectionManagerDataInMode peerAddr m (mode :: MuxMode) where
    CMDInInitiatorMode
      :: ConnectionManagerDataInMode peerAddr m InitiatorMode

    CMDInInitiatorResponderMode
      :: Server.ControlChannel m
          (Server.NewConnection
            peerAddr
            (Handle InitiatorResponderMode peerAddr ByteString m () ()))
      -> StrictTVar m Server.InboundGovernorObservableState
      -> ConnectionManagerDataInMode peerAddr m InitiatorResponderMode


--
-- Node-To-Client type aliases
--
-- Node-To-Client diffusion is only used in 'ResponderMode'.
--

type NodeToClientHandle ntcAddr m =
    Handle ResponderMode ntcAddr ByteString m Void ()

type NodeToClientHandleError ntcVersion =
    HandleError ResponderMode ntcVersion

type NodeToClientConnectionHandler
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionHandler
      ResponderMode
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr m)
      (NodeToClientHandleError ntcVersion)
      (ntcVersion, ntcVersionData)
      m

type NodeToClientConnectionManagerArguments
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr m)
      (NodeToClientHandleError ntcVersion)
      (ntcVersion, ntcVersionData)
      m

type NodeToClientConnectionManager
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionManager
      ResponderMode
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr m)
      (NodeToClientHandleError ntcVersion)
      m

--
-- Node-To-Node type aliases
--
-- Node-To-Node diffusion runs in either 'InitiatorMode' or 'InitiatorResponderMode'.
--

type NodeToNodeHandle
       (mode :: MuxMode)
       ntnAddr m a =
    Handle mode ntnAddr ByteString m () a

type NodeToNodeConnectionHandler
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData m a =
    ConnectionHandler
      mode
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr m a)
      (HandleError mode ntnVersion)
      (ntnVersion, ntnVersionData)
      m

type NodeToNodeConnectionManagerArguments
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData m a =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr m a)
      (HandleError mode ntnVersion)
      (ntnVersion, ntnVersionData)
      m

type NodeToNodeConnectionManager
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion m a =
    ConnectionManager
      mode
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr m a)
      (HandleError mode ntnVersion)
      m

--
-- Governor type aliases
--

type NodeToNodePeerConnectionHandle (mode :: MuxMode) ntnAddr m a =
    PeerConnectionHandle
      mode
      ntnAddr
      ByteString
      m () a

type NodeToNodePeerStateActions (mode :: MuxMode) ntnAddr m a =
    Governor.PeerStateActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr m a)
      m

type NodeToNodePeerSelectionActions (mode :: MuxMode) ntnAddr m a =
    Governor.PeerSelectionActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr m a)
      m

data Interfaces ntnFd ntnAddr ntnVersion ntnVersionData
                ntcFd ntcAddr ntcVersion ntcVersionData
                resolver resolverError
                m =
    Interfaces {
        -- | node-to-node snocket
        --
        diNtnSnocket
          :: Snocket m ntnFd ntnAddr,

        -- | node-to-node handshake configuration
        --
        diNtnHandshakeArguments
          :: HandshakeArguments (ConnectionId ntnAddr) ntnVersion ntnVersionData m,

        -- | node-to-node address type
        --
        diNtnAddressType
          :: ntnAddr -> Maybe AddressType,

        -- | node-to-node data flow used by connection manager to classify
        -- negotiated connections
        --
        diNtnDataFlow
          :: ntnVersion -> ntnVersionData -> DataFlow,

        -- | node-to-node peer address
        --
        diNtnToPeerAddr
          :: IP -> Socket.PortNumber -> ntnAddr,

        -- | node-to-node domain resolver
        --
        diNtnDomainResolver
          :: LookupReqs -> [DomainAccessPoint] -> m (Map DomainAccessPoint (Set ntnAddr)),

        -- | node-to-client snocket
        --
        diNtcSnocket
          :: Snocket m ntcFd ntcAddr,

        -- | node-to-client handshake configuration
        --
        diNtcHandshakeArguments
          :: HandshakeArguments (ConnectionId ntcAddr) ntcVersion ntcVersionData m,

        -- | node-to-client file descriptor
        --
        diNtcGetFileDescriptor
          :: ntcFd -> m FileDescriptor,

        -- | diffusion pseudo random generator. It is split between various
        -- components that need randomness, e.g. inbound governor, peer
        -- selection, policies, etc.
        --
        diRng
          :: StdGen,

        -- | callback which is used to register @SIGUSR1@ signal handler.
        diInstallSigUSR1Handler
          :: forall mode x.
             NodeToNodeConnectionManager mode ntnFd ntnAddr ntnVersion m x
          -> m (),

        -- | diffusion dns actions
        --
        diDnsActions
          :: LookupReqs -> DNSActions resolver resolverError m
      }

runM
    :: forall m ntnFd ntnAddr ntnVersion ntnVersionData
                ntcFd ntcAddr ntcVersion ntcVersionData
                resolver resolverError.
       ( MonadAsync       m
       , MonadEvaluate    m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadMask        m
       , MonadThrow  (STM m)
       , MonadTime        m
       , MonadTimer       m
       , Eq (Async m Void)
       , Typeable  ntnAddr
       , Ord       ntnAddr
       , Show      ntnAddr
       , Typeable  ntnVersion
       , Ord       ntnVersion
       , Show      ntnVersion
       , Typeable  ntcAddr
       , Ord       ntcAddr
       , Show      ntcAddr
       , Ord       ntcVersion
       , Exception resolverError
       )
    => -- | interfaces
       Interfaces ntnFd ntnAddr ntnVersion ntnVersionData
                  ntcFd ntcAddr ntcVersion ntcVersionData
                  resolver resolverError
                  m
    -> -- | tracers
       Tracers ntnAddr ntnVersion
               ntcAddr ntcVersion
               m
    -> -- | p2p tracers
       TracersExtra ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    resolverError m
    -> -- | configuration
       Arguments ntnFd ntnAddr
                 ntcFd ntcAddr
    -> -- | p2p configuration
       ArgumentsExtra m

    -> -- | protocol handlers
       Applications ntnAddr ntnVersion ntnVersionData
                    ntcAddr ntcVersion ntcVersionData
                    m
    -> -- | p2p protocol handlers
       ApplicationsExtra ntnAddr m
    -> m Void
runM Interfaces
       { diNtnSnocket
       , diNtnHandshakeArguments
       , diNtnAddressType
       , diNtnDataFlow
       , diNtnToPeerAddr
       , diNtnDomainResolver
       , diNtcSnocket
       , diNtcHandshakeArguments
       , diNtcGetFileDescriptor
       , diRng
       , diInstallSigUSR1Handler
       , diDnsActions
       }
     Tracers
       { dtMuxTracer
       , dtLocalMuxTracer
       , dtLedgerPeersTracer
       , dtDiffusionInitializationTracer = tracer
       }
     TracersExtra
       { dtTracePeerSelectionTracer
       , dtDebugPeerSelectionInitiatorTracer
       , dtDebugPeerSelectionInitiatorResponderTracer
       , dtTracePeerSelectionCounters
       , dtPeerSelectionActionsTracer
       , dtTraceLocalRootPeersTracer
       , dtTracePublicRootPeersTracer
       , dtConnectionManagerTracer
       , dtConnectionManagerTransitionTracer
       , dtServerTracer
       , dtInboundGovernorTracer
       , dtInboundGovernorTransitionTracer
       , dtLocalConnectionManagerTracer
       , dtLocalServerTracer
       , dtLocalInboundGovernorTracer
       }
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
       { daRethrowPolicy
       , daLocalRethrowPolicy
       , daPeerMetrics
       , daBlockFetchMode
       }
  = do
    -- Thread to which 'RethrowPolicy' will throw fatal exceptions.
    mainThreadId <- myThreadId

    cmIPv4Address
      <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                  daIPv4Address
    case cmIPv4Address of
      Just addr | Just IPv4Address <- diNtnAddressType addr
                -> pure ()
                | otherwise
                -> throwIO (UnexpectedIPv4Address addr)
      Nothing   -> pure ()

    cmIPv6Address
      <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                  daIPv6Address
    case cmIPv6Address of
      Just addr | Just IPv6Address <- diNtnAddressType addr
                -> pure ()
                | otherwise
                -> throwIO (UnexpectedIPv6Address addr)
      Nothing   -> pure ()

    lookupReqs <- case (cmIPv4Address, cmIPv6Address) of
                         (Just _, Nothing) -> return LookupReqAOnly
                         (Nothing, Just _) -> return LookupReqAAAAOnly
                         (Just _, Just _)  -> return LookupReqAAndAAAA
                         _                 ->
                             throwIO (NoSocket :: Failure RemoteAddress)

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
                <*> Server.newObservableStateVar ntnInbgovRng)

    localControlChannel <- Server.newControlChannel
    localServerStateVar <- Server.newObservableStateVar ntcInbgovRng

    -- RNGs used for picking random peers from the ledger and for
    -- demoting/promoting peers.
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
              Just $ withLocalSocket tracer diNtcGetFileDescriptor diNtcSnocket localAddr
                       $ \localSocket -> do
                let localConnectionHandler :: NodeToClientConnectionHandler
                                                ntcFd ntcAddr ntcVersion ntcVersionData m
                    localConnectionHandler =
                      makeConnectionHandler
                        dtLocalMuxTracer
                        SingResponderMode
                        diNtcHandshakeArguments
                        ( ( \ (OuroborosApplication apps)
                           -> Bundle
                                (WithHot apps)
                                (WithWarm (\_ _ -> []))
                                (WithEstablished (\_ _ -> []))
                          ) <$> daLocalResponderApplication )
                        (mainThreadId, rethrowPolicy <> daLocalRethrowPolicy)

                    localConnectionManagerArguments
                      :: NodeToClientConnectionManagerArguments
                           ntcFd ntcAddr ntcVersion ntcVersionData m
                    localConnectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtLocalConnectionManagerTracer,
                          cmTrTracer            = nullTracer, -- TODO: issue #3320
                          cmMuxTracer           = dtLocalMuxTracer,
                          cmIPv4Address         = Nothing,
                          cmIPv6Address         = Nothing,
                          cmAddressType         = const Nothing,
                          cmSnocket             = diNtcSnocket,
                          cmTimeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                          cmOutboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                          connectionDataFlow    = uncurry localDataFlow,
                          cmPrunePolicy         = Diffusion.Policies.prunePolicy
                                                    localServerStateVar,
                          cmConnectionsLimits   = localConnectionLimits
                        }

                withConnectionManager
                  localConnectionManagerArguments
                  localConnectionHandler
                  classifyHandleError
                  (InResponderMode localControlChannel)
                  $ \(localConnectionManager :: NodeToClientConnectionManager
                                                  ntcFd ntcAddr ntcVersion
                                                  ntcVersionData m)
                    -> do

                  --
                  -- run local server
                  --

                  traceWith tracer . RunLocalServer
                    =<< Snocket.getLocalAddr diNtcSnocket localSocket

                  Async.withAsync
                    (Server.run
                      ServerArguments {
                          serverSockets               = localSocket :| [],
                          serverSnocket               = diNtcSnocket,
                          serverTracer                = dtLocalServerTracer,
                          serverTrTracer              = nullTracer, -- TODO: issue #3320
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
            diNtnToPeerAddr
            dtLedgerPeersTracer
            daReadUseLedgerAfter
            daLedgerPeersCtx
            (diNtnDomainResolver lookupReqs)
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
                           m Void
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            =
                            fmap abstractState
                            `contramap` dtConnectionManagerTransitionTracer,
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = diNtnAddressType,
                          cmSnocket             = diNtnSnocket,
                          connectionDataFlow    = uncurry diNtnDataFlow,
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
                           m Void
                    connectionHandler =
                      makeConnectionHandler
                        dtMuxTracer
                        SingInitiatorMode
                        diNtnHandshakeArguments
                        daApplicationInitiatorMode
                        (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  NotInResponderMode
                  $ \(connectionManager
                      :: NodeToNodeConnectionManager
                           InitiatorMode ntnFd ntnAddr ntnVersion m Void)
                    -> do
                  diInstallSigUSR1Handler connectionManager

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
                          :: NodeToNodePeerStateActions InitiatorMode ntnAddr m Void) ->
                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      diNtnToPeerAddr
                      (diDnsActions lookupReqs)
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerSelectionActionsThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorMode ntnAddr m Void) ->

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
                          m ()
                    connectionManagerArguments =
                      ConnectionManagerArguments {
                          cmTracer              = dtConnectionManagerTracer,
                          cmTrTracer            =
                            fmap abstractState
                            `contramap` dtConnectionManagerTransitionTracer,
                          cmMuxTracer           = dtMuxTracer,
                          cmIPv4Address,
                          cmIPv6Address,
                          cmAddressType         = diNtnAddressType,
                          cmSnocket             = diNtnSnocket,
                          connectionDataFlow    = uncurry diNtnDataFlow,
                          cmPrunePolicy         =
                            case cmdInMode of
                              HasInitiatorResponder (CMDInInitiatorResponderMode _ serverStateVar) ->
                                Diffusion.Policies.prunePolicy serverStateVar,
                          cmConnectionsLimits   = daAcceptedConnectionsLimit,
                          cmTimeWaitTimeout     = daTimeWaitTimeout,
                          cmOutboundIdleTimeout = daProtocolIdleTimeout
                        }

                    connectionHandler
                      :: NodeToNodeConnectionHandler
                          InitiatorResponderMode
                          ntnFd ntnAddr ntnVersion ntnVersionData
                          m ()
                    connectionHandler =
                      makeConnectionHandler
                         dtMuxTracer
                         SingInitiatorResponderMode
                         diNtnHandshakeArguments
                         daApplicationInitiatorResponderMode
                         (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  (InResponderMode controlChannel)
                  $ \(connectionManager
                        :: NodeToNodeConnectionManager
                             InitiatorResponderMode ntnFd ntnAddr ntnVersion m ()
                     ) -> do
                  diInstallSigUSR1Handler connectionManager
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
                               InitiatorResponderMode ntnAddr m ()) ->

                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      diNtnToPeerAddr
                      (diDnsActions lookupReqs)
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      peerStateActions
                      requestLedgerPeers
                      $ \mbLocalPeerRootProviderThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorResponderMode ntnAddr m ()) ->

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
                        withSockets tracer diNtnSnocket
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
                                  serverSnocket               = diNtnSnocket,
                                  serverTracer                = dtServerTracer,
                                  serverTrTracer              = dtInboundGovernorTransitionTracer,
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
    (ledgerPeersRng, rng1) = split diRng
    (policyRng,      rng2) = split rng1
    (churnRng,       rng3) = split rng2
    (fuzzRng,        rng4) = split rng3
    (ntnInbgovRng,   ntcInbgovRng) = split rng4

    -- Only the 'IOManagerError's are fatal, all the other exceptions in the
    -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
    -- this is so).
    rethrowPolicy =
      RethrowPolicy $ \_ctx err ->
        case fromException err of
          Just (_ :: IOManagerError) -> ShutdownNode
          Nothing                    -> mempty


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
    :: Tracers RemoteAddress NodeToNodeVersion
               LocalAddress  NodeToClientVersion
               IO
    -> TracersExtra RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
                    LocalAddress  NodeToClientVersion NodeToClientVersionData
                    IOException IO
    -> Arguments Socket      RemoteAddress
                 LocalSocket LocalAddress
    -> ArgumentsExtra IO
    -> Applications
         RemoteAddress NodeToNodeVersion   NodeToNodeVersionData
         LocalAddress  NodeToClientVersion NodeToClientVersionData
         IO
    -> ApplicationsExtra RemoteAddress IO
    -> IO Void
run tracers tracersExtra args argsExtra apps appsExtra = do
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handle (\e -> traceWith (dtDiffusionInitializationTracer tracers) (DiffusionErrored e)
               >> throwIO e)
         $ withIOManager $ \iocp -> do
             let diNtnSnocket :: SocketSnocket
                 diNtnSnocket = Snocket.socketSnocket iocp

                 diNtcSnocket :: LocalSnocket
                 diNtcSnocket = Snocket.localSnocket iocp

                 diNtnHandshakeArguments =
                   HandshakeArguments {
                       haHandshakeTracer = dtHandshakeTracer tracers,
                       haHandshakeCodec  = NodeToNode.nodeToNodeHandshakeCodec,
                       haVersionDataCodec =
                         cborTermVersionDataCodec
                           NodeToNode.nodeToNodeCodecCBORTerm,
                       haAcceptVersion = acceptableVersion,
                       haTimeLimits = timeLimitsHandshake
                     }
                 diNtcHandshakeArguments =
                   HandshakeArguments {
                       haHandshakeTracer  = dtLocalHandshakeTracer tracers,
                       haHandshakeCodec   = NodeToClient.nodeToClientHandshakeCodec,
                       haVersionDataCodec =
                         cborTermVersionDataCodec
                           NodeToClient.nodeToClientCodecCBORTerm,
                       haAcceptVersion = acceptableVersion,
                       haTimeLimits = noTimeLimitsHandshake
                     }

                 diInstallSigUSR1Handler
                   :: forall mode x.
                      NodeToNodeConnectionManager mode Socket RemoteAddress NodeToNodeVersion IO x
                   -> IO ()
#ifdef POSIX
                 diInstallSigUSR1Handler = \connectionManager -> do
                   _ <- Signals.installHandler
                     Signals.sigUSR1
                     (Signals.Catch
                       (do state <- readState connectionManager
                           traceWith (dtConnectionManagerTracer tracersExtra)
                                     (TrState state)
                       )
                     )
                     Nothing
                   return ()
#else
                 diInstallSigUSR1Handler = \_ -> pure ()
#endif

             let diNtnDomainResolver :: LookupReqs -> [DomainAccessPoint]
                                     -> IO (Map DomainAccessPoint (Set Socket.SockAddr))
                 diNtnDomainResolver lr =
                   resolveDomainAccessPoint
                     (dtTracePublicRootPeersTracer tracersExtra)
                     DNS.defaultResolvConf
                     (ioDNSActions lr)

             diRng <- newStdGen
             runM
               Interfaces {
                 diNtnSnocket,
                 diNtnHandshakeArguments,
                 diNtnAddressType = socketAddressType,
                 diNtnDataFlow = nodeDataFlow,
                 diNtnToPeerAddr = curry IP.toSockAddr,
                 diNtnDomainResolver,

                 diNtcSnocket,
                 diNtcHandshakeArguments,
                 diNtcGetFileDescriptor = localSocketFileDescriptor,

                 diRng,
                 diInstallSigUSR1Handler,
                 diDnsActions = ioDNSActions
               }
               tracers tracersExtra args argsExtra apps appsExtra


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


withLocalSocket :: forall ntnAddr ntcFd ntcAddr m a.
                   ( MonadThrow m
                     -- Win32 only constraints:
                   , Typeable ntnAddr
                   , Show     ntnAddr
                   )
                => Tracer m (InitializationTracer ntnAddr ntcAddr)
                -> (ntcFd -> m FileDescriptor)
                -> Snocket m ntcFd ntcAddr
                -> Either ntcFd ntcAddr
                -> (ntcFd -> m a)
                -> m a
withLocalSocket tracer getFileDescriptor sn localAddress k =
  bracket
    (
      case localAddress of
#if defined(mingw32_HOST_OS)
         -- Windows uses named pipes so can't take advantage of existing sockets
         Left _ -> traceWith tracer (UnsupportedReadySocketCase
                                       :: InitializationTracer ntnAddr ntcAddr)
                >> throwIO (UnsupportedReadySocket :: Failure ntnAddr)
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
