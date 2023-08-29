{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadMVar (MonadMVar)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Exception (IOException)
import           Control.Monad.Class.MonadAsync (Async, MonadAsync)
import qualified Control.Monad.Class.MonadAsync as Async
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Monad.Fix (MonadFix)
import           Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import           Data.ByteString.Lazy (ByteString)
import           Data.Foldable (asum)
import           Data.IP (IP)
import qualified Data.IP as IP
import           Data.Kind (Type)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map (Map)
import           Data.Maybe (catMaybes, maybeToList)
import           Data.Set (Set, elemAt)
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           System.Exit (ExitCode)
import           System.Random (StdGen, newStdGen, randomRs, split)
#ifdef POSIX
import qualified System.Posix.Signals as Signals
#endif

import qualified Network.DNS as DNS
import           Network.Socket (Socket)
import qualified Network.Socket as Socket

import           Network.Mux as Mx (MakeBearer)

import           Ouroboros.Network.Snocket (FileDescriptor, LocalAddress,
                     LocalSocket (..), Snocket,
                     localSocketFileDescriptor, makeLocalBearer,
                     makeSocketBearer)
import qualified Ouroboros.Network.Snocket as Snocket

import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Context (ExpandedInitiatorContext,
                     ResponderContext)
import           Ouroboros.Network.Protocol.Handshake
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Socket (configureSocket,
                     configureSystemdSocket)

import           Data.List (nub)
import           Ouroboros.Network.ConnectionHandler
import           Ouroboros.Network.ConnectionManager.Core
import           Ouroboros.Network.ConnectionManager.InformationChannel
                     (InboundGovernorInfoChannel, InformationChannel (..),
                     OutboundGovernorInfoChannel, newInformationChannel)
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Diffusion.Common hiding (nullTracers)
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion.Policies
import           Ouroboros.Network.Diffusion.Utils
import           Ouroboros.Network.ExitPolicy
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                     RemoteTransitionTrace)
import           Ouroboros.Network.IOManager
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
                     PeerSelectionCounters (..), PublicPeerSelectionState (..),
                     TracePeerSelection (..), emptyPublicPeerSelectionState)
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (UseLedgerAfter (..), withLedgerPeers)
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import           Ouroboros.Network.PeerSelection.PeerSelectionActions
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.PeerSelection.PeerStateActions
                     (PeerConnectionHandle, PeerSelectionActionsTrace (..),
                     PeerStateActionsArguments (..), pchPeerSharing,
                     withPeerStateActions)
import           Ouroboros.Network.PeerSelection.RootPeersDNS (DNSActions,
                     DNSSemaphore, DomainAccessPoint, LookupReqs (..),
                     RelayAccessPoint (..), TraceLocalRootPeers (..),
                     TracePublicRootPeers (..), ioDNSActions,
                     newLocalAndPublicRootDNSSemaphore,
                     resolveDomainAccessPoint)
import           Ouroboros.Network.PeerSelection.State.LocalRootPeers
                     (HotValency, WarmValency)
import           Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)
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
        :: Tracer m (DebugPeerSelection ntnAddr)

      -- TODO: can be unified with the previous one
    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer m (DebugPeerSelection ntnAddr)

    , dtTracePeerSelectionCounters
        :: Tracer m PeerSelectionCounters

    , dtPeerSelectionActionsTracer
        :: Tracer m (PeerSelectionActionsTrace ntnAddr ntnVersion)

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

    , daReadLocalRootPeers  :: STM m [(HotValency, WarmValency, Map RelayAccessPoint PeerAdvertise)]
    , daReadPublicRootPeers :: STM m (Map RelayAccessPoint PeerAdvertise)
    -- | Peer's own PeerSharing value.
    --
    -- This value comes from the node's configuration file and is static.
    , daOwnPeerSharing      :: PeerSharing
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

      -- | Churn interval between churn events in deadline mode.  A small fuzz
      -- is added (max 10 minutes) so that not all nodes churn at the same time.
      --
      -- By default it is set to 3300 seconds.
      --
    , daDeadlineChurnInterval :: DiffTime

      -- | Churn interval between churn events in bulk sync mode.  A small fuzz
      -- is added (max 1 minute) so that not all nodes churn at the same time.
      --
      -- By default it is set to 300 seconds.
      --
    , daBulkChurnInterval :: DiffTime
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
-- TODO: we need initiator only mode for Daedalus, there's no reason why it
-- should run a node-to-node server side.
--
data ApplicationsExtra ntnAddr m a =
    ApplicationsExtra {
    -- | /node-to-node/ rethrow policy
    --
      daRethrowPolicy       :: RethrowPolicy

    -- | /node-to-node/ return policy
    --
    , daReturnPolicy        :: ReturnPolicy a

    -- | /node-to-client/ rethrow policy
    --
    , daLocalRethrowPolicy  :: RethrowPolicy

    -- | 'PeerMetrics' used by peer selection policy (see
    -- 'simplePeerSelectionPolicy')
    --
    , daPeerMetrics         :: PeerMetrics m ntnAddr

    -- | Used by churn-governor
    --
    , daBlockFetchMode      :: STM m FetchMode

    -- | Used for peer sharing protocol
    --
    , daPeerSharingRegistry :: PeerSharingRegistry ntnAddr m
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
data ConnectionManagerDataInMode peerAddr versionData m a (mode :: MuxMode) where
    CMDInInitiatorMode
      :: ConnectionManagerDataInMode peerAddr versionData m a InitiatorMode

    CMDInInitiatorResponderMode
      :: InboundGovernorInfoChannel InitiatorResponderMode
                                    (ExpandedInitiatorContext peerAddr m)
                                    peerAddr versionData ByteString  m a ()
      -> OutboundGovernorInfoChannel peerAddr m
      -> StrictTVar m Server.InboundGovernorObservableState
      -> ConnectionManagerDataInMode peerAddr versionData m a InitiatorResponderMode


--
-- Node-To-Client type aliases
--
-- Node-To-Client diffusion is only used in 'ResponderMode'.
--

type NodeToClientHandle ntcAddr versionData m =
    HandleWithMinimalCtx ResponderMode ntcAddr versionData ByteString m Void ()

type NodeToClientHandleError ntcVersion =
    HandleError ResponderMode ntcVersion

type NodeToClientConnectionHandler
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionHandler
      ResponderMode
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr ntcVersionData m)
      (NodeToClientHandleError ntcVersion)
      (ntcVersion, ntcVersionData)
      m

type NodeToClientConnectionManagerArguments
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr ntcVersionData m)
      (NodeToClientHandleError ntcVersion)
      ntcVersion
      ntcVersionData
      m

type NodeToClientConnectionManager
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionManager
      ResponderMode
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr ntcVersionData m)
      (NodeToClientHandleError ntcVersion)
      m

--
-- Node-To-Node type aliases
--
-- Node-To-Node diffusion runs in either 'InitiatorMode' or 'InitiatorResponderMode'.
--

type NodeToNodeHandle
       (mode :: MuxMode)
       ntnAddr ntnVersionData m a b =
    HandleWithExpandedCtx mode ntnAddr ntnVersionData ByteString m a b

type NodeToNodeConnectionHandler
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData m a b =
    ConnectionHandler
      mode
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr ntnVersionData m a b)
      (HandleError mode ntnVersion)
      (ntnVersion, ntnVersionData)
      m

type NodeToNodeConnectionManagerArguments
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersion ntnVersionData m a b =
    ConnectionManagerArguments
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr ntnVersionData m a b)
      (HandleError mode ntnVersion)
      ntnVersion
      ntnVersionData
      m

type NodeToNodeConnectionManager
       (mode :: MuxMode)
       ntnFd ntnAddr ntnVersionData ntnVersion m a b =
    ConnectionManager
      mode
      ntnFd
      ntnAddr
      (NodeToNodeHandle mode ntnAddr ntnVersionData m a b)
      (HandleError mode ntnVersion)
      m

--
-- Governor type aliases
--

type NodeToNodePeerConnectionHandle (mode :: MuxMode) ntnAddr ntnVersionData m a b =
    PeerConnectionHandle
      mode
      (ResponderContext ntnAddr)
      ntnAddr
      ntnVersionData
      ByteString
      m a b

type NodeToNodePeerStateActions (mode :: MuxMode) ntnAddr ntnVersionData m a b =
    Governor.PeerStateActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr ntnVersionData m a b)
      m

type NodeToNodePeerSelectionActions (mode :: MuxMode) ntnAddr ntnVersionData m a b =
    Governor.PeerSelectionActions
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr ntnVersionData m a b)
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

        -- | node-to-node 'Mx.MakeBearer' callback
        --
        diNtnBearer
          :: Mx.MakeBearer m ntnFd,

        -- | node-to-node socket configuration
        --
        diNtnConfigureSocket
          :: ntnFd -> Maybe ntnAddr -> m (),

        -- | node-to-node systemd socket configuration
        --
        diNtnConfigureSystemdSocket
          :: ntnFd -> ntnAddr -> m (),

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

        -- | peer sharing information used by peer selection governor to
        -- decide which peers are available for performing peer sharing
        diNtnPeerSharing
          :: ntnVersionData -> PeerSharing,

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

        -- | node-to-client 'Mx.MakeBearer' callback
        --
        diNtcBearer
          :: Mx.MakeBearer m ntcFd,

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
          :: forall mode x y.
             NodeToNodeConnectionManager mode ntnFd ntnAddr ntnVersionData ntnVersion  m x y
          -> m (),

        -- | diffusion dns actions
        --
        diDnsActions
          :: LookupReqs -> DNSActions resolver resolverError m,

        -- | DNS Semaphore
        --
        diLocalAndPublicRootDnsSemaphore
          :: DNSSemaphore m
      }

runM
    :: forall m ntnFd ntnAddr ntnVersion ntnVersionData
                ntcFd ntcAddr ntcVersion ntcVersionData
                resolver resolverError a.
       ( Alternative (STM m)
       , MonadAsync       m
       , MonadDelay       m
       , MonadEvaluate    m
       , MonadFix         m
       , MonadFork        m
       , MonadLabelledSTM m
       , MonadTraceSTM    m
       , MonadMask        m
       , MonadThrow  (STM m)
       , MonadTime        m
       , MonadTimer       m
       , MonadMVar        m
       , Eq (Async m Void)
       , Typeable  ntnAddr
       , Ord       ntnAddr
       , Show      ntnAddr
       , Typeable  ntnVersion
       , Ord       ntnVersion
       , Show      ntnVersion
       , Show      ntnVersionData
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
                    m a
    -> -- | p2p protocol handlers
       ApplicationsExtra ntnAddr m a
    -> m Void
runM Interfaces
       { diNtnSnocket
       , diNtnBearer
       , diNtnConfigureSocket
       , diNtnConfigureSystemdSocket
       , diNtnHandshakeArguments
       , diNtnAddressType
       , diNtnDataFlow
       , diNtnPeerSharing
       , diNtnToPeerAddr
       , diNtnDomainResolver
       , diNtcSnocket
       , diNtcBearer
       , diNtcHandshakeArguments
       , diNtcGetFileDescriptor
       , diRng
       , diInstallSigUSR1Handler
       , diDnsActions
       , diLocalAndPublicRootDnsSemaphore
       }
     Tracers
       { dtMuxTracer
       , dtLocalMuxTracer
       , dtLedgerPeersTracer
       , dtDiffusionTracer = tracer
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
       , daOwnPeerSharing
       , daReadUseLedgerAfter
       , daProtocolIdleTimeout
       , daTimeWaitTimeout
       , daDeadlineChurnInterval
       , daBulkChurnInterval
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
       , daReturnPolicy
       , daPeerMetrics
       , daBlockFetchMode
       , daPeerSharingRegistry
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

    --
    -- remote connection manager
    --
    let remoteThread :: m Void
        remoteThread = do
          lookupReqs <- case (cmIPv4Address, cmIPv6Address) of
                               (Just _ , Nothing) -> return LookupReqAOnly
                               (Nothing, Just _ ) -> return LookupReqAAAAOnly
                               (Just _ , Just _ ) -> return LookupReqAAndAAAA
                               (Nothing, Nothing) -> throwIO NoSocket

          -- control channel for the server; only required in
          -- @'InitiatorResponderMode' :: 'MuxMode'@
          cmdInMode
            <- case diffusionMode of
                InitiatorOnlyDiffusionMode ->
                  -- action which we pass to connection handler
                  pure (HasInitiator CMDInInitiatorMode)
                InitiatorAndResponderDiffusionMode ->
                  HasInitiatorResponder <$>
                    (CMDInInitiatorResponderMode
                      <$> newInformationChannel
                      <*> newInformationChannel
                      <*> Server.newObservableStateVar ntnInbgovRng)

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


          publicStateVar <- newTVarIO emptyPublicPeerSelectionState

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
                           m a Void
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
                          cmMakeBearer          = diNtnBearer,
                          cmConfigureSocket     = diNtnConfigureSocket,
                          connectionDataFlow    = diNtnDataFlow,
                          cmPrunePolicy         = simplePrunePolicy,

                          -- Server is not running, it will not be able to
                          -- advise which connections to prune.  It's also not
                          -- expected that the governor targets will be larger
                          -- than limits imposed by 'cmConnectionsLimits'.
                          cmConnectionsLimits   = daAcceptedConnectionsLimit,
                          cmTimeWaitTimeout     = daTimeWaitTimeout,
                          cmOutboundIdleTimeout = daProtocolIdleTimeout,
                          cmGetPeerSharing      = diNtnPeerSharing
                        }

                    connectionHandler
                      :: NodeToNodeConnectionHandler
                           InitiatorMode
                           ntnFd ntnAddr ntnVersion ntnVersionData
                           m a Void
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
                  NotInResponderMode
                  $ \(connectionManager
                      :: NodeToNodeConnectionManager
                           InitiatorMode ntnFd ntnAddr ntnVersionData ntnVersion m a Void)
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
                        spsConnectionManager = connectionManager,
                        spsExitPolicy = exitPolicy
                      }
                    $ \(peerStateActions
                          :: NodeToNodePeerStateActions InitiatorMode ntnAddr versionData m a Void) ->
                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      diNtnToPeerAddr
                      diLocalAndPublicRootDnsSemaphore
                      (diDnsActions lookupReqs)
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      daOwnPeerSharing
                      (pchPeerSharing diNtnPeerSharing)
                      (readTVar (getPeerSharingRegistry daPeerSharingRegistry))
                      retry -- Will never receive inbound connections
                      peerStateActions
                      requestLedgerPeers
                      $ \localPeerSelectionActionsThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorMode ntnAddr versionData m a Void) ->

                        Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorTracer
                          dtTracePeerSelectionCounters
                          fuzzRng
                          publicStateVar
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy
                             policyRngVar (readTVar churnModeVar)
                             daPeerMetrics (epErrorDelay exitPolicy)))
                        $ \governorThread ->
                          Async.withAsync
                          (Governor.peerChurnGovernor
                            dtTracePeerSelectionTracer
                            daDeadlineChurnInterval
                            daBulkChurnInterval
                            daPeerMetrics
                            churnModeVar
                            churnRng
                            daBlockFetchMode
                            daPeerSelectionTargets
                            peerSelectionTargetsVar)
                          $ \churnGovernorThread ->
                                -- wait for any thread to fail
                                snd <$> Async.waitAny
                                   [ localPeerSelectionActionsThread
                                   , governorThread
                                   , ledgerPeerThread
                                   , churnGovernorThread
                                   ]

              -- InitiatorResponderMode
              --
              -- Run peer selection and the server.
              --
              HasInitiatorResponder
                (CMDInInitiatorResponderMode inboundInfoChannel outboundInfoChannel observableStateVar) -> do
                let connectionManagerArguments
                      :: NodeToNodeConnectionManagerArguments
                          InitiatorResponderMode
                          ntnFd ntnAddr ntnVersion ntnVersionData
                          m a ()
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
                          cmMakeBearer          = diNtnBearer,
                          cmConfigureSocket     = diNtnConfigureSocket,
                          connectionDataFlow    = diNtnDataFlow,
                          cmPrunePolicy         = Diffusion.Policies.prunePolicy observableStateVar,
                          cmConnectionsLimits   = daAcceptedConnectionsLimit,
                          cmTimeWaitTimeout     = daTimeWaitTimeout,
                          cmOutboundIdleTimeout = daProtocolIdleTimeout,
                          cmGetPeerSharing      = diNtnPeerSharing
                        }

                    connectionHandler
                      :: NodeToNodeConnectionHandler
                          InitiatorResponderMode
                          ntnFd ntnAddr ntnVersion ntnVersionData
                          m a ()
                    connectionHandler =
                      makeConnectionHandler
                         dtMuxTracer
                         SingInitiatorResponderMode
                         diNtnHandshakeArguments
                         (daApplicationInitiatorResponderMode
                            (computePeerSharingPeers (readTVar publicStateVar) peerSharingRng))
                         (mainThreadId, rethrowPolicy <> daRethrowPolicy)

                withConnectionManager
                  connectionManagerArguments
                  connectionHandler
                  classifyHandleError
                  (InResponderMode inboundInfoChannel)
                  (InResponderMode outboundInfoChannel)
                  $ \(connectionManager
                        :: NodeToNodeConnectionManager
                             InitiatorResponderMode ntnFd ntnAddr ntnVersionData ntnVersion m a ()
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
                        spsConnectionManager = connectionManager,
                        spsExitPolicy = stdExitPolicy daReturnPolicy
                      }
                    $ \(peerStateActions
                          :: NodeToNodePeerStateActions
                               InitiatorResponderMode ntnAddr versionData m a ()) ->

                    --
                    -- Run peer selection (p2p governor)
                    --

                    withPeerSelectionActions
                      dtTraceLocalRootPeersTracer
                      dtTracePublicRootPeersTracer
                      diNtnToPeerAddr
                      diLocalAndPublicRootDnsSemaphore
                      (diDnsActions lookupReqs)
                      (readTVar peerSelectionTargetsVar)
                      daReadLocalRootPeers
                      daReadPublicRootPeers
                      daOwnPeerSharing
                      (pchPeerSharing diNtnPeerSharing)
                      (readTVar (getPeerSharingRegistry daPeerSharingRegistry))
                      (readMessage outboundInfoChannel)
                      peerStateActions
                      requestLedgerPeers
                      $ \localPeerRootProviderThread
                        (peerSelectionActions
                           :: NodeToNodePeerSelectionActions
                                InitiatorResponderMode ntnAddr versionData m a ()) ->

                      Async.withAsync
                        (Governor.peerSelectionGovernor
                          dtTracePeerSelectionTracer
                          dtDebugPeerSelectionInitiatorResponderTracer
                          dtTracePeerSelectionCounters
                          fuzzRng
                          publicStateVar
                          peerSelectionActions
                          (Diffusion.Policies.simplePeerSelectionPolicy
                             policyRngVar (readTVar churnModeVar)
                             daPeerMetrics (epErrorDelay exitPolicy)))
                        $ \governorThread ->
                          withSockets tracer diNtnSnocket
                                      (\sock addr -> diNtnConfigureSocket sock (Just addr))
                                      (\sock addr -> diNtnConfigureSystemdSocket sock addr)
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
                                  serverInboundIdleTimeout    = Just daProtocolIdleTimeout,
                                  serverInboundInfoChannel    = inboundInfoChannel,
                                  serverObservableStateVar    = observableStateVar
                                })
                                $ \serverThread ->
                                  Async.withAsync
                                    (Governor.peerChurnGovernor
                                      dtTracePeerSelectionTracer
                                      daDeadlineChurnInterval
                                      daBulkChurnInterval
                                      daPeerMetrics
                                      churnModeVar
                                      churnRng
                                      daBlockFetchMode
                                      daPeerSelectionTargets
                                      peerSelectionTargetsVar)
                                    $ \churnGovernorThread ->

                                      -- wait for any thread to fail
                                      snd <$> Async.waitAny
                                           [ localPeerRootProviderThread
                                           , serverThread
                                           , governorThread
                                           , ledgerPeerThread
                                           , churnGovernorThread
                                           ]

    Async.runConcurrently
      $ asum
      $ Async.Concurrently <$>
          ( remoteThread
          : maybeToList (mkLocalThread' mainThreadId)
          )

  where
    (ledgerPeersRng, rng1) = split diRng
    (policyRng,      rng2) = split rng1
    (churnRng,       rng3) = split rng2
    (fuzzRng,        rng4) = split rng3
    (ntnInbgovRng,   rng5) = split rng4
    (ntcInbgovRng,   peerSharingRng) = split rng5

    -- Only the 'IOManagerError's are fatal, all the other exceptions in the
    -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
    -- this is so).
    rethrowPolicy =
      RethrowPolicy $ \_ctx err ->
        case fromException err of
          Just (_ :: IOManagerError) -> ShutdownNode
          Nothing                    -> mempty

    exitPolicy :: ExitPolicy a
    exitPolicy = stdExitPolicy daReturnPolicy

    computePeerSharingPeers :: STM m (PublicPeerSelectionState ntnAddr)
                            -> StdGen
                            -> PeerSharingAmount
                            -> m [ntnAddr]
    computePeerSharingPeers readPublicState gen amount = do
      publicState <- atomically readPublicState
      let availableToShareSet = availableToShare
                              $ publicState
          is = nub
             $ take (fromIntegral amount)
             $ randomRs (0, length availableToShareSet - 1) gen
          randomList = map (`elemAt` availableToShareSet) is
      if null availableToShareSet
         then return []
         else return randomList

    -- | mkLocalThread - create local connection manager

    mkLocalThread' :: ThreadId m -> Maybe (m Void)
    mkLocalThread' mainThreadId =
      mkLocalThread mainThreadId <$> daLocalAddress

    mkLocalThread :: ThreadId m -> Either ntcFd ntcAddr -> m Void
    mkLocalThread mainThreadId localAddr =
      withLocalSocket tracer diNtcGetFileDescriptor diNtcSnocket localAddr
          $ \localSocket -> do
            localInbInfoChannel <- newInformationChannel
            localOutInfoChannel <- newInformationChannel
            localServerStateVar <- Server.newObservableStateVar ntcInbgovRng

            let localConnectionLimits = AcceptedConnectionsLimit maxBound maxBound 0

                localConnectionHandler :: NodeToClientConnectionHandler
                                            ntcFd ntcAddr ntcVersion ntcVersionData m
                localConnectionHandler =
                  makeConnectionHandler
                    dtLocalMuxTracer
                    SingResponderMode
                    diNtcHandshakeArguments
                    ( ( \ (OuroborosApplication apps)
                       -> TemperatureBundle
                            (WithHot apps)
                            (WithWarm [])
                            (WithEstablished [])
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
                      cmMakeBearer          = diNtcBearer,
                      cmConfigureSocket     = \_ _ -> return (),
                      cmTimeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                      cmOutboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                      connectionDataFlow    = localDataFlow,
                      cmPrunePolicy         = Diffusion.Policies.prunePolicy
                                                localServerStateVar,
                      cmConnectionsLimits   = localConnectionLimits,

                      -- local thread does not start a Outbound Governor
                      -- so it doesn't matter what we put here.
                      -- 'NoPeerSharing' is set for all connections.
                      cmGetPeerSharing = \_ -> NoPeerSharing
                    }

            withConnectionManager
              localConnectionManagerArguments
              localConnectionHandler
              classifyHandleError
              (InResponderMode localInbInfoChannel)
              (InResponderMode localOutInfoChannel)
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
                        serverInboundIdleTimeout    = Nothing,
                        serverConnectionLimits      = localConnectionLimits,
                        serverConnectionManager     = localConnectionManager,
                        serverInboundInfoChannel    = localInbInfoChannel,
                        serverObservableStateVar    = localServerStateVar
                      }) Async.wait



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
         IO a
    -> ApplicationsExtra RemoteAddress IO a
    -> IO Void
run tracers tracersExtra args argsExtra apps appsExtra = do
    let tracer = dtDiffusionTracer tracers
    -- We run two services: for /node-to-node/ and /node-to-client/.  The
    -- naming convention is that we use /local/ prefix for /node-to-client/
    -- related terms, as this is a local only service running over a unix
    -- socket / windows named pipe.
    handleJust (\e -> case fromException e :: Maybe ExitCode of
                  Nothing -> Just e
                  Just {} -> Nothing)
               (\e -> traceWith tracer (DiffusionErrored e)
                   >> throwIO (DiffusionError e))
         $ withIOManager $ \iocp -> do
             let diNtnHandshakeArguments =
                   HandshakeArguments {
                       haHandshakeTracer = dtHandshakeTracer tracers,
                       haHandshakeCodec  = NodeToNode.nodeToNodeHandshakeCodec,
                       haVersionDataCodec =
                         cborTermVersionDataCodec
                           NodeToNode.nodeToNodeCodecCBORTerm,
                       haAcceptVersion = acceptableVersion,
                       haQueryVersion = queryVersion,
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
                       haQueryVersion = queryVersion,
                       haTimeLimits = noTimeLimitsHandshake
                     }

                 diInstallSigUSR1Handler
                   :: forall mode x y.
                      NodeToNodeConnectionManager mode Socket RemoteAddress
                                                  NodeToNodeVersionData NodeToNodeVersion IO x y
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

             diLocalAndPublicRootDnsSemaphore <- newLocalAndPublicRootDNSSemaphore
             let diNtnDomainResolver :: LookupReqs -> [DomainAccessPoint]
                                     -> IO (Map DomainAccessPoint (Set Socket.SockAddr))
                 diNtnDomainResolver lr =
                   resolveDomainAccessPoint
                     (dtTracePublicRootPeersTracer tracersExtra)
                     diLocalAndPublicRootDnsSemaphore
                     DNS.defaultResolvConf
                     (ioDNSActions lr)

             diRng <- newStdGen
             runM
               Interfaces {
                 diNtnSnocket = Snocket.socketSnocket iocp,
                 diNtnBearer = makeSocketBearer,
                 diNtnConfigureSocket = configureSocket,
                 diNtnConfigureSystemdSocket =
                   configureSystemdSocket
                     (SystemdSocketConfiguration `contramap` tracer),
                 diNtnHandshakeArguments,
                 diNtnAddressType = socketAddressType,
                 diNtnDataFlow = nodeDataFlow,
                 diNtnPeerSharing = peerSharing,
                 diNtnToPeerAddr = curry IP.toSockAddr,
                 diNtnDomainResolver,

                 diNtcSnocket = Snocket.localSnocket iocp,
                 diNtcBearer = makeLocalBearer,
                 diNtcHandshakeArguments,
                 diNtcGetFileDescriptor = localSocketFileDescriptor,

                 diRng,
                 diInstallSigUSR1Handler,
                 diDnsActions = ioDNSActions,
                 diLocalAndPublicRootDnsSemaphore
               }
               tracers tracersExtra args argsExtra apps appsExtra

--
-- Data flow
--

-- | For Node-To-Node protocol, any connection which negotiated at least
-- 'NodeToNodeV_9' version and did not declare 'InitiatorOnlyDiffusionMode'
-- will run in 'Duplex' mode.   All connections from lower versions or one that
-- declared themselves as 'InitiatorOnly' will run in 'UnidirectionalMode'
--
nodeDataFlow :: NodeToNodeVersion
             -> NodeToNodeVersionData
             -> DataFlow
nodeDataFlow v NodeToNodeVersionData { diffusionMode = InitiatorAndResponderDiffusionMode }
                 | v >= NodeToNodeV_10
                 = Duplex
nodeDataFlow _ _ = Unidirectional


-- | For Node-To-Client protocol all connection are considered 'Unidirectional'.
--
localDataFlow :: ntcVersion
              -> ntcVersionData
              -> DataFlow
localDataFlow _ _ = Unidirectional
