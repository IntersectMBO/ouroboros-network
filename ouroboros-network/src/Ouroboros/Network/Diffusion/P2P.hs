{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeOperators            #-}

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
  , isFatal
    -- * Re-exports
  , AbstractTransitionTrace
  , RemoteTransitionTrace
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync (Async, MonadAsync)
import Control.Monad.Class.MonadAsync qualified as Async
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.Fix (MonadFix)
import Control.Tracer (Tracer, contramap, nullTracer, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.IP (IP)
import Data.IP qualified as IP
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.IO.Exception (IOException (..), IOErrorType (..))
import System.Exit (ExitCode)
import System.Random (StdGen, newStdGen, split)
#ifdef POSIX
import System.Posix.Signals qualified as Signals
#endif

import Network.Socket (Socket)
import Network.Socket qualified as Socket

import Network.Mux qualified as Mx

import Ouroboros.Network.Snocket (FileDescriptor, LocalAddress,
           LocalSocket (..), Snocket, localSocketFileDescriptor,
           makeLocalBearer, makeSocketBearer)
import Ouroboros.Network.Snocket qualified as Snocket

import Ouroboros.Network.BlockFetch.ConsensusInterface (FetchMode)
import Ouroboros.Network.ConnectionId
import Ouroboros.Network.Context (ExpandedInitiatorContext, ResponderContext)
import Ouroboros.Network.Protocol.Handshake
import Ouroboros.Network.Protocol.Handshake.Codec
import Ouroboros.Network.Protocol.Handshake.Version
import Ouroboros.Network.Socket (configureSocket, configureSystemdSocket)

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.InformationChannel
           (newInformationChannel)
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Diffusion.Common hiding (nullTracers)
import Ouroboros.Network.Diffusion.Policies qualified as Diffusion.Policies
import Ouroboros.Network.Diffusion.Utils
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.InboundGovernor (RemoteTransitionTrace)
import Ouroboros.Network.InboundGovernor qualified as InboundGovernor
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux hiding (MiniProtocol (..))
import Ouroboros.Network.MuxMode
import Ouroboros.Network.NodeToClient (NodeToClientVersion (..),
           NodeToClientVersionData)
import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit (..),
           DiffusionMode (..), NodeToNodeVersion (..),
           NodeToNodeVersionData (..), RemoteAddress)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Ouroboros.Network.PeerSelection.Churn (PeerChurnArgs (..))
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.Governor.Types
           (ChurnMode (ChurnModeNormal), ConsensusModePeerTargets (..),
           DebugPeerSelection (..), PeerSelectionActions, PeerSelectionCounters,
           PeerSelectionInterfaces (..), PeerSelectionPolicy (..),
           PeerSelectionState, TracePeerSelection (..),
           emptyPeerSelectionCounters, emptyPeerSelectionState)
#ifdef POSIX
import Ouroboros.Network.PeerSelection.Governor.Types
           (makeDebugPeerSelectionState)
#endif
import Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers,
           WithLedgerPeersArgs (..))
#ifdef POSIX
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           LedgerPeersConsensusInterface (..), MinBigLedgerPeersForTrustedState,
           UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
           fetchynessBlocks, upstreamyness)
#else
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           MinBigLedgerPeersForTrustedState, UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
#endif
import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.PeerSelection.PeerSelectionActions
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerStateActions (PeerConnectionHandle,
           PeerSelectionActionsTrace (..), PeerStateActionsArguments (..),
           pchPeerSharing, withPeerStateActions)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.DNSActions (DNSActions,
           DNSLookupType (..), ioDNSActions)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server2 qualified as Server

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

      -- | Ledger Peers tracer
    , dtTraceLedgerPeersTracer
        :: Tracer m TraceLedgerPeers

    , dtTracePeerSelectionTracer
        :: Tracer m (TracePeerSelection ntnAddr)

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer m (DebugPeerSelection ntnAddr)

      -- TODO: can be unified with the previous one
    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer m (DebugPeerSelection ntnAddr)

    , dtTracePeerSelectionCounters
        :: Tracer m PeerSelectionCounters

    , dtTraceChurnCounters
        :: Tracer m Governor.ChurnCounters

    , dtPeerSelectionActionsTracer
        :: Tracer m (PeerSelectionActionsTrace ntnAddr ntnVersion)

    , dtConnectionManagerTracer
        :: Tracer m (CM.Trace
                      ntnAddr
                      (ConnectionHandlerTrace
                         ntnVersion
                         ntnVersionData))

    , dtConnectionManagerTransitionTracer
        :: Tracer m (AbstractTransitionTrace CM.ConnStateId)

    , dtServerTracer
        :: Tracer m (Server.Trace ntnAddr)

    , dtInboundGovernorTracer
        :: Tracer m (InboundGovernor.Trace ntnAddr)

    , dtInboundGovernorTransitionTracer
        :: Tracer m (RemoteTransitionTrace ntnAddr)

      --
      -- NodeToClient tracers
      --

      -- | Connection manager tracer for local clients
    , dtLocalConnectionManagerTracer
        :: Tracer m (CM.Trace
                       ntcAddr
                       (ConnectionHandlerTrace
                          ntcVersion
                          ntcVersionData))

      -- | Server tracer for local clients
    , dtLocalServerTracer
        :: Tracer m (Server.Trace ntcAddr)

      -- | Inbound protocol governor tracer for local clients
    , dtLocalInboundGovernorTracer
        :: Tracer m (InboundGovernor.Trace ntcAddr)
    }

nullTracers :: Applicative m
            => TracersExtra ntnAddr ntnVersion ntnVersionData
                            ntcAddr ntcVersion ntcVersionData
                            resolverError m
nullTracers =
    TracersExtra {
        dtTraceLocalRootPeersTracer                  = nullTracer
      , dtTracePublicRootPeersTracer                 = nullTracer
      , dtTraceLedgerPeersTracer                     = nullTracer
      , dtTracePeerSelectionTracer                   = nullTracer
      , dtTraceChurnCounters                         = nullTracer
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
      daPeerTargets            :: ConsensusModePeerTargets

    , daReadLocalRootPeers     :: STM m (LocalRootPeers.Config RelayAccessPoint)
    , daReadPublicRootPeers    :: STM m (Map RelayAccessPoint PeerAdvertise)
    -- | When syncing up, ie. ledgerStateJudgement == TooOld,
    -- when this is True we will maintain connection with many big ledger peers
    -- to get a strong guarantee that when syncing up we will finish with a true
    -- ledger state. When false, we will fall back on the previous algorithms
    -- that leverage UseBootstrapPeers flag
    , daConsensusMode                    :: ConsensusMode
    -- | For Genesis, this sets the floor for minimum number of
    --   active big ledger peers we must be connected to in order
    --   to be able to signal trusted state (OutboundConnectionsState)
    , daMinBigLedgerPeersForTrustedState :: MinBigLedgerPeersForTrustedState
    , daReadUseBootstrapPeers            :: STM m UseBootstrapPeers
    -- | Depending on configuration, node may provide us with
    -- a snapshot of big ledger peers taken at some slot on the chain.
    -- These peers may be selected by ledgerPeersThread when requested
    -- by the peer selection governor when the node is syncing up.
    -- This is especially useful for Genesis consensus mode.
    , daReadLedgerPeerSnapshot :: STM m (Maybe LedgerPeerSnapshot)

    -- | Peer's own PeerSharing value.
    --
    -- This value comes from the node's configuration file and is static.
    , daOwnPeerSharing         :: PeerSharing
    , daReadUseLedgerPeers     :: STM m UseLedgerPeers

      -- | Timeout which starts once all responder protocols are idle. If the
      -- responders stay idle for duration of the timeout, the connection will
      -- be demoted, if it wasn't used by the p2p-governor it will be closed.
      --
      -- Applies to 'Unidirectional' as well as 'Duplex' /node-to-node/
      -- connections.
      --
      -- See 'serverProtocolIdleTimeout'.
      --
    , daProtocolIdleTimeout    :: DiffTime

      -- | Time for which /node-to-node/ connections are kept in
      -- 'TerminatingState', it should correspond to the OS configured @TCP@
      -- @TIME_WAIT@ timeout.
      --
      -- This timeout will apply to after a connection has been closed, its
      -- purpose is to be resilient for delayed packets in the same way @TCP@
      -- is using @TIME_WAIT@.
      --
    , daTimeWaitTimeout        :: DiffTime

      -- | Churn interval between churn events in deadline mode.  A small fuzz
      -- is added (max 10 minutes) so that not all nodes churn at the same time.
      --
      -- By default it is set to 3300 seconds.
      --
    , daDeadlineChurnInterval  :: DiffTime

      -- | Churn interval between churn events in bulk sync mode.  A small fuzz
      -- is added (max 1 minute) so that not all nodes churn at the same time.
      --
      -- By default it is set to 300 seconds.
      --
    , daBulkChurnInterval      :: DiffTime
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
socketAddressType Socket.SockAddrUnix {}  = Nothing


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


--
-- Node-To-Client type aliases
--
-- Node-To-Client diffusion is only used in 'ResponderMode'.
--

type NodeToClientHandle ntcAddr versionData m =
    HandleWithMinimalCtx Mx.ResponderMode ntcAddr versionData ByteString m Void ()

type NodeToClientHandleError ntcVersion =
    HandleError Mx.ResponderMode ntcVersion

type NodeToClientConnectionHandler
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    ConnectionHandler
      Mx.ResponderMode
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr ntcVersionData m)
      (NodeToClientHandleError ntcVersion)
      ntcVersion
      ntcVersionData
      m

type NodeToClientConnectionManagerArguments
      ntcFd ntcAddr ntcVersion ntcVersionData m =
    CM.Arguments
      (ConnectionHandlerTrace ntcVersion ntcVersionData)
      ntcFd
      ntcAddr
      (NodeToClientHandle ntcAddr ntcVersionData m)
      (NodeToClientHandleError ntcVersion)
      ntcVersion
      ntcVersionData
      m


--
-- Node-To-Node type aliases
--
-- Node-To-Node diffusion runs in either 'InitiatorMode' or 'InitiatorResponderMode'.
--

type NodeToNodeHandle
       (mode :: Mx.Mode)
       ntnAddr ntnVersionData m a b =
    HandleWithExpandedCtx mode ntnAddr ntnVersionData ByteString m a b

type NodeToNodeConnectionManager
       (mode :: Mx.Mode)
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

type NodeToNodePeerConnectionHandle (mode :: Mx.Mode) ntnAddr ntnVersionData m a b =
    PeerConnectionHandle
      mode
      (ResponderContext ntnAddr)
      ntnAddr
      ntnVersionData
      ByteString
      m a b

type NodeToNodePeerSelectionActions (mode :: Mx.Mode) ntnAddr ntnVersionData m a b =
    PeerSelectionActions
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
        -- It is used by both inbound and outbound connection.  The address is
        -- the local address that we can bind to if given (NOTE: for
        -- node-to-node connection `Just` is always given).
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
          :: ntnVersionData -> DataFlow,

        -- | remote side peer sharing information used by peer selection governor
        -- to decide which peers are available for performing peer sharing
        diNtnPeerSharing
          :: ntnVersionData -> PeerSharing,

        -- | node-to-node peer address
        --
        diNtnToPeerAddr
          :: IP -> Socket.PortNumber -> ntnAddr,

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
          -> StrictTVar m (PeerSelectionState ntnAddr (NodeToNodePeerConnectionHandle
                               mode ntnAddr ntnVersionData m x y))
          -> PeerMetrics m ntnAddr
          -> m (),

        -- | diffusion dns actions
        --
        diDnsActions
          :: DNSLookupType -> DNSActions resolver resolverError m,

        -- | Update `ntnVersionData` for initiator-only local roots.
        diUpdateVersionData
          :: ntnVersionData -> DiffusionMode -> ntnVersionData,

        -- | `ConnStateIdSupply` used by the connection-manager for this node.
        --
        -- This is exposed for testing, where we use a global
        -- `ConnStateIdSupply`.
        --
        diConnStateIdSupply
          :: CM.ConnStateIdSupply m
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
       , Typeable  ntnAddr
       , Ord       ntnAddr
       , Show      ntnAddr
       , Hashable  ntnAddr
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
       Arguments m ntnFd ntnAddr
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
       , diNtcSnocket
       , diNtcBearer
       , diNtcHandshakeArguments
       , diNtcGetFileDescriptor
       , diRng
       , diInstallSigUSR1Handler
       , diDnsActions
       , diUpdateVersionData
       , diConnStateIdSupply
       }
     Tracers
       { dtMuxTracer
       , dtLocalMuxTracer
       , dtDiffusionTracer = tracer
       }
     TracersExtra
       { dtTracePeerSelectionTracer
       , dtTraceChurnCounters
       , dtDebugPeerSelectionInitiatorTracer
       , dtDebugPeerSelectionInitiatorResponderTracer
       , dtTracePeerSelectionCounters
       , dtPeerSelectionActionsTracer
       , dtTraceLocalRootPeersTracer
       , dtTracePublicRootPeersTracer
       , dtTraceLedgerPeersTracer
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
       , daPublicPeerSelectionVar
       }
     ArgumentsExtra
       { daPeerTargets
       , daReadLocalRootPeers
       , daReadPublicRootPeers
       , daConsensusMode
       , daMinBigLedgerPeersForTrustedState
       , daReadUseBootstrapPeers
       , daOwnPeerSharing
       , daReadUseLedgerPeers
       , daProtocolIdleTimeout
       , daTimeWaitTimeout
       , daDeadlineChurnInterval
       , daBulkChurnInterval
       , daReadLedgerPeerSnapshot
       }
     Applications
       { daApplicationInitiatorMode
       , daApplicationInitiatorResponderMode
       , daLocalResponderApplication
       , daLedgerPeersCtx
       , daUpdateOutboundConnectionsState
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

    Async.runConcurrently
      $ asum
      $ Async.Concurrently <$>
          ( mkRemoteThread mainThreadId
          : maybeToList (mkLocalThread mainThreadId <$> daLocalAddress)
          )

  where
    (ledgerPeersRng, rng1) = split diRng
    (policyRng,      rng2) = split rng1
    (churnRng,       rng3) = split rng2
    (fuzzRng,        rng4) = split rng3
    (cmLocalStdGen,  rng5) = split rng4
    (cmStdGen1, cmStdGen2) = split rng5


    mkInboundPeersMap :: InboundGovernor.PublicState ntnAddr ntnVersionData
                      -> Map ntnAddr PeerSharing
    mkInboundPeersMap
      InboundGovernor.PublicState { InboundGovernor.inboundDuplexPeers }
      =
      Map.map diNtnPeerSharing inboundDuplexPeers

    -- TODO: this policy should also be used in `PeerStateActions` and
    -- `InboundGovernor` (when creating or accepting connections)
    rethrowPolicy =
      -- Only the 'IOManagerError's are fatal, all the other exceptions in the
      -- networking code will only shutdown the bearer (see 'ShutdownPeer' why
      -- this is so).
      RethrowPolicy (\_ctx err ->
        case fromException err of
          Just (_ :: IOManagerError) -> ShutdownNode
          Nothing                    -> mempty)
      <>
      RethrowPolicy (\_ctx err ->
        case fromException err of
          Just IOError { ioe_type } ->
            if isFatal ioe_type then ShutdownNode
                                else mempty
          Nothing -> mempty)
      <>
      RethrowPolicy (\ctx err -> case  (ctx, fromException err) of
                        -- mux unknown mini-protocol errors on the outbound
                        -- side are fatal, since this is misconfiguration of the
                        -- ouroboros-network stack.
                        (OutboundError, Just Mx.UnknownMiniProtocol {})
                          -> ShutdownNode
                        _ -> mempty)


    -- | mkLocalThread - create local connection manager

    mkLocalThread :: ThreadId m -> Either ntcFd ntcAddr -> m Void
    mkLocalThread mainThreadId localAddr =
      withLocalSocket tracer diNtcGetFileDescriptor diNtcSnocket localAddr
      $ \localSocket -> do
        localInbInfoChannel <- newInformationChannel

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
              CM.Arguments {
                  CM.tracer              = dtLocalConnectionManagerTracer,
                  CM.trTracer            = nullTracer, -- TODO: issue #3320
                  CM.muxTracer           = dtLocalMuxTracer,
                  CM.ipv4Address         = Nothing,
                  CM.ipv6Address         = Nothing,
                  CM.addressType         = const Nothing,
                  CM.snocket             = diNtcSnocket,
                  CM.makeBearer          = diNtcBearer,
                  CM.configureSocket     = \_ _ -> return (),
                  CM.timeWaitTimeout     = local_TIME_WAIT_TIMEOUT,
                  CM.outboundIdleTimeout = local_PROTOCOL_IDLE_TIMEOUT,
                  CM.connectionDataFlow    = ntcDataFlow,
                  CM.prunePolicy         = Diffusion.Policies.prunePolicy,
                  CM.stdGen              = cmLocalStdGen,
                  CM.connectionsLimits   = localConnectionLimits,
                  CM.updateVersionData   = \a _ -> a,
                  CM.connStateIdSupply   = diConnStateIdSupply
                }

        CM.with
          localConnectionManagerArguments
          localConnectionHandler
          classifyHandleError
          (InResponderMode localInbInfoChannel)
          $ \localConnectionManager-> do
            --
            -- node-to-client server
            --
            traceWith tracer . RunLocalServer
              =<< Snocket.getLocalAddr diNtcSnocket localSocket

            Server.with
              Server.Arguments {
                  Server.sockets               = localSocket :| [],
                  Server.snocket               = diNtcSnocket,
                  Server.tracer                = dtLocalServerTracer,
                  Server.trTracer              = nullTracer, -- TODO: issue #3320
                  Server.debugInboundGovernor  = nullTracer,
                  Server.inboundGovernorTracer = dtLocalInboundGovernorTracer,
                  Server.inboundIdleTimeout    = Nothing,
                  Server.connectionLimits      = localConnectionLimits,
                  Server.connectionManager     = localConnectionManager,
                  Server.connectionDataFlow    = ntcDataFlow,
                  Server.inboundInfoChannel    = localInbInfoChannel
                }
              (\inboundGovernorThread _ -> Async.wait inboundGovernorThread)


    -- | mkRemoteThread - create remote connection manager

    mkRemoteThread :: ThreadId m -> m Void
    mkRemoteThread mainThreadId = do
      let
        exitPolicy :: ExitPolicy a
        exitPolicy = stdExitPolicy daReturnPolicy

      ipv4Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    daIPv4Address
      case ipv4Address of
        Just addr | Just IPv4Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv4Address addr)
        Nothing   -> pure ()

      ipv6Address
        <- traverse (either (Snocket.getLocalAddr diNtnSnocket) pure)
                    daIPv6Address
      case ipv6Address of
        Just addr | Just IPv6Address <- diNtnAddressType addr
                  -> pure ()
                  | otherwise
                  -> throwIO (UnexpectedIPv6Address addr)
        Nothing   -> pure ()

      lookupReqs <- case (ipv4Address, ipv6Address) of
                           (Just _ , Nothing) -> return LookupReqAOnly
                           (Nothing, Just _ ) -> return LookupReqAAAAOnly
                           (Just _ , Just _ ) -> return LookupReqAAndAAAA
                           (Nothing, Nothing) -> throwIO NoSocket

      -- RNGs used for picking random peers from the ledger and for
      -- demoting/promoting peers.
      policyRngVar <- newTVarIO policyRng

      churnModeVar <- newTVarIO ChurnModeNormal

      localRootsVar <- newTVarIO mempty

      peerSelectionTargetsVar <- newTVarIO $
        case daConsensusMode of
          PraosMode   -> deadlineTargets daPeerTargets
          GenesisMode -> syncTargets daPeerTargets

      countersVar <- newTVarIO emptyPeerSelectionCounters

      -- Design notes:
      --  - We split the following code into two parts:
      --    - Part (a): plumb data flow (in particular arguments and tracersr)
      --      and define common functions as a sequence of 'let's in which we
      --      define needed 'withXXX' functions (and similar) which
      --       - are used in Part (b),
      --       - handle the plumbing of tracers, and
      --       - capture commonalities between the two cases.
      --
      --    - Part (b): capturing the major control-flow of runM:
      --      in particular, two different case alternatives in which is captured
      --      the monadic flow of the program stripped down to its essence:
      ---     ```
      --       <setup...>
      --       case diffusionMode of
      --         InitiatorOnlyDiffusionMode -> ...
      --         InitiatorAndResponderDiffusionMode -> ...
      --      ```

      --
      -- Part (a): plumb data flow and define common functions
      --

      let connectionManagerArguments'
            :: forall handle handleError.
               PrunePolicy ntnAddr
            -> StdGen
            -> CM.Arguments
                 (ConnectionHandlerTrace ntnVersion ntnVersionData)
                 ntnFd ntnAddr handle handleError ntnVersion ntnVersionData m
          connectionManagerArguments' prunePolicy stdGen =
            CM.Arguments {
                CM.tracer              = dtConnectionManagerTracer,
                CM.trTracer            =
                  fmap CM.abstractState
                  `contramap` dtConnectionManagerTransitionTracer,
                CM.muxTracer           = dtMuxTracer,
                CM.ipv4Address,
                CM.ipv6Address,
                CM.addressType         = diNtnAddressType,
                CM.snocket             = diNtnSnocket,
                CM.makeBearer          = diNtnBearer,
                CM.configureSocket     = diNtnConfigureSocket,
                CM.connectionDataFlow    = diNtnDataFlow,
                CM.prunePolicy         = prunePolicy,
                CM.stdGen,
                CM.connectionsLimits   = daAcceptedConnectionsLimit,
                CM.timeWaitTimeout     = daTimeWaitTimeout,
                CM.outboundIdleTimeout = daProtocolIdleTimeout,
                CM.updateVersionData   = diUpdateVersionData,
                CM.connStateIdSupply   = diConnStateIdSupply
              }

      let peerSelectionPolicy = Diffusion.Policies.simplePeerSelectionPolicy
                                  policyRngVar (readTVar churnModeVar)
                                  daPeerMetrics (epErrorDelay exitPolicy)

      let makeConnectionHandler'
            :: forall muxMode socket initiatorCtx responderCtx b c.
               SingMuxMode muxMode
            -> Versions ntnVersion ntnVersionData
                 (OuroborosBundle muxMode initiatorCtx responderCtx ByteString m b c)
            -> MuxConnectionHandler
                 muxMode socket initiatorCtx responderCtx ntnAddr
                 ntnVersion ntnVersionData ByteString m b c
          makeConnectionHandler' muxMode versions =
            makeConnectionHandler
              dtMuxTracer
              muxMode
              diNtnHandshakeArguments
              versions
              (mainThreadId, rethrowPolicy <> daRethrowPolicy)

          -- | Capture the two variations (InitiatorMode,InitiatorResponderMode) of
          --   withConnectionManager:

          withConnectionManagerInitiatorOnlyMode =
            CM.with
              (connectionManagerArguments' simplePrunePolicy cmStdGen1)
                 -- Server is not running, it will not be able to
                 -- advise which connections to prune.  It's also not
                 -- expected that the governor targets will be larger
                 -- than limits imposed by 'cmConnectionsLimits'.
              (makeConnectionHandler'
                SingInitiatorMode
                daApplicationInitiatorMode)
              classifyHandleError
              NotInResponderMode

          withConnectionManagerInitiatorAndResponderMode
            inbndInfoChannel =
              CM.with
                (connectionManagerArguments' Diffusion.Policies.prunePolicy cmStdGen2)
                (makeConnectionHandler'
                   SingInitiatorResponderMode
                   daApplicationInitiatorResponderMode)
                classifyHandleError
                (InResponderMode inbndInfoChannel)

      --
      -- peer state actions
      --
      -- Peer state actions run a job pool in the background which
      -- tracks threads forked by 'PeerStateActions'
      --

      let -- | parameterized version of 'withPeerStateActions'
          withPeerStateActions'
            :: forall (muxMode :: Mx.Mode) responderCtx socket b c.
               HasInitiator muxMode ~ True
            => MuxConnectionManager
                 muxMode socket (ExpandedInitiatorContext ntnAddr m)
                 responderCtx ntnAddr ntnVersionData ntnVersion
                 ByteString m a b
            -> (Governor.PeerStateActions
                  ntnAddr
                  (PeerConnectionHandle muxMode responderCtx ntnAddr
                     ntnVersionData ByteString m a b)
                  m
                -> m c)
            -> m c
          withPeerStateActions' connectionManager =
            withPeerStateActions
              PeerStateActionsArguments {
                    spsTracer = dtPeerSelectionActionsTracer,
                    spsDeactivateTimeout = Diffusion.Policies.deactivateTimeout,
                    spsCloseConnectionTimeout =
                      Diffusion.Policies.closeConnectionTimeout,
                    spsConnectionManager = connectionManager,
                    spsExitPolicy = exitPolicy,
                    spsRethrowPolicy = rethrowPolicy,
                    spsMainThreadId = mainThreadId
                  }

      dnsSemaphore <- newLedgerAndPublicRootDNSSemaphore
      --
      -- Run peer selection (p2p governor)
      --
      let withPeerSelectionActions'
            :: forall muxMode responderCtx bytes a1 b c.
               m (Map ntnAddr PeerSharing)
            -> PeerSelectionActionsDiffusionMode ntnAddr (PeerConnectionHandle muxMode responderCtx ntnAddr ntnVersionData bytes m a1 b) m
            -> (   (Async m Void, Async m Void)
                -> PeerSelectionActions
                     ntnAddr
                     (PeerConnectionHandle
                        muxMode responderCtx ntnAddr ntnVersionData bytes m a1 b)
                      m
                -> m c)
            -- ^ continuation, receives a handle to the local roots peer provider thread
            -- (only if local root peers were non-empty).
            -> m c
          withPeerSelectionActions' readInboundPeers =
              withPeerSelectionActions localRootsVar PeerActionsDNS {
                                         paToPeerAddr = diNtnToPeerAddr,
                                         paDnsActions = diDnsActions lookupReqs,
                                         paDnsSemaphore = dnsSemaphore }
                                       PeerSelectionActionsArgs {
                                         psLocalRootPeersTracer = dtTraceLocalRootPeersTracer,
                                         psPublicRootPeersTracer = dtTracePublicRootPeersTracer,
                                         psReadTargets = readTVar peerSelectionTargetsVar,
                                         getLedgerStateCtx = daLedgerPeersCtx,
                                         psReadLocalRootPeers = daReadLocalRootPeers,
                                         psReadPublicRootPeers = daReadPublicRootPeers,
                                         psReadUseBootstrapPeers = daReadUseBootstrapPeers,
                                         psPeerSharing = daOwnPeerSharing,
                                         psPeerConnToPeerSharing = pchPeerSharing diNtnPeerSharing,
                                         psReadPeerSharingController = readTVar (getPeerSharingRegistry daPeerSharingRegistry),
                                         psReadInboundPeers =
                                           case daOwnPeerSharing of
                                             PeerSharingDisabled -> pure Map.empty
                                             PeerSharingEnabled  -> readInboundPeers,
                                         psUpdateOutboundConnectionsState = daUpdateOutboundConnectionsState,
                                         peerTargets = daPeerTargets,
                                         readLedgerPeerSnapshot = daReadLedgerPeerSnapshot }
                                       WithLedgerPeersArgs {
                                         wlpRng = ledgerPeersRng,
                                         wlpConsensusInterface = daLedgerPeersCtx,
                                         wlpTracer = dtTraceLedgerPeersTracer,
                                         wlpGetUseLedgerPeers = daReadUseLedgerPeers,
                                         wlpGetLedgerPeerSnapshot = daReadLedgerPeerSnapshot }

          peerSelectionGovernor'
            :: forall (muxMode :: Mx.Mode) b.
               Tracer m (DebugPeerSelection ntnAddr)
            -> StrictTVar m (PeerSelectionState ntnAddr
                              (NodeToNodePeerConnectionHandle
                               muxMode ntnAddr ntnVersionData m a b))
            -> NodeToNodePeerSelectionActions muxMode ntnAddr ntnVersionData m a b
            -> m Void
          peerSelectionGovernor' peerSelectionTracer dbgVar peerSelectionActions =
            Governor.peerSelectionGovernor
              dtTracePeerSelectionTracer
              peerSelectionTracer
              dtTracePeerSelectionCounters
              fuzzRng
              daConsensusMode
              daMinBigLedgerPeersForTrustedState
              peerSelectionActions
              peerSelectionPolicy
              PeerSelectionInterfaces {
                countersVar,
                publicStateVar     = daPublicPeerSelectionVar,
                debugStateVar      = dbgVar,
                readUseLedgerPeers = daReadUseLedgerPeers
              }


      --
      -- The peer churn governor:
      --
      let peerChurnGovernor' = Governor.peerChurnGovernor PeerChurnArgs {
                                 pcaPeerSelectionTracer = dtTracePeerSelectionTracer,
                                 pcaChurnTracer         = dtTraceChurnCounters,
                                 pcaDeadlineInterval    = daDeadlineChurnInterval,
                                 pcaBulkInterval        = daBulkChurnInterval,
                                 pcaPeerRequestTimeout  = policyPeerShareOverallTimeout
                                                            peerSelectionPolicy,
                                 pcaMetrics             = daPeerMetrics,
                                 pcaModeVar             = churnModeVar,
                                 pcaRng                 = churnRng,
                                 pcaReadFetchMode       = daBlockFetchMode,
                                 pcaPeerSelectionVar    = peerSelectionTargetsVar,
                                 pcaReadCounters        = readTVar countersVar,
                                 peerTargets            = daPeerTargets,
                                 pcaReadUseBootstrap    = daReadUseBootstrapPeers,
                                 pcaConsensusMode       = daConsensusMode,
                                 getLedgerStateCtx      = daLedgerPeersCtx,
                                 getLocalRootHotTarget  =
                                       LocalRootPeers.hotTarget
                                     . LocalRootPeers.clampToTrustable
                                     . LocalRootPeers.fromGroups
                                   <$> readTVar localRootsVar }

      --
      -- Part (b): capturing the major control-flow of runM:
      --
      case diffusionMode of

        -- InitiatorOnly mode, run peer selection only:
        InitiatorOnlyDiffusionMode ->
          withConnectionManagerInitiatorOnlyMode $ \connectionManager-> do
          debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng daConsensusMode daMinBigLedgerPeersForTrustedState
          diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
          withPeerStateActions' connectionManager $ \peerStateActions->
            withPeerSelectionActions'
              (return Map.empty)
              PeerSelectionActionsDiffusionMode { psPeerStateActions = peerStateActions } $
              \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions->
                Async.withAsync
                  (peerSelectionGovernor'
                    dtDebugPeerSelectionInitiatorTracer
                    debugStateVar
                    peerSelectionActions) $ \governorThread ->
                    Async.withAsync
                      peerChurnGovernor' $ \churnGovernorThread ->
                      -- wait for any thread to fail:
                      snd <$> Async.waitAny
                                [ledgerPeersThread, localRootPeersProvider, governorThread, churnGovernorThread]

        -- InitiatorAndResponder mode, run peer selection and the server:
        InitiatorAndResponderDiffusionMode -> do
          inboundInfoChannel  <- newInformationChannel
          withConnectionManagerInitiatorAndResponderMode
            inboundInfoChannel $ \connectionManager ->
              --
              -- node-to-node sockets
              --
              withSockets
                tracer
                diNtnSnocket
                (\sock addr -> diNtnConfigureSocket sock (Just addr))
                (\sock addr -> diNtnConfigureSystemdSocket sock addr)
                (catMaybes [daIPv4Address, daIPv6Address])
                $ \sockets addresses ->
                  --
                  -- node-to-node server
                  --
                  Server.with
                    Server.Arguments {
                        Server.sockets               = sockets,
                        Server.snocket               = diNtnSnocket,
                        Server.tracer                = dtServerTracer,
                        Server.trTracer              = dtInboundGovernorTransitionTracer,
                        Server.debugInboundGovernor  = nullTracer,
                        Server.inboundGovernorTracer = dtInboundGovernorTracer,
                        Server.connectionLimits      = daAcceptedConnectionsLimit,
                        Server.connectionManager     = connectionManager,
                        Server.connectionDataFlow    = diNtnDataFlow,
                        Server.inboundIdleTimeout    = Just daProtocolIdleTimeout,
                        Server.inboundInfoChannel    = inboundInfoChannel
                      } $ \inboundGovernorThread readInboundState -> do
                    debugStateVar <- newTVarIO $ emptyPeerSelectionState fuzzRng daConsensusMode daMinBigLedgerPeersForTrustedState
                    diInstallSigUSR1Handler connectionManager debugStateVar daPeerMetrics
                    withPeerStateActions' connectionManager $ \peerStateActions ->
                      withPeerSelectionActions'
                        (mkInboundPeersMap <$> readInboundState)
                        PeerSelectionActionsDiffusionMode { psPeerStateActions = peerStateActions } $
                          \(ledgerPeersThread, localRootPeersProvider) peerSelectionActions ->
                            Async.withAsync
                              (peerSelectionGovernor' dtDebugPeerSelectionInitiatorResponderTracer debugStateVar peerSelectionActions) $ \governorThread -> do
                                -- begin, unique to InitiatorAndResponder mode:
                                traceWith tracer (RunServer addresses)
                                -- end, unique to ...
                                Async.withAsync peerChurnGovernor' $ \churnGovernorThread ->
                                  -- wait for any thread to fail:
                                  snd <$> Async.waitAny [ledgerPeersThread, localRootPeersProvider, governorThread, churnGovernorThread, inboundGovernorThread]

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
    -> Arguments IO
                 Socket      RemoteAddress
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
                   :: forall mode x y ntnconn.
                      NodeToNodeConnectionManager mode Socket RemoteAddress
                                                  NodeToNodeVersionData NodeToNodeVersion IO x y
                   -> StrictTVar IO (PeerSelectionState RemoteAddress ntnconn)
                   -> PeerMetrics IO RemoteAddress
                   -> IO ()
#ifdef POSIX
                 diInstallSigUSR1Handler = \connectionManager dbgStateVar metrics -> do
                   _ <- Signals.installHandler
                     Signals.sigUSR1
                     (Signals.Catch
                       (do state <- atomically $ readState connectionManager
                           traceWith (dtConnectionManagerTracer tracersExtra)
                                     (CM.TrState state)
                           ps <- readTVarIO dbgStateVar
                           now <- getMonotonicTime
                           (up, bp, lsj, am) <- atomically $
                                                  (,,,) <$> upstreamyness metrics
                                                        <*> fetchynessBlocks metrics
                                                        <*> lpGetLedgerStateJudgement (daLedgerPeersCtx apps)
                                                        <*> Governor.readAssociationMode
                                                              (daReadUseLedgerPeers argsExtra)
                                                              (daOwnPeerSharing argsExtra)
                                                              (Governor.bootstrapPeersFlag ps)
                           let dbgState = makeDebugPeerSelectionState ps up bp lsj am
                           traceWith (dtTracePeerSelectionTracer tracersExtra)
                                     (TraceDebugState now dbgState)
                       )
                     )
                     Nothing
                   return ()
#else
                 diInstallSigUSR1Handler = \_ _ _ -> pure ()
#endif

             diRng <- newStdGen
             diConnStateIdSupply <- atomically $ CM.newConnStateIdSupply Proxy
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
                 diNtnDataFlow = ntnDataFlow,
                 diNtnPeerSharing = peerSharing,
                 diNtnToPeerAddr = curry IP.toSockAddr,

                 diNtcSnocket = Snocket.localSnocket iocp,
                 diNtcBearer = makeLocalBearer,
                 diNtcHandshakeArguments,
                 diNtcGetFileDescriptor = localSocketFileDescriptor,

                 diRng,
                 diInstallSigUSR1Handler,
                 diDnsActions = ioDNSActions,
                 diUpdateVersionData = \versionData diffusionMode -> versionData { diffusionMode },
                 diConnStateIdSupply
               }
               tracers tracersExtra args argsExtra apps appsExtra

--
-- Fatal Errors
--
-- If we are out of file descriptors (either because we exhausted
-- process or system limit) we should shut down the node and let the
-- operator investigate.
--
-- Refs:
-- * https://hackage.haskell.org/package/ghc-internal-9.1001.0/docs/src/GHC.Internal.Foreign.C.Error.html#errnoToIOError
-- * man socket.2
-- * man connect.2
-- * man accept.2
-- NOTE: many `connect` and `accept` exceptions are classified as
-- `OtherError`, here we only distinguish fatal IO errors (e.g.
-- ones that propagate to the main thread).
-- NOTE: we don't use the rethrow policy for `accept` calls, where
-- all but `ECONNABORTED` are fatal exceptions.
--
-- NOTE: UnsupportedOperation error type is not considered fatal since it can
-- happen on a race condition between the connect and accept call between two
-- nodes that have each other as local roots.
--
isFatal :: IOErrorType -> Bool
isFatal ResourceExhausted = True
        -- EAGAIN            -- connect, accept
        -- EMFILE            -- socket, accept
        -- ENFILE            -- socket, accept
        -- ENOBUFS           -- socket, accept
        -- ENOMEM            -- socket, accept
isFatal InvalidArgument      = True
        -- EINVAL            -- socket, accept
        -- ENOTSOCK          -- connect
        -- EBADF             -- connect, accept
isFatal ProtocolError        = True
        -- EPROTONOSUPPOPRT  -- socket
        -- EPROTO            -- accept
isFatal _                    = False

--
-- Data flow
--

-- | Node-To-Node protocol connections which negotiated
-- `InitiatorAndResponderDiffusionMode` are `Duplex`.
--
ntnDataFlow :: NodeToNodeVersionData -> DataFlow
ntnDataFlow NodeToNodeVersionData { diffusionMode } =
  case diffusionMode of
    InitiatorAndResponderDiffusionMode -> Duplex
    InitiatorOnlyDiffusionMode         -> Unidirectional


-- | All Node-To-Client protocol connections are considered 'Unidirectional'.
--
ntcDataFlow :: ntcVersionData -> DataFlow
ntcDataFlow _ = Unidirectional
