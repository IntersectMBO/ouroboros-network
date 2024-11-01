-- Common things between P2P and NonP2P Diffusion modules
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Diffusion.Types
  ( DiffusionTracer (..)
  , Failure (..)
  , Tracers (..)
  , nullTracers
  , Arguments (..)
  , Applications (..)
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (Exception, SomeException)
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, nullTracer)

import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Typeable (Typeable)
import Data.Void (Void)

import Network.Mux qualified as Mx

import Ouroboros.Network.Mux (OuroborosApplicationWithMinimalCtx,
           OuroborosBundleWithExpandedCtx)

import Ouroboros.Network.BlockFetch
import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))
import Ouroboros.Network.Protocol.Handshake (Versions)

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.ConsensusMode
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.InboundGovernor (RemoteTransitionTrace)
import Ouroboros.Network.InboundGovernor qualified as InboundGovernor
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server2 qualified as Server
import Ouroboros.Network.Snocket (FileDescriptor)
import Ouroboros.Network.Socket (SystemdSocketTracer)

import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit, ConnectionId,
           DiffusionMode)
import Ouroboros.Network.NodeToNode qualified as NodeToNode

import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor qualified as Governor
import Ouroboros.Network.PeerSelection.Governor.Types
           (ConsensusModePeerTargets (..), DebugPeerSelection (..),
           PeerSelectionCounters, PublicPeerSelectionState,
           TracePeerSelection (..))
import Ouroboros.Network.PeerSelection.LedgerPeers (TraceLedgerPeers)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (LedgerPeerSnapshot,
           LedgerPeersConsensusInterface (..), MinBigLedgerPeersForTrustedState,
           UseLedgerPeers)
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerAdvertise
import Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.PeerSelection.PeerStateActions
           (PeerSelectionActionsTrace)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
           (TraceLocalRootPeers)
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
           (TracePublicRootPeers)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers

-- | The 'DiffusionTracer' logs
--
-- * diffusion initialisation messages
-- * terminal errors thrown by diffusion
--
data DiffusionTracer ntnAddr ntcAddr
  = RunServer (NonEmpty ntnAddr)
  | RunLocalServer ntcAddr
  | UsingSystemdSocket ntcAddr
  -- Rename as 'CreateLocalSocket'
  | CreateSystemdSocketForSnocketPath ntcAddr
  | CreatedLocalSocket ntcAddr
  | ConfiguringLocalSocket ntcAddr FileDescriptor
  | ListeningLocalSocket ntcAddr FileDescriptor
  | LocalSocketUp  ntcAddr FileDescriptor
  -- Rename as 'CreateServerSocket'
  | CreatingServerSocket ntnAddr
  | ConfiguringServerSocket ntnAddr
  | ListeningServerSocket ntnAddr
  | ServerSocketUp ntnAddr
  -- Rename as 'UnsupportedLocalSocketType'
  | UnsupportedLocalSystemdSocket ntnAddr
  -- Remove (this is impossible case), there's no systemd on Windows
  | UnsupportedReadySocketCase
  | DiffusionErrored SomeException
  | SystemdSocketConfiguration SystemdSocketTracer
    deriving Show

-- TODO: add a tracer for these misconfiguration
data Failure where
  UnsupportedReadySocket :: Failure
  UnexpectedIPv4Address  :: forall ntnAddr. (Show ntnAddr, Typeable ntnAddr) => ntnAddr -> Failure
  UnexpectedIPv6Address  :: forall ntnAddr. (Show ntnAddr, Typeable ntnAddr) => ntnAddr -> Failure
  NoSocket               :: Failure
  DiffusionError         :: SomeException -> Failure

deriving instance Show Failure
instance Exception Failure

-- | Common DiffusionTracers interface between P2P and NonP2P
--
data Tracers ntnAddr ntnVersion ntnVersionData
             ntcAddr ntcVersion ntcVersionData
             resolverError m = Tracers {
      -- | Mux tracer
      dtMuxTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) Mx.Trace)

      -- | Handshake protocol tracer
    , dtHandshakeTracer
        :: Tracer m (NodeToNode.HandshakeTr ntnAddr ntnVersion)

      --
      -- NodeToClient tracers
      --

      -- | Mux tracer for local clients
    , dtLocalMuxTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) Mx.Trace)

      -- | Handshake protocol tracer for local clients
    , dtLocalHandshakeTracer
        :: Tracer m (NodeToClient.HandshakeTr ntcAddr ntcVersion)

      -- | Diffusion initialisation tracer
    , dtDiffusionTracer
        :: Tracer m (DiffusionTracer ntnAddr ntcAddr)

    , dtTraceLocalRootPeersTracer
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
        :: Tracer m (AbstractTransitionTrace ntnAddr)

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
            => Tracers ntnAddr ntnVersion ntnVersionData
                       ntcAddr ntcVersion ntcVersionData
                       resolverError m
nullTracers = Tracers {
    dtMuxTracer                                  = nullTracer
  , dtHandshakeTracer                            = nullTracer
  , dtLocalMuxTracer                             = nullTracer
  , dtLocalHandshakeTracer                       = nullTracer
  , dtDiffusionTracer                            = nullTracer
  , dtTraceLocalRootPeersTracer                  = nullTracer
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

-- | Common DiffusionArguments interface between P2P and NonP2P
--
data Arguments m ntnFd ntnAddr ntcFd ntcAddr = Arguments {
      -- | an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
      --
      daIPv4Address              :: Maybe (Either ntnFd ntnAddr)

      -- | an @IPv6@ socket ready to accept connections or an @IPv6@ addresses
      --
    , daIPv6Address              :: Maybe (Either ntnFd ntnAddr)

      -- | an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
      -- socket path
    , daLocalAddress             :: Maybe (Either ntcFd ntcAddr)

      -- | parameters for limiting number of accepted connections
      --
    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit

      -- | run in initiator only mode
      --
    , daMode                     :: DiffusionMode

      -- | public peer selection state
      --
      -- It is created outside of diffusion, since it is needed to create some
      -- apps (e.g. peer sharing).
      --
    , daPublicPeerSelectionVar   :: StrictTVar m (PublicPeerSelectionState ntnAddr)

      -- | selection targets for the peer governor
      --
    , daPeerTargets            :: ConsensusModePeerTargets

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


-- | Versioned mini-protocol bundles run on a negotiated connection.
--
data Applications ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData
                  m a =
  Applications {
      -- | NodeToNode initiator applications for initiator only mode.
      --
      -- TODO: we should accept one or the other, but not both:
      -- 'daApplicationInitiatorMode', 'daApplicationInitiatorResponderMode'.
      --
      -- Even in non-p2p mode we use p2p apps.
      daApplicationInitiatorMode
        :: Versions ntnVersion
                    ntnVersionData
                      (OuroborosBundleWithExpandedCtx
                      Mx.InitiatorMode ntnAddr
                      ByteString m a Void)

      -- | NodeToNode initiator & responder applications for bidirectional mode.
      --
    , daApplicationInitiatorResponderMode
           -- Peer Sharing result computation callback
        :: Versions ntnVersion
                    ntnVersionData
                    (OuroborosBundleWithExpandedCtx
                      Mx.InitiatorResponderMode ntnAddr
                      ByteString m a ())

      -- | NodeToClient responder application (server role)
      --
      -- Because p2p mode does not infect local connections we we use non-p2p
      -- apps.
    , daLocalResponderApplication
        :: Versions ntcVersion
                    ntcVersionData
                     (OuroborosApplicationWithMinimalCtx
                      Mx.ResponderMode ntcAddr
                      ByteString m Void ())

      -- | Interface used to get peers from the current ledger.
      --
      -- TODO: it should be in 'InterfaceExtra'
    , daLedgerPeersCtx :: LedgerPeersConsensusInterface m

      -- | Callback provided by consensus to inform it if the node is
      -- connected to only local roots or also some external peers.
      --
      -- This is useful in order for the Bootstrap State Machine to
      -- simply refuse to transition from TooOld to YoungEnough while
      -- it only has local peers.
      --
    , daUpdateOutboundConnectionsState :: OutboundConnectionsState -> STM m ()

      -- | /node-to-node/ rethrow policy
      --
    , daRethrowPolicy       :: RethrowPolicy

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
