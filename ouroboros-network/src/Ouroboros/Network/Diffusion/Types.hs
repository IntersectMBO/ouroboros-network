-- Common things between P2P and NonP2P Diffusion modules
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Network.Diffusion.Types
  ( DiffusionTracer (..)
  , Failure (..)
  , Tracers (..)
  , nullTracers
  , Configuration (..)
  , Applications (..)
  , Arguments (..)
  , Interfaces (..)
    -- * ForkPolicy
  , Mx.ForkPolicy
  , Mx.noBindForkPolicy
  , Mx.responderForkPolicy
    -- * NodeToClient type aliases
  , NodeToClientHandle
  , NodeToClientHandleError
  , MkNodeToClientConnectionHandler
    -- * NodeToNode type aliases
  , NodeToNodeHandle
  , NodeToNodeConnectionManager
  , NodeToNodePeerConnectionHandle
  , NodeToNodePeerSelectionActions
    -- * Re-exports
  , AbstractTransitionTrace
  , IG.RemoteTransitionTrace
  , SRVPrefix
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (Exception, SomeException)
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, nullTracer)

import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe.Strict
import Data.Set (Set)
import Data.Typeable (Typeable)
import Data.Void (Void)
import System.Random (StdGen)

import Network.Mux qualified as Mx
import Network.Mux.Trace qualified as Mux
import Network.Mux.Types (ReadBuffer)
import Network.Socket qualified as Socket

import Ouroboros.Network.Mux (OuroborosApplicationWithMinimalCtx,
           OuroborosBundleWithExpandedCtx)

import Ouroboros.Network.PeerSharing (PeerSharingRegistry (..))

import Ouroboros.Network.ConnectionHandler
import Ouroboros.Network.ConnectionManager.Core qualified as CM
import Ouroboros.Network.ConnectionManager.State qualified as CM
import Ouroboros.Network.ConnectionManager.Types
import Ouroboros.Network.Context
import Ouroboros.Network.ExitPolicy
import Ouroboros.Network.InboundGovernor qualified as IG
import Ouroboros.Network.Mux qualified as Mx
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments, Versions)
import Ouroboros.Network.RethrowPolicy
import Ouroboros.Network.Server qualified as Server
import Ouroboros.Network.Snocket (FileDescriptor, Snocket)
import Ouroboros.Network.Socket (SystemdSocketTracer)

import Ouroboros.Network.NodeToClient qualified as NodeToClient
import Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit, DiffusionMode)
import Ouroboros.Network.NodeToNode qualified as NodeToNode
import Ouroboros.Network.PeerSelection as PeerSelection
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.RootPeersDNS
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

-- | Diffusion Tracers
--
data Tracers ntnAddr ntnVersion ntnVersionData
             ntcAddr ntcVersion ntcVersionData
             extraState extraDebugState
             extraFlags extraPeers extraCounters m = Tracers {
      -- | Mux tracer
      dtMuxTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) Mx.Trace)

      -- | Mux's channel tracer
      --
    , dtChannelTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) Mx.ChannelTrace)

      -- | Bearer tracer
      --
    , dtBearerTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntnAddr) Mx.BearerTrace)


      -- | Handshake protocol tracer
    , dtHandshakeTracer
        :: Tracer m (NodeToNode.HandshakeTr ntnAddr ntnVersion)

      --
      -- NodeToClient tracers
      --

      -- | Mux tracer for local clients
      --
    , dtLocalMuxTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) Mx.Trace)

      -- | Mux's channel tracer for local clients
      --
    , dtLocalChannelTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) Mx.ChannelTrace)

      -- | Bearer tracer for local clients
      --
    , dtLocalBearerTracer
        :: Tracer m (Mx.WithBearer (ConnectionId ntcAddr) Mx.BearerTrace)

      -- | Handshake protocol tracer for local clients
    , dtLocalHandshakeTracer
        :: Tracer m (NodeToClient.HandshakeTr ntcAddr ntcVersion)

      -- | Diffusion initialisation tracer
    , dtDiffusionTracer
        :: Tracer m (DiffusionTracer ntnAddr ntcAddr)

    , dtTraceLocalRootPeersTracer
        :: Tracer m (TraceLocalRootPeers extraFlags ntnAddr)

    , dtTracePublicRootPeersTracer
        :: Tracer m TracePublicRootPeers

      -- | Ledger Peers tracer
    , dtTraceLedgerPeersTracer
        :: Tracer m TraceLedgerPeers

    , dtTracePeerSelectionTracer
        :: Tracer m (TracePeerSelection extraDebugState extraFlags extraPeers ntnAddr)

    , dtDebugPeerSelectionInitiatorTracer
        :: Tracer m (DebugPeerSelection extraState extraFlags extraPeers ntnAddr)

      -- TODO: can be unified with the previous one
    , dtDebugPeerSelectionInitiatorResponderTracer
        :: Tracer m (DebugPeerSelection extraState extraFlags extraPeers ntnAddr)

    , dtTracePeerSelectionCounters
        :: Tracer m (PeerSelectionCounters extraCounters)

    , dtTraceChurnCounters
        :: Tracer m PeerSelection.ChurnCounters

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
        :: Tracer m (IG.Trace ntnAddr)

    , dtInboundGovernorTransitionTracer
        :: Tracer m (IG.RemoteTransitionTrace ntnAddr)

    , dtDnsTracer :: Tracer m DNSTrace

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
        :: Tracer m (IG.Trace ntcAddr)
    }


nullTracers :: Applicative m
            => Tracers ntnAddr ntnVersion ntnVersionData
                       ntcAddr ntcVersion ntcVersionData
                       extraState extraDebugState
                       extraFlags extraPeers extraCounters m
nullTracers = Tracers {
    dtMuxTracer                                  = nullTracer
  , dtChannelTracer                              = nullTracer
  , dtBearerTracer                               = nullTracer
  , dtHandshakeTracer                            = nullTracer
  , dtLocalMuxTracer                             = nullTracer
  , dtLocalChannelTracer                         = nullTracer
  , dtLocalBearerTracer                          = nullTracer
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
  , dtDnsTracer                                  = nullTracer
  }

-- | Diffusion arguments which allow to instantiate a completely different
-- diffusion layer. These differ from
--
data Arguments extraState extraDebugState extraFlags extraPeers
               extraAPI extraChurnArgs extraCounters exception
               resolver m
               ntnFd ntnAddr ntnVersion ntnVersionData
               ntcAddr ntcVersion ntcVersionData =
  Arguments {

    -- | node-to-node data flow used by connection manager to classify
    -- negotiated connections
    --
    daNtnDataFlow
      :: ntnVersionData -> DataFlow

    -- | remote side peer sharing information used by peer selection governor
    -- to decide which peers are available for performing peer sharing
  , daNtnPeerSharing
      :: ntnVersionData -> PeerSharing

    -- | Update `ntnVersionData` for initiator-only local roots.
  , daUpdateVersionData
      :: ntnVersionData -> DiffusionMode -> ntnVersionData

    -- | node-to-node handshake configuration
    --
  , daNtnHandshakeArguments
      :: HandshakeArguments (ConnectionId ntnAddr) ntnVersion ntnVersionData m

    -- | node-to-client handshake configuration
    --
  , daNtcHandshakeArguments
      :: HandshakeArguments (ConnectionId ntcAddr) ntcVersion ntcVersionData m

    -- | Interface used to get peers from the current ledger.
    --
  , daLedgerPeersCtx :: LedgerPeersConsensusInterface extraAPI m

    -- | Extra State empty value
    --
  , daEmptyExtraState        :: extraState

    -- | Extra Counters empty value
    --
  , daEmptyExtraCounters     :: extraCounters

    -- | Provide Public Extra Actions for extraPeers to be
    --
  , daExtraPeersAPI          :: PublicExtraPeersAPI extraPeers ntnAddr

    -- | callback which is used to register @SIGUSR1@ signal handler.
  , daInstallSigUSR1Handler
      :: forall mode x y.
         NodeToNodeConnectionManager mode ntnFd
                                     ntnAddr ntnVersionData
                                     ntnVersion m x y
      -> StrictTVar m
           (PeerSelectionState extraState extraFlags extraPeers
                               ntnAddr
                               (NodeToNodePeerConnectionHandle
                                   mode ntnAddr
                                   ntnVersionData m x y))
      -> m ()

  , daPeerSelectionGovernorArgs
      :: forall muxMode responderCtx bytes a b .
         PeerSelectionGovernorArgs
           extraState extraDebugState extraFlags extraPeers
           extraAPI extraCounters
           ntnAddr
           (PeerConnectionHandle muxMode responderCtx
                                 ntnAddr ntnVersionData bytes m a b)
           exception m

    -- | Function that computes extraCounters from PeerSelectionState
    --
  , daPeerSelectionStateToExtraCounters
      :: forall muxMode responderCtx bytes a b .
         PeerSelectionState extraState extraFlags extraPeers
                            ntnAddr (PeerConnectionHandle
                                       muxMode responderCtx ntnAddr
                                       ntnVersionData bytes m a b)
      -> extraCounters

    -- | Function that constructs a 'extraPeers' set from a map of dns
    -- lookup results.
    --
  , daToExtraPeers :: Map ntnAddr PeerAdvertise -> extraPeers

    -- | Request Public Root Peers.
    --
    -- If no custom public root peers is provided (i.e. Nothing) just the
    -- default one from
    -- 'Ouroboros.Network.PeerSelection.PeerSelectionActions.getPublicRootPeers'
    --
  , daRequestPublicRootPeers
      :: Maybe (    PeerActionsDNS ntnAddr resolver m
                 -> DNSSemaphore m
                 -> (Map ntnAddr PeerAdvertise -> extraPeers)
                 -> ( (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set ntnAddr, DiffTime)))
                 -> LedgerPeersKind
                 -> StdGen
                 -> Int
                 -> m (PublicRootPeers extraPeers ntnAddr, DiffTime)))

    -- | Peer Churn Governor if no custom churn governor is required just
    -- use the default one from
    -- 'Ouroboros.Network.PeerSelection.Churn.peerChurnGovernor'
    --
  , daPeerChurnGovernor
      :: PeerSelection.PeerChurnArgs
           m
           extraChurnArgs
           extraDebugState
           extraFlags
           extraPeers
           extraAPI
           extraCounters
           ntnAddr
      -> m Void

    -- | Provide extraChurnArgs to be passed to churn governor
    --
  , daExtraChurnArgs :: extraChurnArgs

    -- | SRV Prefix, as defined in CIP#0155
    --
  , daSRVPrefix :: SRVPrefix
  }

-- | Required Diffusion Arguments to run network layer
--
data Configuration extraFlags m ntnFd ntnAddr ntcFd ntcAddr = Configuration {
      -- | an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
      --
      dcIPv4Address              :: Maybe (Either ntnFd ntnAddr)

      -- | an @IPv6@ socket ready to accept connections or an @IPv6@ addresses
      --
    , dcIPv6Address              :: Maybe (Either ntnFd ntnAddr)

      -- | an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
      -- socket path or name of `named-pipe` on `Windows`.
      --
      -- __Windows Named Pipes__
      -- Names of named-pipes must follow
      -- https://learn.microsoft.com/en-us/windows/win32/ipc/pipe-names, in
      -- particular:
      --
      -- * It must be well formed, e.g. @\\.\pipe\PipeName@
      -- * The pipe name string specified by @PipeName@ can include any character
      --   other than a backslash, including numbers and special characters.
      -- * The entire pipe name string can be up to 256 characters long. Pipe
      --   names are not case-sensitive.
      --  * @PipeName@ has to include an app name to avoid clashes with other
      --    applications, e.g. @cardano-node-mainnet@.
      --  * @PipeName@ should be different for different parallel-installable
      --    instances, e.g. mainnet vs testnet.
      --  * default file permissions should be right: it will be accessible to
      --    the same user.
      --
    , dcLocalAddress             :: Maybe (Either ntcFd ntcAddr)

      -- | parameters for limiting number of accepted connections
      --
    , dcAcceptedConnectionsLimit :: AcceptedConnectionsLimit

      -- | run in initiator only mode
      --
    , dcMode                     :: DiffusionMode

      -- | public peer selection state
      --
      -- It is created outside of diffusion, since it is needed to create some
      -- apps (e.g. peer sharing).
      --
    , dcPublicPeerSelectionVar   :: StrictTVar m (PublicPeerSelectionState ntnAddr)

      -- | selection targets for the peer governor
      --
    , dcPeerSelectionTargets   :: PeerSelectionTargets
    , dcReadLocalRootPeers     :: STM m (LocalRootPeers.Config extraFlags RelayAccessPoint)
    , dcReadPublicRootPeers    :: STM m (Map RelayAccessPoint PeerAdvertise)

    -- | Depending on configuration, node may provide us with
    -- a snapshot of big ledger peers taken at some slot on the chain.
    -- These peers may be selected by ledgerPeersThread when requested
    -- by the peer selection governor when the node is syncing up.
    -- This is especially useful for Genesis consensus mode.
    , dcReadLedgerPeerSnapshot :: STM m (Maybe LedgerPeerSnapshot)

    -- | `UseLedgerPeers` from topology file.
    --
    , dcReadUseLedgerPeers     :: STM m UseLedgerPeers

    -- | Peer's own PeerSharing value.
    --
    -- This value comes from the node's configuration file and is static.
    , dcPeerSharing            :: PeerSharing

      -- | Timeout which starts once all responder protocols are idle. If the
      -- responders stay idle for duration of the timeout, the connection will
      -- be demoted, if it wasn't used by the p2p-governor it will be closed.
      --
      -- Applies to 'Unidirectional' as well as 'Duplex' /node-to-node/
      -- connections.
      --
      -- See 'serverProtocolIdleTimeout'.
      --
    , dcProtocolIdleTimeout    :: DiffTime

      -- | Time for which /node-to-node/ connections are kept in
      -- 'TerminatingState', it should correspond to the OS configured @TCP@
      -- @TIME_WAIT@ timeout.
      --
      -- This timeout will apply to after a connection has been closed, its
      -- purpose is to be resilient for delayed packets in the same way @TCP@
      -- is using @TIME_WAIT@.
      --
    , dcTimeWaitTimeout        :: DiffTime

      -- | Churn interval between churn events in deadline mode.  A small fuzz
      -- is added (max 10 minutes) so that not all nodes churn at the same time.
      --
      -- By default it is set to 3300 seconds.
      --
    , dcDeadlineChurnInterval  :: DiffTime

      -- | Churn interval between churn events in bulk sync mode.  A small fuzz
      -- is added (max 1 minute) so that not all nodes churn at the same time.
      --
      -- By default it is set to 300 seconds.
      --
    , dcBulkChurnInterval      :: DiffTime

      -- | A fork policy for node-to-node mini-protocol threads spawn by mux.
      --
    , dcMuxForkPolicy :: Mx.ForkPolicy ntnAddr

    -- | A fork policy for node-to-client mini-protocols threads spawn by mux.
    --
    , dcLocalMuxForkPolicy :: Mx.ForkPolicy ntcAddr

    -- | Mux egress queue's poll interval
    , dcEgressPollInterval :: DiffTime

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

      -- | /node-to-node/ rethrow policy.
      --
      -- TODO: should it be part of `ExitPolicy`?
      --
    , daRethrowPolicy       :: RethrowPolicy

      -- | /node-to-node/ return policy
      --
      -- Internally packed into `ExitPolicy` record.
      --
    , daReturnPolicy        :: ReturnPolicy a

      -- | /node-to-node/ error delay.
      --
      -- Internally packed into `ExitPolicy` record.
    , daRepromoteErrorDelay :: RepromoteDelay

      -- | /node-to-client/ rethrow policy
      --
    , daLocalRethrowPolicy  :: RethrowPolicy

      -- | 'PeerSelectionPolicy' used by the outbound governor to pick peers to
      -- promote/demote ((see 'simplePeerSelectionPolicy').
      --
    , daPeerSelectionPolicy :: PeerSelectionPolicy ntnAddr m

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

type MkNodeToClientConnectionHandler
      ntcFd ntcAddr ntcVersion ntcVersionData m =
       (   StrictTVar m (StrictMaybe IG.ResponderCounters)
        -> Tracer m (Mux.WithBearer (ConnectionId ntcAddr) Mux.Trace))
    -> ConnectionHandler
         Mx.ResponderMode
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

type NodeToNodePeerSelectionActions extraState extraFlags extraPeers extraAPI extraCounters
                                    (mode :: Mx.Mode) ntnAddr ntnVersionData m a b =
    PeerSelectionActions
      extraState extraFlags extraPeers extraAPI extraCounters
      ntnAddr
      (NodeToNodePeerConnectionHandle mode ntnAddr ntnVersionData m a b)
      m


data Interfaces ntnFd ntnAddr ntcFd ntcAddr
                resolver m =
    Interfaces {
        -- | node-to-node snocket
        --
        diNtnSnocket
          :: Snocket m ntnFd ntnAddr,

        -- | node-to-node 'Mx.MakeBearer' callback
        --
        diNtnBearer
          :: Mx.MakeBearer m ntnFd,

        -- | readbuffer
        diWithBuffer
          :: (Maybe (ReadBuffer m) -> m ()) -> m (),

        -- | node-to-node socket configuration
        --
        diNtnConfigureSocket
          :: ntnFd -> Maybe ntnAddr -> m (),

        -- | node-to-node systemd socket configuration
        --
        diNtnConfigureSystemdSocket
          :: ntnFd -> ntnAddr -> m (),

        -- | node-to-node address type
        --
        diNtnAddressType
          :: ntnAddr -> Maybe AddressType,

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

        -- | diffusion dns actions
        --
        diDnsActions
          :: Tracer m DNSTrace
          -> DNSLookupType
          -> (IP -> Socket.PortNumber -> ntnAddr)
          -> DNSActions ntnAddr resolver m,

        -- | `ConnStateIdSupply` used by the connection-manager for this node.
        --
        -- This is exposed for testing, where we use a global
        -- `ConnStateIdSupply`.
        --
        diConnStateIdSupply
          :: CM.ConnStateIdSupply m
      }
