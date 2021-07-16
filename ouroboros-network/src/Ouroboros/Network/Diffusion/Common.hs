-- Common things between P2P and NonP2P Diffusion modules
{-# LANGUAGE DataKinds #-}

module Ouroboros.Network.Diffusion.Common
  ( DiffusionInitializationTracer(..)
  , DiffusionFailure(..)
  , DiffusionTracers(..)
  , nullTracers
  , DiffusionArguments(..)
  , DiffusionApplications(..)
  ) where

import           Data.List.NonEmpty (NonEmpty)
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Control.Exception (SomeException, Exception)
import           Control.Tracer (Tracer, nullTracer)

import           Network.Mux
                  ( WithMuxBearer
                  , MuxTrace
                  , MuxMode(..)
                  )

import           Ouroboros.Network.Mux
                  ( OuroborosBundle
                  , OuroborosApplication
                  )
import           Ouroboros.Network.Snocket (FileDescriptor)
import           Ouroboros.Network.PeerSelection.LedgerPeers
                  ( TraceLedgerPeers
                  , LedgerPeersConsensusInterface
                  )
import           Ouroboros.Network.NodeToNode
                  ( ConnectionId
                  , AcceptedConnectionsLimit
                  , DiffusionMode
                  )
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.NodeToClient
                  ( Versions
                  )
import qualified Ouroboros.Network.NodeToClient as NodeToClient

-- TODO: use LocalAddress where appropriate rather than 'path'.
--
data DiffusionInitializationTracer ntnAddr ntcAddr
  = RunServer !(NonEmpty ntnAddr)
  | RunLocalServer !ntcAddr
  | UsingSystemdSocket !ntcAddr
  -- Rename as 'CreateLocalSocket'
  | CreateSystemdSocketForSnocketPath !ntcAddr
  | CreatedLocalSocket !ntcAddr
  | ConfiguringLocalSocket !ntcAddr !FileDescriptor
  | ListeningLocalSocket !ntcAddr !FileDescriptor
  | LocalSocketUp  !ntcAddr !FileDescriptor
  -- Rename as 'CreateServerSocket'
  | CreatingServerSocket !ntnAddr
  | ConfiguringServerSocket !ntnAddr
  | ListeningServerSocket !ntnAddr
  | ServerSocketUp !ntnAddr
  -- Rename as 'UnsupportedLocalSocketType'
  | UnsupportedLocalSystemdSocket !ntnAddr
  -- Remove (this is impossible case), there's no systemd on Windows
  | UnsupportedReadySocketCase
  | DiffusionErrored SomeException
    deriving Show

-- TODO: add a tracer for these misconfiguration
data DiffusionFailure ntnAddr = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
                      | UnexpectedIPv4Address ntnAddr
                      | UnexpectedIPv6Address ntnAddr
                      | NoSocket
  deriving (Eq, Show)

instance (Typeable ntnAddr, Show ntnAddr) => Exception (DiffusionFailure ntnAddr)

-- | Common DiffusionTracers interface between P2P and NonP2P
--
data DiffusionTracers p2p ntnAddr ntnVersion ntcAddr ntcVersion m = DiffusionTracers {
      -- | Mux tracer
      dtMuxTracer
        :: Tracer m (WithMuxBearer (ConnectionId ntnAddr) MuxTrace)

      -- | Handshake protocol tracer
    , dtHandshakeTracer
        :: Tracer m (NodeToNode.HandshakeTr ntnAddr ntnVersion)

      --
      -- NodeToClient tracers
      --

      -- | Mux tracer for local clients
    , dtLocalMuxTracer
        :: Tracer m (WithMuxBearer (ConnectionId ntcAddr) MuxTrace)

      -- | Handshake protocol tracer for local clients
    , dtLocalHandshakeTracer
        :: Tracer m (NodeToClient.HandshakeTr ntcAddr ntcVersion)

      -- | Diffusion initialisation tracer
    , dtDiffusionInitializationTracer
        :: Tracer m (DiffusionInitializationTracer ntnAddr ntcAddr)

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer
        :: Tracer m TraceLedgerPeers

      -- | P2P or NonP2P DiffusionTracers
    , dtExtra :: p2p
    }

nullTracers :: Applicative m => p2p -> DiffusionTracers p2p ntnAddr ntnVersion
                                                            ntcAddr ntcVersion m
nullTracers p2pNullTracers = DiffusionTracers {
  dtMuxTracer                       = nullTracer
  , dtHandshakeTracer               = nullTracer
  , dtLocalMuxTracer                = nullTracer
  , dtLocalHandshakeTracer          = nullTracer
  , dtDiffusionInitializationTracer = nullTracer
  , dtLedgerPeersTracer             = nullTracer
  , dtExtra                         = p2pNullTracers
  }

-- | Common DiffusionArguments interface between P2P and NonP2P
--
data DiffusionArguments p2p ntnFd ntnAddr ntcFd ntcAddr = DiffusionArguments {
      -- | an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
      --
      daIPv4Address  :: Maybe (Either ntnFd ntnAddr)

      -- | an @IPV4@ socket ready to accept connections or an @IPv6@ addresses
      --
    , daIPv6Address  :: Maybe (Either ntnFd ntnAddr)

      -- | an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
      -- socket path
    , daLocalAddress :: Maybe (Either ntcFd ntcAddr)

      -- | parameters for limiting number of accepted connections
      --
    , daAcceptedConnectionsLimit :: AcceptedConnectionsLimit

      -- | run in initiator only mode
      --
    , daDiffusionMode :: DiffusionMode

      -- | p2p polymorphic type argument to allow easy switching between
      -- P2P and NonP2P DiffusionArguments Extras
      --
    , daExtra :: p2p
  }

-- | Common DiffusionArguments interface between P2P and NonP2P
--
-- TODO: we need initiator only mode for Daedalus, there's no reason why we
-- should run a node-to-node server for it.
--
data DiffusionApplications p2p
                           ntnAddr ntnVersion ntnVersionData
                           ntcAddr ntcVersion ntcVersionData
                           m =
  DiffusionApplications {
      -- | NodeToNode initiator applications for initiator only mode.
      --
      -- TODO: we should accept one or the other, but not both:
      -- 'daApplicationInitiatorMode', 'daApplicationInitiatorResponderMode'.
      --
      daApplicationInitiatorMode
        :: Versions ntnVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorMode ntnAddr
                      ByteString m () Void)

      -- | NodeToNode initiator & responder applications for bidirectional mode.
      --
    , daApplicationInitiatorResponderMode
        :: Versions ntnVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorResponderMode ntnAddr
                      ByteString m () ())

    -- | NodeToClient responder application (server role)
    --
    , daLocalResponderApplication
        :: Versions ntcVersion
                    ntcVersionData
                    (OuroborosApplication
                      ResponderMode ntcAddr
                      ByteString m Void ())

      -- | Interface used to get peers from the current ledger.
    , daLedgerPeersCtx :: LedgerPeersConsensusInterface m

      -- | p2p polymorphic type argument to allow easy switching between
      -- P2P and NonP2P DiffusionApplications Extras
      --
    , dapExtra :: p2p
  }
