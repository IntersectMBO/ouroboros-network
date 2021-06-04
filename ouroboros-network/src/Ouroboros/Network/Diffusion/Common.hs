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
import           Data.Void (Void)

import           Control.Exception (SomeException, Exception)
import           Control.Tracer (Tracer, nullTracer)

import           Network.Socket
                  ( SockAddr
                  , AddrInfo
                  , Socket
                  )
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
                  , NodeToNodeVersion
                  , AcceptedConnectionsLimit
                  , DiffusionMode
                  )
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.NodeToClient
                  ( LocalAddress
                  , Versions
                  , NodeToClientVersion
                  )
import qualified Ouroboros.Network.NodeToClient as NodeToClient

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

-- TODO: add a tracer for these misconfiguration
data DiffusionFailure = UnsupportedLocalSocketType
                      | UnsupportedReadySocket -- Windows only
                      | UnexpectedIPv4Address
                      | UnexpectedIPv6Address
                      | UnexpectedUnixAddress
                      | NoSocket
  deriving (Eq, Show)

instance Exception DiffusionFailure

-- | Common DiffusionTracers interface between P2P and NonP2P
--
data DiffusionTracers p2p = DiffusionTracers {
      -- | Mux tracer
      dtMuxTracer
        :: Tracer IO (WithMuxBearer (ConnectionId SockAddr) MuxTrace)

      -- | Handshake protocol tracer
    , dtHandshakeTracer
        :: Tracer IO NodeToNode.HandshakeTr

      --
      -- NodeToClient tracers
      --

      -- | Mux tracer for local clients
    , dtLocalMuxTracer
        :: Tracer IO (WithMuxBearer (ConnectionId LocalAddress) MuxTrace)

      -- | Handshake protocol tracer for local clients
    , dtLocalHandshakeTracer
        :: Tracer IO NodeToClient.HandshakeTr

      -- | Diffusion initialisation tracer
    , dtDiffusionInitializationTracer
        :: Tracer IO DiffusionInitializationTracer

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer
        :: Tracer IO TraceLedgerPeers

      -- | P2P or NonP2P DiffusionTracers
    , dtExtra :: p2p
    }

nullTracers :: p2p -> DiffusionTracers p2p
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
data DiffusionArguments p2p = DiffusionArguments {
      -- | an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
      --
      daIPv4Address  :: Maybe (Either Socket AddrInfo)

      -- | an @IPV4@ socket ready to accept connections or an @IPv6@ addresses
      --
    , daIPv6Address  :: Maybe (Either Socket AddrInfo)

      -- | an @AF_UNIX@ socket ready to accept connections or an @AF_UNIX@
      -- socket path
    , daLocalAddress :: Maybe (Either Socket FilePath)

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
data DiffusionApplications p2p ntnAddr ntcAddr ntnVersionData ntcVersionData m =
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

      -- | Interface used to get peers from the current ledger.
    , daLedgerPeersCtx :: LedgerPeersConsensusInterface m

      -- | p2p polymorphic type argument to allow easy switching between
      -- P2P and NonP2P DiffusionApplications Extras
      --
    , dapExtra :: p2p
  }
