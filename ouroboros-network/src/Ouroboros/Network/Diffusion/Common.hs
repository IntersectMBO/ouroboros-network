-- Common things between P2P and NonP2P Diffusion modules
{-# LANGUAGE DataKinds #-}

module Ouroboros.Network.Diffusion.Common
  ( InitializationTracer(..)
  , Failure(..)
  , Tracers(..)
  , nullTracers
  , Arguments(..)
  , Applications(..)
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
data InitializationTracer ntnAddr ntcAddr
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
data Failure ntnAddr = UnsupportedReadySocket -- Windows only
                     | UnexpectedIPv4Address ntnAddr
                     | UnexpectedIPv6Address ntnAddr
                     | NoSocket
  deriving (Eq, Show)

instance (Typeable ntnAddr, Show ntnAddr) => Exception (Failure ntnAddr)

-- | Common DiffusionTracers interface between P2P and NonP2P
--
data Tracers ntnAddr ntnVersion ntcAddr ntcVersion m = Tracers {
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
        :: Tracer m (InitializationTracer ntnAddr ntcAddr)

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer
        :: Tracer m TraceLedgerPeers
    }


nullTracers :: Applicative m
            => Tracers ntnAddr ntnVersion
                       ntcAddr ntcVersion
                       m
nullTracers = Tracers {
    dtMuxTracer                     = nullTracer
  , dtHandshakeTracer               = nullTracer
  , dtLocalMuxTracer                = nullTracer
  , dtLocalHandshakeTracer          = nullTracer
  , dtDiffusionInitializationTracer = nullTracer
  , dtLedgerPeersTracer             = nullTracer
  }

-- | Common DiffusionArguments interface between P2P and NonP2P
--
data Arguments ntnFd ntnAddr ntcFd ntcAddr = Arguments {
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
    , daMode         :: DiffusionMode
  }

-- | Common DiffusionArguments interface between P2P and NonP2P
--
-- TODO: we need initiator only mode for blockchain explorer or a similar
-- application, there's no reason why one should run a node-to-node server for
-- it.
--
data Applications ntnAddr ntnVersion ntnVersionData
                  ntcAddr ntcVersion ntcVersionData
                  m =
  Applications {
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
      --
      -- TODO: it should be in 'InterfaceExtra'
    , daLedgerPeersCtx :: LedgerPeersConsensusInterface m
  }
