-- Common things between P2P and NonP2P Diffusion modules
{-# LANGUAGE DataKinds #-}

module Ouroboros.Network.Diffusion.Common
  ( DiffusionTracer (..)
  , Failure (..)
  , Tracers (..)
  , nullTracers
  , Arguments (..)
  , Applications (..)
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Typeable (Typeable)
import           Data.Void (Void)

import           Control.Exception (Exception, SomeException)
import           Control.Tracer (Tracer, nullTracer)

import           Network.Mux (MuxMode (..), MuxTrace, WithMuxBearer)

import           Ouroboros.Network.Mux (OuroborosApplication, OuroborosBundle)
import           Ouroboros.Network.NodeToClient (Versions)
import qualified Ouroboros.Network.NodeToClient as NodeToClient
import           Ouroboros.Network.NodeToNode (AcceptedConnectionsLimit,
                     ConnectionId, DiffusionMode)
import qualified Ouroboros.Network.NodeToNode as NodeToNode
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface, TraceLedgerPeers)
import           Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount)
import           Ouroboros.Network.Snocket (FileDescriptor)
import           Ouroboros.Network.Socket (SystemdSocketTracer)

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
    , dtDiffusionTracer
        :: Tracer m (DiffusionTracer ntnAddr ntcAddr)

      -- | Ledger Peers tracer
    , dtLedgerPeersTracer
        :: Tracer m TraceLedgerPeers
    }


nullTracers :: Applicative m
            => Tracers ntnAddr ntnVersion
                       ntcAddr ntcVersion
                       m
nullTracers = Tracers {
    dtMuxTracer            = nullTracer
  , dtHandshakeTracer      = nullTracer
  , dtLocalMuxTracer       = nullTracer
  , dtLocalHandshakeTracer = nullTracer
  , dtDiffusionTracer      = nullTracer
  , dtLedgerPeersTracer    = nullTracer
  }

-- | Common DiffusionArguments interface between P2P and NonP2P
--
data Arguments ntnFd ntnAddr ntcFd ntcAddr = Arguments {
      -- | an @IPv4@ socket ready to accept connections or an @IPv4@ addresses
      --
      daIPv4Address              :: Maybe (Either ntnFd ntnAddr)

      -- | an @IPV4@ socket ready to accept connections or an @IPv6@ addresses
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
                    (OuroborosBundle
                      InitiatorMode ntnAddr
                      ByteString m a Void)

      -- | NodeToNode initiator & responder applications for bidirectional mode.
      --
    , daApplicationInitiatorResponderMode
           -- Peer Sharing result computation callback
        :: (PeerSharingAmount -> m [ntnAddr])
        -> Versions ntnVersion
                    ntnVersionData
                    (OuroborosBundle
                      InitiatorResponderMode ntnAddr
                      ByteString m a ())

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
