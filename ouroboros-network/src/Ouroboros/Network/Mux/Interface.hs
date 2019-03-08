{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: this module should be exposed as 'Ouorboros.Network'
module Ouroboros.Network.Mux.Interface
  ( NetworkInterface (..)
  , MuxPeer (..)
  , NetworkNode (..)

  -- * Auxiliary functions
  , miniProtocolDescription
  ) where

import qualified Codec.CBOR.Read     as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)
import           Numeric.Natural (Natural)

import           Control.Monad.Class.MonadAsync ( MonadAsync )
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Mux.Types
import           Ouroboros.Network.Mux.Control

-- |
-- Specification for peers of a protocol.  This type instructs the multiplexing
-- layer to run a given client \/ server peers.
--
data MuxPeer m where
  -- |
  -- A non pipeliend peer together with a codec.
  --
  OnlyClient
    :: forall ps (st :: ps) m a.
       Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsClient st m a
    -> MuxPeer m

  -- |
  -- A pipelined peer together with a codec.
  --
  OnlyPipelinedClient
    :: forall ps (st :: ps) m a.
       Natural
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> PeerPipelined ps AsClient st m a
    -> MuxPeer m

  -- |
  -- Server peer with a codec
  OnlyServer
    :: forall ps (st :: ps) m a.
       Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsServer st m a
    -> MuxPeer m

  -- |
  -- Client and server peers with the corresponding codec.
  --
  ClientAndServer
    :: forall ps (st :: ps) m a.
       Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsClient st m a
    -> Peer ps AsServer st m a
    -> MuxPeer m

  -- |
  -- Pipelined client and a server with the correspnding codec.
  --
  PipelinedClientAndServer
    :: forall ps (st :: ps) m a.
       Natural
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> PeerPipelined ps AsClient st m a
    -> Peer ps AsServer st m a
    -> MuxPeer m


-- |
-- Public network interface for 'ouroboros-network'.
--
data NetworkInterface ptcl addr m = NetworkInterface {
      -- |
      -- Address of the node to run.  The node will bind to this address, and
      -- listen for incoming connections.  Some bearers do not have a notion of
      -- address.
      --
      nodeAddress      :: addr,

      -- |
      -- List of mux versions that we understand
      --
      knownMuxVersions :: [SomeVersion],

      -- |
      -- Map of protocols that we run.  @'SomeVersion'@ will be one of the
      -- @'knownMuxVersions'@
      --
      protocols        :: (SomeVersion -> Maybe (ptcl -> MuxPeer m))
    }

-- | Low level network interface.  It can be intiatiated using a socket, pair
-- of pipes or a pair queues.
--
data NetworkNode addr m = NetworkNode {
      -- |
      -- The way to connect ot other peers.  On startup the network interface
      -- will run this to connect with a given list of peer.  But it can also
      -- be used at a later stage to connect to a new peer.
      --
      -- This function will run client side of mux version negotation and then
      -- start a the list protocols given by @'NetworkInterface'@.
      --
      connectTo :: addr -> m (),

      -- |
      -- This will cancel the thread that is listening for new connections and
      -- close the underlaying bearer.
      killNode  :: m ()
    }

-- |
-- Transform a @'MuxPeer'@ into @'ProtocolDescription'@ used by the
-- multiplexing layer.
--
miniProtocolDescription
  :: forall m ptcl.
     ( MonadAsync m
     , MonadCatch m
     , MonadThrow m
     )
  => MuxPeer m
  -> MiniProtocolDescription ptcl m
miniProtocolDescription (OnlyClient codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPeer codec chan peer)),
      mpdResponder = Nothing
    }
miniProtocolDescription (OnlyPipelinedClient omax codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPipelinedPeer omax codec chan peer)),
      mpdResponder = Nothing
    }
miniProtocolDescription (OnlyServer codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Nothing,
      mpdResponder = Just (\chan -> void (runPeer codec chan peer))
    }
miniProtocolDescription (ClientAndServer codec clientPeer serverPeer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPeer codec chan clientPeer)),
      mpdResponder = Just (\chan -> void (runPeer codec chan serverPeer))
    }
miniProtocolDescription (PipelinedClientAndServer omax codec clientPeer serverPeer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPipelinedPeer omax codec chan clientPeer)),
      mpdResponder = Just (\chan -> void (runPeer codec chan serverPeer))
    }
