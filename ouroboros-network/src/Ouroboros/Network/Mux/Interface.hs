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
  , Connection (..)
  , withConnection

  -- * Auxiliary functions
  , miniProtocolDescription
  ) where

import qualified Codec.CBOR.Read     as CBOR
import           Control.Exception (SomeException, bracket)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)
import           Numeric.Natural (Natural)

import           Control.Monad.Class.MonadAsync ( MonadAsync )
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )

import           Control.Tracer

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Mux.Types

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
       Tracer m (TraceSendRecv ps)
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsClient st m a
    -> MuxPeer m

  -- |
  -- A pipelined peer together with a codec.
  --
  OnlyPipelinedClient
    :: forall ps (st :: ps) m a.
       Natural
    -> Tracer m (TraceSendRecv ps)
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> PeerPipelined ps AsClient st m a
    -> MuxPeer m

  -- |
  -- Server peer with a codec
  OnlyServer
    :: forall ps (st :: ps) m a.
       Tracer m (TraceSendRecv ps)
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsServer st m a
    -> MuxPeer m

  -- |
  -- Client and server peers with the corresponding codec.
  --
  ClientAndServer
    :: forall ps (st :: ps) m a.
       Tracer m (TraceSendRecv ps)
    -> Codec ps CBOR.DeserialiseFailure m ByteString
    -> Peer ps AsClient st m a
    -> Peer ps AsServer st m a
    -> MuxPeer m

  -- |
  -- Pipelined client and a server with the correspnding codec.
  --
  PipelinedClientAndServer
    :: forall ps (st :: ps) m a.
       Natural
    -> Tracer m (TraceSendRecv ps)
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
      -- Map of protocols that we run
      --
      protocols        :: ptcl -> MuxPeer m
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
      -- @'connectTo'@ will throw an exception if the underlying bearer breaks,
      -- or the remote part is not reachable (e.g. @'Control.Exception.IOException'@).
      --
      connectTo :: addr -> m (Connection m),

      -- |
      -- This will cancel the thread that is listening for new connections and
      -- close the underlaying bearer.
      killNode  :: m ()
    }


-- |
-- A way of controling an openned connection.
--
data Connection m = Connection {
      -- |
      -- After opening a connection to a peer, a clean way to shut down the
      -- underlaying bearer and wind down the thread that serves this connection.
      -- Note that this will interupt the protocols running on that connection;
      -- alternatively you can terminate a protocol by sending its terminal message,
      -- this will trigger shutdown of the bearer as well.
      --
      terminate :: m (),

      -- |
      -- Await on the mux thread and return an exception that was raised by it
      -- if any.  It will not close the connection in case of an error, use @'terminate'@ for that.
      await :: m (Maybe SomeException)
    }


-- |
-- A bracket of an @IO@ action which opens with @'connectTo'@ and closed with
-- @'terminate'@.  Inside the callback you should not use @'terminate'@ (this
-- might result in closing a wrong socket!), but @'observe'@ is safe.
--
withConnection
  :: NetworkNode addr IO
  -> addr
  -> (Connection IO -> IO a)
  -> IO a
withConnection nn addr k =
  bracket (connectTo nn addr) terminate k


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
miniProtocolDescription (OnlyClient tr codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPeer tr codec chan peer)),
      mpdResponder = Nothing
    }
miniProtocolDescription (OnlyPipelinedClient omax tr codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPipelinedPeer omax tr codec chan peer)),
      mpdResponder = Nothing
    }
miniProtocolDescription (OnlyServer tr codec peer) =
  MiniProtocolDescription {
      mpdInitiator = Nothing,
      mpdResponder = Just (\chan -> void (runPeer tr codec chan peer))
    }
miniProtocolDescription (ClientAndServer tr codec clientPeer serverPeer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPeer tr codec chan clientPeer)),
      mpdResponder = Just (\chan -> void (runPeer tr codec chan serverPeer))
    }
miniProtocolDescription (PipelinedClientAndServer omax tr codec clientPeer serverPeer) =
  MiniProtocolDescription {
      mpdInitiator = Just (\chan -> void (runPipelinedPeer omax tr codec chan clientPeer)),
      mpdResponder = Just (\chan -> void (runPeer tr codec chan serverPeer))
    }
