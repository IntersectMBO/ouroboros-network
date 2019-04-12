{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO: this module should be exposed as 'Ouorboros.Network'
module Ouroboros.Network.Mux.Interface
  (
  -- * High level interface for the multiplex layer
  -- $interface
    NetworkInterface (..)
  , MuxPeer (..)
  , NetworkNode (..)

  -- * Run mux layer on initiated connections
  , Connection (..)
  , WithConnection
  , withConnection
  , runWithConnection
  , withConnectionAsync

  -- * Auxiliary functions
  , miniProtocolDescription
  ) where

import qualified Codec.CBOR.Read     as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)
import           Numeric.Natural (Natural)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )

import           Control.Tracer

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined

import           Ouroboros.Network.Mux.Types

-- $interface
--
-- To run a node you will also need a bearer and a way to run a server, see
--
-- * @'Ouroboros.Network.Socket'@ module provides a socket based bearer and
--   a server that accepts connections and allows to connect to remote peers.
--
-- * @'Ouroboros.Network.Pipe'@ module provides a pipe based bearer with
--   a function that runs the mux layer on it.
--

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
data NetworkInterface ptcl addr m r = NetworkInterface {
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
data NetworkNode addr m r = NetworkNode {
      -- |
      -- The way to connect ot other peers.  On startup the network interface
      -- will run this to connect with a given list of peer.  But it can also
      -- be used at a later stage to connect to a new peer.
      --
      -- This function will run client side of mux version negotation and then
      -- start a the list protocols given by @'NetworkInterface'@.
      connect :: WithConnection m addr (Connection m) r,

      -- |
      -- This will cancel the thread that is listening for new connections and
      -- close the underlaying bearer.
      killNode  :: m ()
    }


-- |
-- Monadic computation which runs mux layer for initiated connection.
--
newtype Connection m = Connection {
      runConnection :: m ()
    }

-- |
-- CPS style for runing mux layer
--
type WithConnection m addr a r = addr -> (a -> m r) -> m r

-- |
-- Run @'WithConnection' m addr (Connection m) ()@ in the current thread.  The
-- implemntation of @'WithConnection'@ handles resouce aquisition,
-- see @'Ouroboros.Network.Socket.withNetworkNode'@.
--
withConnection :: WithConnection m addr (Connection m) () -> addr -> m ()
withConnection withConn addr = withConn addr runConnection

-- |
-- Run @'WithConnection'@ with supplied @addr@ and continuation.
--
runWithConnection :: WithConnection m addr a r -> addr -> (a -> m r) -> m r
runWithConnection withConn a k = withConn a k

-- |
-- Run @'WithConnectionAsync' m addr (Connection m) ()@ in another thread giving
-- acces to the @'Async' m@.  Note: when the call back @k@ will terminate the
-- connection will be teared down (by @'WithConnection'@) and the spawned thread
-- will be killed.
--
withConnectionAsync :: MonadAsync m
                    => WithConnection m addr (Connection m) r
                    -> addr
                    -> (Async m () -> m r)
                    -> m r
withConnectionAsync withConn addr0 k0 = runWithConnection (asAsync withConn) addr0 k0
    where
      asAsync :: MonadAsync m
              => WithConnection m addr (Connection m) r
              -> WithConnection m addr (Async m ()) r
      asAsync f = \addr k -> f addr $ \conn -> withAsync (runConnection conn) k

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
