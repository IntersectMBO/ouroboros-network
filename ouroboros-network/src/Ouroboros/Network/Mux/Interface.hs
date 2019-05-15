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
  , MuxApplication (..)
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

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )

import           Network.TypedProtocol.Channel

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
-- Application run by mux layer.
--
-- * enumeration of client application, e.g. a wallet application communicating
--   with a node using ChainSync and TxSubmission protocols; this only requires
--   to run client side of each protocol.
--
-- * enumeration of server applications: this application type is mostly useful
--   tests.
--
-- * enumeration of both client and server applications, e.g. a full node
--   serving downstream peers using server side of each protocol and getting
--   updates from upstream peers using client side of each of the protocols.
--
data MuxApplication ptcl m where
  MuxClientApplication
    -- Client application; most simple application will be @'runPeer'@ or
    -- @'runPipelinedPeer'@ supplied with a codec and a @'Peer'@ for each
    -- @ptcl@.  But it allows to handle resources if just application of
    -- @'runPeer'@ is not enough.  It will be run as @'muxInitiator'@.
    :: (ptcl -> Channel m ByteString -> m a)
    -> MuxApplication ptcl m

  MuxServerApplication
    -- Server application; similarly to the @'MuxClientApplication'@ but it
    -- will be run using @'muxResponder'@.
    :: (ptcl -> Channel m ByteString -> m a)
    -> MuxApplication ptcl m

  MuxClientAndServerApplication
    -- Client and server applications.
    :: (ptcl -> Channel m ByteString -> m a)
    -> (ptcl -> Channel m ByteString -> m b)
    -> MuxApplication ptcl m

-- |
-- Public network interface for 'ouroboros-network'.
--
data NetworkInterface ptcl addr m r = NetworkInterface {
      -- |
      -- Address of the node to run.  The node will bind to this address, and
      -- listen for incoming connections.  Some bearers do not have a notion of
      -- address.
      --
      nodeAddress     :: addr,

      -- |
      -- Map of protocols that we run
      --
      nodeApplication :: MuxApplication ptcl m
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
  => MuxApplication ptcl m
  -> MiniProtocolDescriptions ptcl m
miniProtocolDescription (MuxClientApplication client) = \ptcl ->
  MiniProtocolDescription {
      mpdInitiator = Just (void . client ptcl),
      mpdResponder = Nothing
    }
miniProtocolDescription (MuxServerApplication server) = \ptcl ->
  MiniProtocolDescription {
      mpdInitiator = Nothing,
      mpdResponder = Just (void . server ptcl)
    }
miniProtocolDescription (MuxClientAndServerApplication client server) = \ptcl ->
  MiniProtocolDescription {
      mpdInitiator = Just (void . client ptcl),
      mpdResponder = Just (void . server ptcl)
    }
