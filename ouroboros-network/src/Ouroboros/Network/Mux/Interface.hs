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
  , clientApplication
  , serverApplication
  , MuxPeer (..)
  , simpleMuxClientApplication
  , simpleMuxServerApplication
  , NetworkNode (..)

  -- * Run mux layer on initiated connections
  , Connection (..)

  -- * Auxiliary functions
  , miniProtocolDescription
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)
import           Numeric.Natural (Natural)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Tracer (Tracer)

import           Control.Exception (Exception)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
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
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ptcl m

  MuxServerApplication
    -- Server application; similarly to the @'MuxClientApplication'@ but it
    -- will be run using @'muxResponder'@.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ptcl m

  MuxClientAndServerApplication
    -- Client and server applications.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ptcl m


-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
clientApplication
  :: Functor m
  => (MuxApplication ptcl m)
  -> Maybe (ptcl -> Channel m ByteString ->  m ())
clientApplication (MuxClientApplication app) = Just $ \ptcl channel -> app ptcl channel
clientApplication (MuxServerApplication _)   = Nothing
clientApplication (MuxClientAndServerApplication app _) = Just $ \ptcl channel -> app ptcl channel

-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
serverApplication
  :: Functor m
  => (MuxApplication ptcl m)
  -> Maybe (ptcl -> Channel m ByteString ->  m ())
serverApplication (MuxServerApplication app) = Just app
serverApplication (MuxClientApplication _)   = Nothing
serverApplication (MuxClientAndServerApplication _ app) = Just app

-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
data MuxPeer failure m a where
    MuxPeer :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m ByteString
            -> Peer ps pr st m a
            -> MuxPeer failure m a

    MuxPeerPipelined
            :: Natural
            -> Tracer m (TraceSendRecv ps)
            -> Codec ps failure m ByteString
            -> PeerPipelined ps pr st m a
            -> MuxPeer failure m a

-- |
-- Smart constructor for @'MuxClientApplication'@.  It is a simple client, since
-- none of the applications requires resource handling to run in the monad @m@.
-- Each one is simply run either by @'runPeer'@ or @'runPipelinedPeer'@.
--
simpleMuxClientApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m a)
  -> MuxApplication ptcl m
simpleMuxClientApplication fn = MuxClientApplication $ \ptcl channel ->
  case fn ptcl of
    MuxPeer tracer codec peer -> void $ runPeer tracer codec channel peer
    MuxPeerPipelined n tracer codec peer -> void $ runPipelinedPeer n tracer codec channel peer


-- |
-- Smart constructor for @'MuxServerApplicatin'@, similar to @'simpleMuxClient'@.
--
simpleMuxServerApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m a)
  -> MuxApplication ptcl m
simpleMuxServerApplication fn = MuxServerApplication $ \ptcl channel ->
  case fn ptcl of
    MuxPeer tracer codec peer -> void $ runPeer tracer codec channel peer
    MuxPeerPipelined n tracer codec peer -> void $ runPipelinedPeer n tracer codec channel peer


-- |
-- Public network interface for 'ouroboros-network'.
--
data NetworkInterface ptcl addr m = NetworkInterface {
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
      connect :: addr -> (Connection m -> m r) -> m r,

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
      mpdInitiator = Just (client ptcl),
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
