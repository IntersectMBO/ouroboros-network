{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- TODO: this module should be exposed as 'Ouorboros.Network'
module Ouroboros.Network.Mux.Interface
  (
  -- * High level interface for the multiplex layer
  -- $interface
    AppType (..)
  , MuxApplication (..)
  , runMuxApplication
  , MuxPeer (..)
  , simpleMuxClientApplication
  , simpleMuxServerApplication
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

data AppType where
    ClientApp :: AppType
    ServerApp :: AppType

-- |
-- Application run by mux layer.
--
-- * enumeration of client application, e.g. a wallet application communicating
--   with a node using ChainSync and TxSubmission protocols; this only requires
--   to run client side of each protocol.
--
-- * enumeration of server applications, this is useful for running a server
-- side of a multiplexing layer.
--
data MuxApplication (appType :: AppType) ptcl m  where
  MuxClientApplication
    -- Client application; most simple application will be @'runPeer'@ or
    -- @'runPipelinedPeer'@ supplied with a codec and a @'Peer'@ for each
    -- @ptcl@.  But it allows to handle resources if just application of
    -- @'runPeer'@ is not enough.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ClientApp ptcl m

  MuxServerApplication
    -- Server application; similarly to the @'MuxClientApplication'@.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ServerApp ptcl m


-- |
-- Unwrap @'MuxApplication'@.
--
runMuxApplication
  :: MuxApplication appType ptcl m
  -> ptcl
  -> Channel m ByteString
  -> m ()
runMuxApplication (MuxClientApplication app) = app
runMuxApplication (MuxServerApplication app) = app

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
  -> MuxApplication ClientApp ptcl m
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
  -> MuxApplication ServerApp ptcl m
simpleMuxServerApplication fn = MuxServerApplication $ \ptcl channel ->
  case fn ptcl of
    MuxPeer tracer codec peer -> void $ runPeer tracer codec channel peer
    MuxPeerPipelined n tracer codec peer -> void $ runPipelinedPeer n tracer codec channel peer
