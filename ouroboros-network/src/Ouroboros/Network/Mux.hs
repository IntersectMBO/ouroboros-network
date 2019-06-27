{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Ouroboros.Network.Mux
  ( module Network.Mux
  , module Network.Mux.Interface
  , MuxPeer (..)
  , runMuxPeer
  , simpleMuxInitiatorApplication
  , simpleMuxResponderApplication
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Exception (Exception)
import           Control.Tracer (Tracer)
import           Data.Void (Void)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Pipelined

import           Network.Mux.Interface
import           Network.Mux

-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
--
data MuxPeer peerid failure m bytes a where
    MuxPeer :: Tracer m (TraceSendRecv ps peerid failure)
            -> Codec ps failure m bytes
            -> Peer ps pr st m a
            -> MuxPeer peerid failure m bytes a

    MuxPeerPipelined
            :: Tracer m (TraceSendRecv ps peerid failure)
            -> Codec ps failure m bytes
            -> PeerPipelined ps pr st m a
            -> MuxPeer peerid failure m bytes a


-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer peerid failure m bytes a
  -> peerid
  -> Channel m bytes
  -> m a
runMuxPeer (MuxPeer tracer codec peer) peerid channel =
    runPeer tracer codec peerid channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) peerid channel =
    runPipelinedPeer tracer codec peerid channel peer


-- |
-- Smart constructor for @'MuxInitiatorApplication'@.  It is a simple client, since
-- none of the applications requires resource handling to run in the monad @m@.
-- Each one is simply run either by @'runPeer'@ or @'runPipelinedPeer'@.
--
simpleMuxInitiatorApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer peerid failure m bytes a)
  -> MuxApplication InitiatorApp peerid ptcl m bytes a Void
simpleMuxInitiatorApplication fn = MuxInitiatorApplication $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleMuxResponderApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer peerid failure m bytes a)
  -> MuxApplication ResponderApp peerid ptcl m bytes Void a
simpleMuxResponderApplication fn = MuxResponderApplication $ \peerid ptcl channel ->
  runMuxPeer (fn ptcl) peerid channel
