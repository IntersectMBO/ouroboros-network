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
  , HasInitiator
  , HasResponder
  , MuxApplication (..)
  , initiatorApplication
  , responderApplication
  , MuxPeer (..)
  , runMuxPeer
  , simpleMuxInitiatorApplication
  , simpleMuxResponderApplication
  , ProtocolEnum (..)
  , MiniProtocolLimits (..)

  -- * Version data
  , Versions (..)
  , simpleSingletonVersions
  , acceptEq
  , Sigma (..)
  , Version
  , DictVersion (..)
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Functor (void)

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

import           Ouroboros.Network.Protocol.Handshake.Type (acceptEq)
import           Ouroboros.Network.Protocol.Handshake.Version
import           Ouroboros.Network.Mux.Types ( MiniProtocolLimits (..)
                                             , ProtocolEnum (..)
                                             )


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
    InitiatorApp             :: AppType
    ResponderApp             :: AppType
    InitiatorAndResponderApp :: AppType

type family HasInitiator (appType :: AppType) :: Bool where
    HasInitiator InitiatorApp             = True
    HasInitiator ResponderApp             = False
    HasInitiator InitiatorAndResponderApp = True

type family HasResponder (appType :: AppType) :: Bool where
    HasResponder InitiatorApp             = False
    HasResponder ResponderApp             = True
    HasResponder InitiatorAndResponderApp = True

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
data MuxApplication (appType :: AppType) ptcl m  where
  MuxInitiatorApplication
    -- Initiator application; most simple application will be @'runPeer'@ or
    -- @'runPipelinedPeer'@ supplied with a codec and a @'Peer'@ for each
    -- @ptcl@.  But it allows to handle resources if just application of
    -- @'runPeer'@ is not enough.  It will be run as @'ModeInitiator'@.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication InitiatorApp ptcl m

  MuxResponderApplication
    -- Responder application; similarly to the @'MuxInitiatorApplication'@ but it
    -- will be run using @'ModeResponder'@.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication ResponderApp ptcl m

  MuxInitiatorAndResponderApplication
    -- Initiator and server applications.
    :: (ptcl -> Channel m ByteString ->  m ())
    -> (ptcl -> Channel m ByteString ->  m ())
    -> MuxApplication InitiatorAndResponderApp ptcl m

-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
initiatorApplication
  :: HasInitiator appType ~ True
  => (MuxApplication appType ptcl m)
  -> (ptcl -> Channel m ByteString ->  m ())
initiatorApplication (MuxInitiatorApplication app) = \ptcl channel -> app ptcl channel
initiatorApplication (MuxInitiatorAndResponderApplication app _) = \ptcl channel -> app ptcl channel

-- |
-- Accessor for the client side of a @'MuxApplication'@.
--
responderApplication
  :: HasResponder appType ~ True
  => (MuxApplication appType ptcl m)
  -> (ptcl -> Channel m ByteString ->  m ())
responderApplication (MuxResponderApplication app) = app
responderApplication (MuxInitiatorAndResponderApplication _ app) = app

-- |
-- This type is only necessary to use the @'simpleMuxClient'@ and
-- @'simpleMuxServer'@ smart constructors.
data MuxPeer failure m a where
    MuxPeer :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m ByteString
            -> Peer ps pr st m a
            -> MuxPeer failure m a

    MuxPeerPipelined
            :: Tracer m (TraceSendRecv ps)
            -> Codec ps failure m ByteString
            -> PeerPipelined ps pr st m a
            -> MuxPeer failure m a


-- |
-- Run a @'MuxPeer'@ using either @'runPeer'@ or @'runPipelinedPeer'@.
--
runMuxPeer
  :: ( MonadThrow m
     , MonadCatch m
     , MonadAsync m
     , Exception failure
     )
  => MuxPeer failure m a
  -> Channel m ByteString
  -> m a
runMuxPeer (MuxPeer tracer codec peer) channel =
    runPeer tracer codec channel peer

runMuxPeer (MuxPeerPipelined tracer codec peer) channel =
    runPipelinedPeer tracer codec channel peer


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
  => (ptcl -> MuxPeer failure m a)
  -> MuxApplication InitiatorApp ptcl m
simpleMuxInitiatorApplication fn = MuxInitiatorApplication $ \ptcl channel ->
  void $ runMuxPeer (fn ptcl) channel


-- |
-- Smart constructor for @'MuxResponderApplicatin'@, similar to @'simpleMuxInitiator'@.
--
simpleMuxResponderApplication
  :: MonadThrow m
  => MonadCatch m
  => MonadAsync m
  => Exception failure
  => (ptcl -> MuxPeer failure m a)
  -> MuxApplication ResponderApp ptcl m
simpleMuxResponderApplication fn = MuxResponderApplication $ \ptcl channel ->
  void $ runMuxPeer (fn ptcl) channel
