{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
-- This module should be imported qualified.
--
module Ouroboros.Network.Driver.Stateful
  ( -- * Introduction
    -- $intro
    -- * Normal peers
    runPeer
  , TraceSendRecv (..)
  , Role (..)
  , DecoderFailure (..)
    -- * Connected peers
    -- TODO: move these to a test lib
  , runConnectedPeers
  , runConnectedPeersAsymmetric
  ) where

import Data.Kind (Type)

import Network.TypedProtocol.Codec (AnyMessage (..))
import Network.TypedProtocol.Core
import Network.TypedProtocol.Stateful.Codec hiding (AnyMessage (..))
import Network.TypedProtocol.Stateful.Driver
import Network.TypedProtocol.Stateful.Peer

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple (DecoderFailure (..), Role (..),
           TraceSendRecv (..))
import Ouroboros.Network.Util.ShowProxy

import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), contramap, traceWith)


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actually run one we need a source and sink for the typed
-- protocol messages. These are provided by a 'Channel' and a 'Codec'. The
-- 'Channel' represents one end of an untyped duplex message transport, and
-- the 'Codec' handles conversion between the typed protocol messages and
-- the untyped channel.
--
-- So given the 'Peer' and a compatible 'Codec' and 'Channel' we can run the
-- peer in some appropriate monad. The peer and codec have to agree on
-- the same protocol and role in that protocol. The codec and channel have to
-- agree on the same untyped medium, e.g. text or bytes. All three have to
-- agree on the same monad in which they will run.
--
-- This module provides drivers for normal and pipelined peers. There is
-- very little policy involved here so typically it should be possible to
-- use these drivers, and customise things by adjusting the peer, or codec
-- or channel.
--
-- It is of course possible to write custom drivers and the code for these ones
-- may provide a useful starting point. The 'runDecoder' function may be a
-- helpful utility for use in custom drives.
--


driverStateful :: forall ps (pr :: PeerRole) failure bytes (f :: ps -> Type) m.
                ( MonadAsync       m
                , MonadMask        m
                , Show failure
                , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                , ShowProxy ps
                )
             => Tracer m (TraceSendRecv ps)
             -> Codec ps failure f m bytes
             -> Channel m bytes
             -> Driver ps pr bytes failure (Maybe bytes) f m
driverStateful tracer Codec{encode, decode} channel@Channel{send} = do
    Driver { sendMessage
           , recvMessage
           , initialDState = Nothing
           }
  where
    sendMessage :: forall (st :: ps) (st' :: ps).
                   StateTokenI st
                => ActiveState st
                => ReflRelativeAgency (StateAgency st)
                                       WeHaveAgency
                                      (Relative pr (StateAgency st))
                -> f st'
                -> Message ps st st'
                -> m ()
    sendMessage !_ f msg = do
      send (encode f msg)
      traceWith tracer (TraceSendMsg (AnyMessage msg))

    recvMessage :: forall (st :: ps).
                   StateTokenI st
                => ActiveState st
                => ReflRelativeAgency (StateAgency st)
                                        TheyHaveAgency
                                       (Relative pr (StateAgency st))
                -> f st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage !_ f trailing = do
      let tok = stateToken
      decoder <- decode tok f
      result  <- runDecoderWithChannel channel trailing decoder
      case result of
        Right x@(SomeMessage msg, _trailing') -> do
          traceWith tracer (TraceRecvMsg (AnyMessage msg))
          return x
        Left failure ->
          throwIO (DecoderFailure tok failure)


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr failure bytes f m a .
     ( MonadAsync       m
     , MonadMask        m
     , Show failure
     , forall (st' :: ps) stok. stok ~ StateToken st' => Show stok
     , ShowProxy ps
     )
  => Tracer m (TraceSendRecv ps)
  -> Codec ps failure f m bytes
  -> Channel m bytes
  -> f st
  -> Peer ps pr st f m a
  -> m (a, Maybe bytes)
runPeer tracer codec channel f peer =
    runPeerWithDriver driver f peer
  where
    driver = driverStateful tracer codec channel



--
-- Utils
--

-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoderWithChannel :: Monad m
                      => Channel m bytes
                      -> Maybe bytes
                      -> DecodeStep bytes failure m a
                      -> m (Either failure (a, Maybe bytes))

runDecoderWithChannel Channel{recv} = go
  where
    go _ (DecodeDone x trailing)         = return (Right (x, trailing))
    go _ (DecodeFail failure)            = return (Left failure)
    go Nothing         (DecodePartial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing


-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: forall ps pr st failure bytes f m a b.
                     ( MonadAsync       m
                     , MonadMask        m
                     , Show failure
                     , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
                     , ShowProxy ps
                     )
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (Role, TraceSendRecv ps)
                  -> Codec ps failure f m bytes
                  -> f st
                  -> Peer ps             pr  st f m a
                  -> Peer ps (FlipAgency pr) st f m b
                  -> m (a, b)
runConnectedPeers createChannels tracer codec f client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (do labelThisThread "client"
        fst <$> runPeer tracerClient codec clientChannel f client
    )
      `concurrently`
    (do labelThisThread "server"
        fst <$> runPeer tracerServer codec serverChannel f server
    )
  where
    tracerClient = contramap (Client,) tracer
    tracerServer = contramap (Server,) tracer


-- Run the same protocol with different codes.  This is useful for testing
-- 'Handshake' protocol which knows how to decode different versions.
--
runConnectedPeersAsymmetric
    :: ( MonadAsync       m
       , MonadMask        m
       , Show failure
       , forall (st' :: ps) tok. tok ~ StateToken st' => Show tok
       , ShowProxy ps
       )
    => m (Channel m bytes, Channel m bytes)
    -> Tracer m (Role, TraceSendRecv ps)
    -> Codec ps failure f m bytes
    -> Codec ps failure f m bytes
    -> f st
    -> Peer ps             pr  st f m a
    -> Peer ps (FlipAgency pr) st f m b
    -> m (a, b)
runConnectedPeersAsymmetric createChannels tracer codec codec' f client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    (fst <$> runPeer tracerClient codec  clientChannel f client)
      `concurrently`
    (fst <$> runPeer tracerServer codec' serverChannel f server)
  where
    tracerClient = contramap (Client,) tracer
    tracerServer = contramap (Server,) tracer

