{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Network.TypedProtocol.Driver.Simple (

  -- * Introduction
  -- $intro

  -- * Normal peers
  runPeer,
  TraceSendRecv(..),

  -- * Pipelined peers
  runPipelinedPeer,

  -- * Connected peers
  runConnectedPeers,
  runConnectedPeersPipelined,

  -- * Driver utilities
  -- | This may be useful if you want to write your own driver.
  driverSimple,
  runDecoderWithChannel,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Driver.General

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer (..), traceWith)


-- $intro
--
-- A 'Peer' is a particular implementation of an agent that engages in a
-- typed protocol. To actualy run one we need a source and sink for the typed
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

-- | Structured 'Tracer' output for 'runPeer' and derivitives.
--
data TraceSendRecv ps peerid failure where
     TraceSendMsg
       :: peerid
       -> AnyMessage ps
       -> TraceSendRecv ps peerid failure
     TraceRecvMsg
       :: peerid
       -> AnyMessage ps
       -> TraceSendRecv ps peerid failure
     TraceDecoderFailure
       :: forall (pr :: PeerRole) (st :: ps) peerid failure.
          peerid
       -> PeerHasAgency pr st
       -> failure
       -> TraceSendRecv ps peerid failure

-- requires @UndecidableInstances@ extension
instance ( Show peerid
         , Show (AnyMessage ps)
         , forall (pr :: PeerRole) (st :: ps). Show (PeerHasAgency pr st)
         , Show failure
         ) => Show (TraceSendRecv ps peerid failure) where
  show (TraceSendMsg peerid msg) = "Send " ++ show peerid ++ " " ++ show msg
  show (TraceRecvMsg peerid msg) = "Recv " ++ show peerid ++ " " ++ show msg
  show (TraceDecoderFailure peerid stok err) = mconcat
    [ "DecoderFailure "
    , show peerid
    , " "
    , show stok
    , " "
    , show err
    ]


driverSimple :: forall ps peerid failure bytes m.
                (MonadThrow m, Exception failure)
             => Tracer m (TraceSendRecv ps peerid failure)
             -> peerid
             -> Codec ps failure m bytes
             -> Channel m bytes
             -> Driver ps (Maybe bytes) m
driverSimple tr peerid Codec{encode, decode} channel@Channel{send} =
    Driver { sendMessage, recvMessage, startDState = Nothing }
  where
    sendMessage :: forall (pr :: PeerRole) (st :: ps) (st' :: ps).
                   PeerHasAgency pr st
                -> Message ps st st'
                -> m ()
    sendMessage stok msg = do
      send (encode stok msg)
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))

    recvMessage :: forall (pr :: PeerRole) (st :: ps).
                   PeerHasAgency pr st
                -> Maybe bytes
                -> m (SomeMessage st, Maybe bytes)
    recvMessage stok trailing = do
      decoder <- decode stok
      result  <- runDecoderWithChannel channel trailing decoder
      case result of
        Right x@(SomeMessage msg, _trailing') -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
          return x
        Left failure -> do
          traceWith tr (TraceDecoderFailure peerid stok failure)
          throwM failure


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr peerid failure bytes m a .
     (MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a
runPeer tr codec peerid channel peer =
    fst <$> runPeerWithDriver driver peer (startDState driver)
  where
    driver = driverSimple tr peerid codec channel


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeer
  :: forall ps (st :: ps) pr peerid failure bytes m a.
     (MonadSTM m, MonadAsync m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a
runPipelinedPeer tr codec peerid channel peer =
    fst <$> runPipelinedPeerWithDriver driver peer (startDState driver)
  where
    driver = driverSimple tr peerid codec channel


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
    go _ (DecodeDone x trailing) = return (Right (x, trailing))
    go _ (DecodeFail failure)    = return (Left failure)
    go Nothing         (DecodePartial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing


-- | Run two 'Peer's via a pair of connected 'Channel's and a common 'Codec'.
--
-- This is useful for tests and quick experiments.
--
-- The first argument is expected to create two channels that are connected,
-- for example 'createConnectedChannels'.
--
runConnectedPeers :: (MonadSTM m, MonadAsync m, MonadCatch m,
                      Exception failure)
                  => m (Channel m bytes, Channel m bytes)
                  -> Tracer m (TraceSendRecv ps peerid failure)
                  -> Codec ps failure m bytes
                  -> peerid
                  -> peerid
                  -> Peer ps pr st m a
                  -> Peer ps (FlipAgency pr) st m b
                  -> m (a, b)
runConnectedPeers createChannels tr codec clientPeerid serverPeerid client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    runPeer tr codec clientPeerid clientChannel client
      `concurrently`
    runPeer tr codec serverPeerid serverChannel server


runConnectedPeersPipelined :: (MonadSTM m, MonadAsync m, MonadCatch m,
                               Exception failure)
                           => m (Channel m bytes, Channel m bytes)
                           -> Tracer m (TraceSendRecv ps peerid failure)
                           -> Codec ps failure m bytes
                           -> peerid
                           -> peerid
                           -> PeerPipelined ps pr st m a
                           -> Peer          ps (FlipAgency pr) st m b
                           -> m (a, b)
runConnectedPeersPipelined createChannels tr codec clientPeerid serverPeerid client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    runPipelinedPeer tr codec clientPeerid clientChannel client
      `concurrently`
    runPeer          tr codec serverPeerid serverChannel server

