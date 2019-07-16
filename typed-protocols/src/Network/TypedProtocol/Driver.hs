{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
-- @UndecidableInstances@ extensions is required for defining @Show@ instance
-- of @'TraceSendRecv'@.
{-# LANGUAGE UndecidableInstances #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Network.TypedProtocol.Driver (

  -- * Introduction
  -- $intro

  -- ** Exception handling
  -- | TODO: This remains to be clarified.

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
  runDecoderWithChannel,
  ) where

import Data.Void (Void)

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec

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


--
-- Driver for normal peers
--

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

runPeer tr Codec{encode, decode} peerid channel@Channel{send} =
    go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pr st' m a
       -> m a
    go  trailing (Effect k) = k >>= go trailing
    go _trailing (Done _ x) = return x
    -- TODO: we should return the trailing here to allow for one protocols to
    -- be run after another on the same channel. It could be the case that the
    -- opening message of the next protocol arrives in the same data chunk as
    -- the final message of the previous protocol. This would also mean we
    -- would need to pass in any trailing data as an input. Alternatively we
    -- would need to move to a Channel type that can push back trailing data.

    go trailing (Yield stok msg k) = do
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure

    -- Note that we do not complain about trailing data in any case, neither
    -- the 'Await' nor 'Done' cases.
    --
    -- We want to be able to use a non-pipelined peer in communication with
    -- a pipelined peer, and in that case the non-pipelined peer will in
    -- general see trailing data after an 'Await' which is the next incoming
    -- message.
    --
    -- Likewise for 'Done', we want to allow for one protocols to be run after
    -- another on the same channel. It would be legal for the opening message
    -- of the next protocol arrives in the same data chunk as the final
    -- message of the previous protocol.


--
-- Driver for pipelined peers
--

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
runPipelinedPeer tr codec peerid channel (PeerPipelined peer) = do
    receiveQueue <- atomically newTQueue
    collectQueue <- atomically newTQueue
    a <- runPipelinedPeerReceiverQueue tr receiveQueue collectQueue
                                          codec peerid channel
           `withAsyncLoop`
         runPipelinedPeerSender        tr receiveQueue collectQueue
                                          codec peerid channel peer
    return a

  where
    withAsyncLoop :: m Void -> m x -> m x
    withAsyncLoop left right = do
      -- race will throw if either of the threads throw
      res <- race left right
      case res of
        Left v  -> case v of {}
        Right a -> return a

data ReceiveHandler bytes ps pr m c where
     ReceiveHandler :: MaybeTrailing bytes n
                    -> PeerReceiver ps pr (st :: ps) (st' :: ps) m c
                    -> ReceiveHandler bytes ps pr m c

-- | The handling of trailing data here is quite subtle. Trailing data is data
-- we have read from the channel but the decoder has told us that it comes
-- after the message we decoded. So it potentially belongs to the next message
-- to decode.
--
-- We read from the channel on both the 'runPipelinedPeerSender' and the
-- 'runPipelinedPeerReceiver', and we synchronise our use of trailing data
-- between the two. The scheme for the sender and receiver threads using the
-- channel ensures that only one can use it at once:
--
-- * When there are zero outstanding pipelined receiver handlers then the
--   sending side is allowed to access the channel directly (to do synchronous
--   yield\/awaits). Correspondingly the receiver side is idle and not
--   accessing the channel.
-- * When there are non-zero outstanding pipelined receiver handlers then
--   the receiver side can access the channel, but the sending side is not
--   permitted to do operations that access the channel.
--
-- So the only times we need to synchronise the trailing data are the times
-- when the right to access the channel passes from one side to the other.
--
-- The transitions are as follows:
--
-- * There having been Zero outstanding pipelined requests there is now a
--   new pipelined yield. In this case we must pass the trailing data from
--   the sender thread to the receiver thread. We pass it with the
--   'ReceiveHandler'.
--
-- * When the last pipelined request is collected. In this case we must pass
--   the trailing data from the receiver thread to the sender thread. We pass
--   it with the collected result.
--
-- Note that the receiver thread cannot know what the last pipelined request
-- is, that is tracked on the sender side. So the receiver thread always
-- returns the trailing data with every collected result. It is for the sender
-- thread to decide if it needs to use it. For the same reason, the receiver
-- thread ends up retaining the last trailing data (as well as passing it to
-- the sender). So correspondingly when new trailing data is passed to the
-- receiver thread, it simply overrides any trailing data it already had, since
-- we now know that copy to be stale.
--
data MaybeTrailing bytes (n :: N) where
     MaybeTrailing :: Maybe bytes -> MaybeTrailing bytes Z
     NoTrailing    ::                MaybeTrailing bytes (S n)


runPipelinedPeerSender
  :: forall ps (st :: ps) pr peerid failure bytes c m a.
     (MonadSTM m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> TQueue m (ReceiveHandler bytes ps pr m c)
  -> TQueue m (c, Maybe bytes)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> PeerSender ps pr st Z c m a
  -> m a
runPipelinedPeerSender tr receiveQueue collectQueue
                       Codec{encode, decode} peerid channel@Channel{send} =
    go Zero (MaybeTrailing Nothing)
  where
    go :: forall st' n.
          Nat n
       -> MaybeTrailing bytes n
       -> PeerSender ps pr st' n c m a
       -> m a
    go n     trailing (SenderEffect k) = k >>= go n trailing
    go Zero _trailing (SenderDone _ x) = return x
    --TODO: same issue with trailing as the 'Done' case for 'runPeer' above.

    go Zero trailing (SenderYield stok msg k) = do
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))
      send (encode stok msg)
      go Zero trailing k

    go Zero (MaybeTrailing trailing) (SenderAwait stok k) = do
      decoder <- decode stok
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
          go Zero (MaybeTrailing trailing') (k msg)
        Left failure -> do
          traceWith tr (TraceDecoderFailure peerid stok failure)
          throwM failure

    go n trailing (SenderPipeline stok msg receiver k) = do
      atomically (writeTQueue receiveQueue (ReceiveHandler trailing receiver))
      traceWith tr (TraceSendMsg peerid (AnyMessage msg))
      send (encode stok msg)
      go (Succ n) NoTrailing k

    go (Succ n) NoTrailing (SenderCollect Nothing k) = do
      (c, trailing) <- atomically (readTQueue collectQueue)
      case n of
        Zero    -> go Zero      (MaybeTrailing trailing) (k c)
        Succ n' -> go (Succ n')  NoTrailing              (k c)

    go (Succ n) NoTrailing (SenderCollect (Just k') k) = do
      mc <- atomically (tryReadTQueue collectQueue)
      case mc of
        Nothing  -> go (Succ n) NoTrailing  k'
        Just (c, trailing) ->
          case n of
            Zero    -> go Zero      (MaybeTrailing trailing) (k c)
            Succ n' -> go (Succ n')  NoTrailing              (k c)


-- NOTE: @'runPipelinedPeer'@ assumes that @'runPipelinedPeerReceiverQueue'@ is
-- an infinite loop which never returns.
runPipelinedPeerReceiverQueue
  :: forall ps pr peerid failure bytes m c.
     (MonadSTM m, MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> TQueue m (ReceiveHandler bytes ps pr m c)
  -> TQueue m (c, Maybe bytes)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> m Void
runPipelinedPeerReceiverQueue tr receiveQueue collectQueue codec peerid channel =
    go Nothing
  where
    go :: Maybe bytes -> m Void
    go receiverTrailing = do
      ReceiveHandler senderTrailing receiver
        <- atomically (readTQueue receiveQueue)
      let trailing = case (senderTrailing, receiverTrailing) of
                       (MaybeTrailing t, _) -> t
                       (NoTrailing,      t) -> t
      (c, trailing') <- runPipelinedPeerReceiver tr codec peerid channel trailing receiver
      atomically (writeTQueue collectQueue (c, trailing'))
      go trailing'


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (stdone :: ps) pr peerid failure bytes m c.
     (MonadThrow m, Exception failure)
  => Tracer m (TraceSendRecv ps peerid failure)
  -> Codec ps failure m bytes
  -> peerid
  -> Channel m bytes
  -> Maybe bytes
  -> PeerReceiver ps pr (st :: ps) (stdone :: ps) m c
  -> m (c, Maybe bytes)
runPipelinedPeerReceiver tr Codec{decode} peerid channel = go
  where
    go :: forall st' st''.
          Maybe bytes
       -> PeerReceiver ps pr st' st'' m c
       -> m (c, Maybe bytes)
    go trailing (ReceiverEffect k) = k >>= go trailing

    go trailing (ReceiverDone x) = return (x, trailing)

    go trailing (ReceiverAwait stok k) = do
      decoder <- decode stok
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> do
          traceWith tr (TraceRecvMsg peerid (AnyMessage msg))
          go trailing' (k msg)
        Left failure ->
          throwM failure


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

