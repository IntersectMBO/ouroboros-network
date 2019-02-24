{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}

-- | Drivers for running 'Peer's with a 'Codec' and a 'Channel'.
--
module Network.TypedProtocol.Driver (

  -- * Introduction
  -- $intro

  -- ** Exception handling
  -- | TODO: This remains to be clarified.

  -- * Normal peers
  runPeer,

  -- * Pipelined peers
  runPipelinedPeer,

  -- * Connected peers
  runConnectedPeers,
  runConnectedPeersPipelined,

  -- * Driver utilities
  -- | This may be useful if you want to write your own driver.
  runDecoderWithChannel,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow

import Numeric.Natural (Natural)


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


--
-- Driver for normal peers
--

-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pr failure bytes m a .
     (MonadThrow m, Exception failure)
  => Codec ps failure m bytes
  -> Channel m bytes
  -> Peer ps pr st m a
  -> m a

runPeer Codec{encode, decode} channel@Channel{send} =
    go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pr st' m a
       -> m a
    go trailing (Effect k) = k >>= go trailing
    go _        (Done _ x) = return x

    go trailing (Yield stok msg k) = do
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoderWithChannel channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left failure                       -> throwM failure


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
  :: forall ps (st :: ps) pr failure bytes m a.
     (MonadSTM m, MonadAsync m, MonadCatch m, Exception failure)
  => Natural
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pr st m a
  -> m a
runPipelinedPeer maxOutstanding codec channel (PeerPipelined peer) = do
    receiveQueue <- atomically $ newTBQueue maxOutstanding
    collectQueue <- atomically $ newTBQueue maxOutstanding
    snd <$> runPipelinedPeerReceiverQueue receiveQueue collectQueue
                                          codec channel
              `concurrently`
            runPipelinedPeerSender        receiveQueue collectQueue
                                          codec channel peer


data ReceiveHandler ps pr m c where
     ReceiveHandler :: PeerReceiver ps pr (st :: ps) (st' :: ps) m c
                    -> ReceiveHandler ps pr m c


runPipelinedPeerSender
  :: forall ps (st :: ps) pr failure bytes c m a.
     MonadSTM m
  => TBQueue m (ReceiveHandler ps pr m c)
  -> TBQueue m c
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> PeerSender ps pr st Z c m a
  -> m a
runPipelinedPeerSender receiveQueue collectQueue Codec{encode} Channel{send} =
    go Zero
  where
    go :: forall st' n. Nat n -> PeerSender ps pr st' n c m a -> m a
    go n    (SenderEffect k) = k >>= go n
    go Zero (SenderDone _ x) = return x

    go n (SenderYield stok msg k) = do
      send (encode stok msg)
      go n k

    go n (SenderPipeline stok msg receiver k) = do
      atomically (writeTBQueue receiveQueue (ReceiveHandler receiver))
      send (encode stok msg)
      go (Succ n) k

    go (Succ n) (SenderCollect Nothing k) = do
      c <- atomically (readTBQueue collectQueue)
      go n (k c)

    go (Succ n) (SenderCollect (Just k') k) = do
      mc <- atomically (tryReadTBQueue collectQueue)
      case mc of
        Nothing -> go (Succ n) k'
        Just c  -> go n (k c)


runPipelinedPeerReceiverQueue
  :: forall ps pr failure bytes m c.
     (MonadSTM m, MonadThrow m, Exception failure)
  => TBQueue m (ReceiveHandler ps pr m c)
  -> TBQueue m c
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> m ()
runPipelinedPeerReceiverQueue receiveQueue collectQueue codec channel =
    go Nothing
  where
    go :: Maybe bytes -> m ()
    go trailing = do
      ReceiveHandler receiver <- atomically (readTBQueue receiveQueue)
      (c, trailing') <- runPipelinedPeerReceiver codec channel trailing receiver
      atomically (writeTBQueue collectQueue c)
      go trailing'


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (stdone :: ps) pr failure bytes m c.
     (MonadThrow m, Exception failure)
  => Codec ps failure m bytes
  -> Channel m bytes
  -> Maybe bytes
  -> PeerReceiver ps pr (st :: ps) (stdone :: ps) m c
  -> m (c, Maybe bytes)
runPipelinedPeerReceiver Codec{decode} channel = go
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
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left failure                       -> throwM failure


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
                  -> Codec ps failure m bytes
                  -> Peer ps AsClient st m a
                  -> Peer ps AsServer st m b
                  -> m (a, b)
runConnectedPeers createChannels codec client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    runPeer codec clientChannel client
      `concurrently`
    runPeer codec serverChannel server


runConnectedPeersPipelined :: (MonadSTM m, MonadAsync m, MonadCatch m,
                               Exception failure)
                           => m (Channel m bytes, Channel m bytes)
                           -> Codec ps failure m bytes
                           -> Natural
                           -> PeerPipelined ps AsClient st m a
                           -> Peer          ps AsServer st m b
                           -> m (a, b)
runConnectedPeersPipelined createChannels codec maxOutstanding client server =
    createChannels >>= \(clientChannel, serverChannel) ->

    runPipelinedPeer maxOutstanding codec clientChannel client
      `concurrently`
    runPeer                         codec serverChannel server

