{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

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

  -- * Driver utilities
  -- | This may be useful if you want to write your own driver.
  runDecoder,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec

import Control.Monad.Class.MonadSTM

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


-- | Run a peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
runPeer
  :: forall ps (st :: ps) pk failure bytes m a .
     Monad m
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> Peer ps pk st m a
  -> m (Either failure a)

runPeer Codec{encode, decode} channel@Channel{send} = go Nothing
  where
    go :: forall st'.
          Maybe bytes
       -> Peer ps pk st' m a
       -> m (Either failure a)
    go trailing (Effect k) = k >>= go trailing
    go _        (Done _ x) = return (Right x)

    go trailing (Yield stok msg k) = do
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoder channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left failure                       -> return (Left failure)


-- | Run a codec incremental decoder 'DecodeStep' against a channel. It also
-- takes any extra input data and returns any unused trailing data.
--
runDecoder :: Monad m
           => Channel m bytes
           -> Maybe bytes
           -> DecodeStep bytes failure m a
           -> m (Either failure (a, Maybe bytes))

runDecoder Channel{recv} = go
  where
    go _ (DecodeDone x trailing) = return (Right (x, trailing))
    go _ (DecodeFail failure)    = return (Left failure)
    go Nothing         (DecodePartial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (DecodePartial k) = k (Just trailing) >>= go Nothing


-- | Run a pipelined peer with the given channel via the given codec.
--
-- This runs the peer to completion (if the protocol allows for termination).
--
-- Unlike normal peers, running pipelined peers rely on concurrency, hence the
-- 'MonadSTM' constraint.
--
runPipelinedPeer
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => Natural
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerPipelined ps pk st m a
  -> m a
runPipelinedPeer maxOutstanding codec channel (PeerPipelined peer) = do
    receiveQueue <- atomically $ newTBQueue maxOutstanding
    collectQueue <- atomically $ newTBQueue maxOutstanding
    fork $ runPipelinedPeerReceiverQueue receiveQueue collectQueue
                                         codec channel
    runPipelinedPeerSender receiveQueue collectQueue
                           codec channel peer
    --TODO: manage the fork + exceptions here


data ReceiveHandler ps pk m c where
     ReceiveHandler :: PeerReceiver ps pk (st :: ps) (st' :: ps) m c
                    -> ReceiveHandler ps pk m c


runPipelinedPeerSender
  :: forall ps (st :: ps) pk failure bytes c m a.
     MonadSTM m
  => TBQueue m (ReceiveHandler ps pk m c)
  -> TBQueue m c
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerSender ps pk st Z c m a
  -> m a
runPipelinedPeerSender receiveQueue collectQueue Codec{encode} Channel{send} =
    go Zero
  where
    go :: forall st' n. Nat n -> PeerSender ps pk st' n c m a -> m a
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
      fail "TODO: need tryReadTBQueue"
--      mc <- atomically (tryReadTBQueue collectQueue)
--      case mc of
--        Nothing -> go (Succ n) k'
--        Just c  -> go n (k c)


runPipelinedPeerReceiverQueue
  :: forall ps pk failure bytes m c.
     MonadSTM m
  => TBQueue m (ReceiveHandler ps pk m c)
  -> TBQueue m c
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> m ()
runPipelinedPeerReceiverQueue receiveQueue collectQueue codec channel = go Nothing
  where
    go :: Maybe bytes -> m ()
    go trailing = do
      ReceiveHandler receiver <- atomically (readTBQueue receiveQueue)
      --TODO: use 'try' here once we have MonadCatch
      (c, trailing') <- runPipelinedPeerReceiver codec channel trailing receiver
      atomically (writeTBQueue collectQueue c)
      go trailing'


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (stdone :: ps) pk failure bytes m c.
     Monad m
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> Maybe bytes
  -> PeerReceiver ps pk (st :: ps) (stdone :: ps) m c
  -> m (c, Maybe bytes)
runPipelinedPeerReceiver Codec{decode} channel = go
  where
    go :: forall st' st''.
          Maybe bytes
       -> PeerReceiver ps pk st' st'' m c
       -> m (c, Maybe bytes)
    go trailing (ReceiverEffect k) = k >>= go trailing

    go trailing (ReceiverDone x) = return (x, trailing)

    go trailing (ReceiverAwait stok k) = do
      decoder <- decode stok
      res <- runDecoder channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left failure                       -> error "TODO: proper exceptions for runPipelinedPeer"

