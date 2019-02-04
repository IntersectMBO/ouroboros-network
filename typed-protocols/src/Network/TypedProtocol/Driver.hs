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

  -- * Driver utilities
  -- | This may be useful if you want to write your own driver.
  runDecoder,
  ) where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel
import Network.TypedProtocol.Codec

import Control.Monad.Class.MonadSTM


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
  -> m a

runPeer Codec{encode, decode} channel@Channel{send} = go Nothing
  where
    go :: forall st'. Maybe bytes -> Peer ps pk st' m a -> m a
    go trailing (Effect k) = k >>= go trailing
    go _        (Done _ x) = return x

    go trailing (Yield stok msg k) = do
      send (encode stok msg)
      go trailing k

    go trailing (Await stok k) = do
      decoder <- decode stok
      res <- runDecoder channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left _failure                      -> error "TODO: proper exceptions for runPeer"


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
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerSender ps pk st m a
  -> m a
runPipelinedPeer codec channel peer = do
    queue <- atomically $ newTBQueue 10  --TODO: size?
    fork $ runPipelinedPeerReceiverQueue queue codec channel
    runPipelinedPeerSender queue codec channel peer
    --TODO: manage the fork + exceptions here


data ReceiveHandler ps pk m where
     ReceiveHandler :: PeerReceiver ps pk (st :: ps) (st' :: ps) m
                    -> ReceiveHandler ps pk m


runPipelinedPeerSender
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => TBQueue m (ReceiveHandler ps pk m)
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerSender ps pk st m a
  -> m a
runPipelinedPeerSender queue Codec{encode} Channel{send} = go
  where
    go :: forall st'. PeerSender ps pk st' m a -> m a
    go (SenderEffect k) = k >>= go
    go (SenderDone _ x) = return x

    go (SenderYield stok msg receiver k) = do
      atomically (writeTBQueue queue (ReceiveHandler receiver))
      send (encode stok msg)
      go k


runPipelinedPeerReceiverQueue
  :: forall ps pk failure bytes m.
     MonadSTM m
  => TBQueue m (ReceiveHandler ps pk m)
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> m ()
runPipelinedPeerReceiverQueue queue codec channel = go Nothing
  where
    go :: Maybe bytes -> m ()
    go trailing = do
      ReceiveHandler receiver <- atomically (readTBQueue queue)
      trailing' <- runPipelinedPeerReceiver codec channel trailing receiver
      go trailing'


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (stdone :: ps) pk failure bytes m.
     Monad m
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> Maybe bytes
  -> PeerReceiver ps pk (st :: ps) (stdone :: ps) m
  -> m (Maybe bytes)
runPipelinedPeerReceiver Codec{decode} channel = go
  where
    go :: forall st' st''.
          Maybe bytes
       -> PeerReceiver ps pk st' st'' m
       -> m (Maybe bytes)
    go trailing (ReceiverEffect k) = k >>= go trailing

    go trailing ReceiverDone = return trailing

    go trailing (ReceiverAwait stok k) = do
      decoder <- decode stok
      res <- runDecoder channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left _failure                      -> error "TODO: proper exceptions for runPipelinedPeer"

