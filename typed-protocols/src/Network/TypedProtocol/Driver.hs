{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}

--TODO: just temporary while hacking:

-- |
-- = Driving a Peer by was of a Duplex and Channel
--
-- A 'Duplex' allows for sending and receiving pieces of some concrete type.
-- In applications, this will probably be some sort of socket. In order to
-- use it to drive a typed protocol application (represented by a 'Peer'),
-- there must be a way to encode typed transitions of that protocol to the
-- concrete type, and to parse pieces of that concrete type incrementally into
-- a typed transition. This is defined by a 'Codec'.
--
-- A 'Codec' and a 'Duplex' alone is not enough to do encoding and decoding,
-- because the 'Codec' does not make any /decisions/ about the way in which
-- the protocol application progresses. It defines encodings for /all/ possible
-- transitions from a state, and an inverse for that encoder. It's the 'Peer'
-- term which decides which transitions to encode, thereby leading the 'Codec'
-- through a path in the protocol type.
--
-- Driving a 'Peer' in this way may give rise to an exception, given by
-- @'Unexpected' :: 'Result' t@.

module Network.TypedProtocol.Driver where

import Network.TypedProtocol.Core
import Network.TypedProtocol.Pipelined
import Network.TypedProtocol.Channel

import qualified Network.TypedProtocol.Codec as Codec
import           Network.TypedProtocol.Codec hiding (Done)

import           Control.Monad.Class.MonadSTM



runPeer
  :: forall ps (st :: ps) pk failure bytes m a .
     Monad m
  => Codec pk ps failure m bytes
  -> Channel m bytes
  -> Peer pk st m a
  -> m a

runPeer codec channel (Effect k) =
    k >>= runPeer codec channel

runPeer _codec _channel (Done _ x) =
    return x

runPeer codec@Codec{encode} Channel{send} (Yield stok msg k) = do
    channel' <- send (encode stok msg)
    runPeer codec channel' k

runPeer codec@Codec{decode} channel (Await stok k) = do
    decoder <- decode stok
    res <- runDecoder channel decoder
    case res of
      Right (SomeMessage msg, channel') -> runPeer codec channel' (k msg)
      Left failure                      -> error "TODO: proper exceptions for runPeer"


runDecoder :: Monad m
           => Channel m bytes
           -> Codec.DecodeStep bytes failure m a
           -> m (Either failure (a, Channel m bytes))

runDecoder channel (Codec.Done x Nothing) =
    return (Right (x, channel))

runDecoder channel (Codec.Done x (Just trailing)) =
    return (Right (x, prependChannelRecv trailing channel))

runDecoder _channel (Codec.Fail failure) =
    return (Left failure)

runDecoder Channel{recv} (Codec.Partial k) = do
    (minput, channel') <- recv
    runDecoder channel' =<< k minput


runPipelinedPeer
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => Codec pk ps failure m bytes
  -> Channel m bytes
  -> PeerSender pk st m a
  -> m a
runPipelinedPeer codec channel peer = do
    queue <- atomically $ newTBQueue 10  --TODO: size?
    fork $ manageReceiverQueue queue channel
    runPipelinedPeerSender queue codec channel peer
  where
    --TODO: here we're forking the channel, which breaks it's invariants
    manageReceiverQueue queue channel = do
      ReceiveHandler receiver <- atomically (readTBQueue queue)
      channel' <- runPipelinedPeerReceiver codec channel receiver
      manageReceiverQueue queue channel'


data ReceiveHandler pk ps m where
     ReceiveHandler :: PeerReceiver pk (st :: ps) (st' :: ps) m
                    -> ReceiveHandler pk ps m


runPipelinedPeerSender
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => TBQueue m (ReceiveHandler pk ps m)
  -> Codec pk ps failure m bytes
  -> Channel m bytes
  -> PeerSender pk st m a
  -> m a
runPipelinedPeerSender queue Codec{encode} = go
  where
    go :: forall st'.
          Channel m bytes
       -> PeerSender pk st' m a
       -> m a
    go  channel (SenderEffect k) = k >>= go channel

    go _channel (SenderDone _ x) = return x

    go Channel{send} (SenderYield stok msg receiver k) = do
      atomically (writeTBQueue queue (ReceiveHandler receiver))
      channel' <- send (encode stok msg)
      go channel' k


runPipelinedPeerReceiver
  :: forall ps (st :: ps) (st' :: ps) pk failure bytes m.
     Monad m
  => Codec pk ps failure m bytes
  -> Channel m bytes
  -> PeerReceiver pk (st :: ps) (st' :: ps) m
  -> m (Channel m bytes)
runPipelinedPeerReceiver Codec{decode} = go
  where
    go :: forall st st'.
          Channel m bytes
       -> PeerReceiver pk st st' m
       -> m (Channel m bytes)
    go channel (ReceiverEffect k) = k >>= go channel

    go channel ReceiverDone = return channel

    go channel (ReceiverAwait stok k) = do
      decoder <- decode stok
      res <- runDecoder channel decoder
      case res of
        Right (SomeMessage msg, channel') -> go channel' (k msg)
        Left failure                      -> error "TODO: proper exceptions for runPipelinedPeer"

