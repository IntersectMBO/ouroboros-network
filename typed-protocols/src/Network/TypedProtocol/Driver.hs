{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeInType #-}

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
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> Peer pk st m a
  -> m a

runPeer Codec{encode, decode} channel@Channel{send} = go Nothing
  where
    go :: forall st'. Maybe bytes -> Peer pk st' m a -> m a
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
        Left failure            -> error "TODO: proper exceptions for runPeer"


runDecoder :: Monad m
           => Channel m bytes
           -> Maybe bytes
           -> Codec.DecodeStep bytes failure m a
           -> m (Either failure (a, Maybe bytes))

runDecoder Channel{recv} = go
  where
    go _ (Codec.Done x trailing) = return (Right (x, trailing))
    go _ (Codec.Fail failure)    = return (Left failure)
    go Nothing         (Codec.Partial k) = recv >>= k        >>= go Nothing
    go (Just trailing) (Codec.Partial k) = k (Just trailing) >>= go Nothing


runPipelinedPeer
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerSender pk st m a
  -> m a
runPipelinedPeer codec channel peer = do
    queue <- atomically $ newTBQueue 10  --TODO: size?
    fork $ runPipelinedPeerReceiverQueue queue codec channel
    runPipelinedPeerSender queue codec channel peer
    --TODO: manage the fork + exceptions here


data ReceiveHandler pk ps m where
     ReceiveHandler :: PeerReceiver pk (st :: ps) (st' :: ps) m
                    -> ReceiveHandler pk ps m


runPipelinedPeerSender
  :: forall ps (st :: ps) pk failure bytes m a.
     MonadSTM m
  => TBQueue m (ReceiveHandler pk ps m)
  -> Codec ps pk failure m bytes
  -> Channel m bytes
  -> PeerSender pk st m a
  -> m a
runPipelinedPeerSender queue Codec{encode} Channel{send} = go
  where
    go :: forall st'. PeerSender pk st' m a -> m a
    go (SenderEffect k) = k >>= go
    go (SenderDone _ x) = return x

    go (PipelinedYield stok msg receiver k) = do
      atomically (writeTBQueue queue (ReceiveHandler receiver))
      send (encode stok msg)
      go k

    go (SenderYield stok msg k) = do
      send (encode stok msg)
      go k


runPipelinedPeerReceiverQueue
  :: forall ps pk failure bytes m.
     MonadSTM m
  => TBQueue m (ReceiveHandler pk ps m)
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
  -> PeerReceiver pk (st :: ps) (stdone :: ps) m
  -> m (Maybe bytes)
runPipelinedPeerReceiver Codec{decode} channel = go
  where
    go :: forall st' st''.
          Maybe bytes
       -> PeerReceiver pk st' st'' m
       -> m (Maybe bytes)
    go trailing (ReceiverEffect k) = k >>= go trailing

    go trailing ReceiverDone = return trailing

    go trailing (ReceiverAwait stok k) = do
      decoder <- decode stok
      res <- runDecoder channel trailing decoder
      case res of
        Right (SomeMessage msg, trailing') -> go trailing' (k msg)
        Left failure                       -> error "TODO: proper exceptions for runPipelinedPeer"

