{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.UntypedStream.Client where

import Control.Monad (void)
import Data.Word (Word32)
import Protocol.Untyped

import Ouroboros.Network.Protocol.UntypedStream.Type
import Ouroboros.Network.MonadClass

data UntypedStreamClient m range a t where
  SendMsgRequest
    :: range
    -> Window
    -> Threshold
    -> ClientHandleData m a t
    -> UntypedStreamClient m range a t

data ClientHandleData m a t =
  ClientHandleData {
    recvMsgData        :: a -> m (ClientHandleData m a t),
    recvMsgStreamEnd   :: m t
  }

-- | Consumer part of the block streaming protocol.
--
-- When the consumer will receive `Threshold` messages it will send
-- `MsgUpdateWindow` with the difference between window and the threshold.
--
--  In the following digrams, lines from client to server represent
--  `MsgRequestBlock` (the initial line), or `MsgUpdateWindow`.  The lines from
--  server to client represent `MsgStreamBlock`.
--
--  `x` represent the point when the client will block (not sending next
--  `MsgUpdateQueue`).  This can happen after receiving `window + (window
--  - threshold)` messages.  This is the maximum number of allocations the
--  client must be prepared for.
--
--  window    = 3
--  threshold = 2
--
--  server -----------------------
--            / \ \ \  /\    /\
--           /   \ \ \/  \  /  \
--          /     \ \/\   \/    \
--  client ---------------x-------
--
--  window    = 4
--  threshold = 2
--
--  server -----------------------
--            / \ \ \ \/\ \  /\ \
--           /   \ \ \/\ \ \/  \ \
--          /     \ \/\ \ \/\   \ \
--  client ---------------x-------
--
untypedStreamClientPeer
  :: forall m range a t. Monad m
  => UntypedStreamClient m range a t
  -> Peer (StreamMessage range a) m t
untypedStreamClientPeer (SendMsgRequest range window threshold client) =
  yield (MsgRequest range window) $ clientHandleData 0 client
 where
  clientHandleData :: Word32 -> ClientHandleData m a t -> Peer (StreamMessage range a) m t
  clientHandleData writes client' | writes >= fromIntegral threshold =
    yield (MsgUpdateWindow (window - fromIntegral threshold)) (clientHandleData 0 client')
  clientHandleData writes ClientHandleData{recvMsgData,recvMsgStreamEnd} =
    await $ \(SomeMessage msg) -> case msg of
      MsgData a        -> Just $ lift (clientHandleData (succ writes) <$> recvMsgData a)
      MsgStreamEnd     -> Just $ lift (PeerDone <$> recvMsgStreamEnd)
      _                -> Nothing

-- | A specialised type used to put elements in a @'TBQueue'@.  Reading
-- @'EndOfStream'@ means no more elements will be written to the queue.
--
-- It is strict: the reader of a queue will pattern match on @'StreamElement'@,
-- this will force evaluation of carried data early (which will be either
-- written to disc or required by the running application anyway).
--
data StreamElement a
  = StreamElement !a
  | EndOfStream
  deriving (Eq, Ord, Show)

-- | Create an @'UntypedStreamClient'@ which writes to a @'TBQueue'@ of size
-- @'Window'@.  This means that the client will block after receiving @'Window'@
-- outstanding messages.
--
streamClient
  :: forall m range a. MonadSTM m
  => range
  -> Window
  -> Threshold
  -> m ( TBQueue m (StreamElement a)
       , UntypedStreamClient m range a ()
       )
streamClient range window threshold = do
  queue <- atomically $ newTBQueue (fromIntegral window)
  return (queue, SendMsgRequest range window threshold (client queue))
 where
  client :: TBQueue m (StreamElement a) -> ClientHandleData m a ()
  client queue = ClientHandleData {
    recvMsgData      = \a -> client queue <$ atomically (writeTBQueue queue (StreamElement a)),
    recvMsgStreamEnd = void $ atomically (writeTBQueue queue EndOfStream)
  }
