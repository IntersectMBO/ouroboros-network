{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.UntypedStream.Server where

import Protocol.Untyped
import Pipes (Producer)
import qualified Pipes

import Ouroboros.Network.Protocol.UntypedStream.Type

newtype UntypedStreamServer m range a t = UntypedStreamServer {
    recvMsgRequest :: (range -> m (ServerHandleData m a t))
  }

data ServerHandleData m a t where
  SendMsgData
    :: a
    -> m (ServerHandleData m a t)
    -> ServerHandleData m a t

  SendMsgStreamEnd
    :: t
    -> ServerHandleData m a t

untypedStreamServerPeer
  :: forall m range a t. Monad m
  => UntypedStreamServer m range a t
  -> Peer (StreamMessage range a) m t
untypedStreamServerPeer UntypedStreamServer{recvMsgRequest}
  = await $ \(SomeMessage msg) ->
      case msg of
        MsgRequest range window  -> Just $ lift (serverHandleData window <$> recvMsgRequest range)
        _                      -> Nothing
 where
  serverHandleData
    :: Window
    -> ServerHandleData m a t
    -> Peer (StreamMessage range a) m t
  serverHandleData 0 server =
    await $ \(SomeMessage msg) -> case msg of
      MsgUpdateWindow window -> Just (serverHandleData window server)
      _                      -> Nothing
  serverHandleData window  (SendMsgData a mserver) =
    yield (MsgData a) $ lift $ do
      server <- mserver
      return $ serverHandleData (pred window) server
  serverHandleData _window (SendMsgStreamEnd t) =
    yield MsgStreamEnd (PeerDone t)

streamServerFromProducer
  :: forall m range a t. Monad m
  => (range -> Producer a m t)
  -> UntypedStreamServer m range a t
streamServerFromProducer stream = UntypedStreamServer $ \range -> server (stream range)
 where
  server :: Producer a m t -> m (ServerHandleData m a t)
  server p =
    Pipes.next p >>= \case
      Left t        -> return (SendMsgStreamEnd t)
      Right (a, p') -> return (SendMsgData a (server p'))
