{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Protocol.Stream.Server
  ( StreamServer (..)
  , ServerHandleData (..)
  , streamServerPeer
  , streamServer
  ) where

import Protocol.Core
import Pipes (Producer)
import qualified Pipes

import Ouroboros.Network.Protocol.Stream.Type

newtype StreamServer m range a t = StreamServer {
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

-- | Interpret @'StreamServer'@ as a @'Peer'@.
--
streamServerPeer
  :: forall m range a t. Monad m
  => StreamServer m range a t
  -> Peer StreamProtocol (StreamMessage range a)
      ('Awaiting 'StIdle) ('Finished 'StDone)
      m t
streamServerPeer StreamServer{recvMsgRequest}
  = await $ \msg ->
      case msg of
        MsgRequest range -> lift (serverHandleData <$> recvMsgRequest range)
 where
  serverHandleData
    :: ServerHandleData m a t
    -> Peer StreamProtocol (StreamMessage range a)
        ('Yielding 'StBusy) ('Finished 'StDone)
        m t
  serverHandleData (SendMsgData a mserver) =
    part (MsgData a) $ lift $ do
      server <- mserver
      return $ serverHandleData server
  serverHandleData (SendMsgStreamEnd t) =
    out MsgStreamEnd (done t)

-- | Create a @'StreamServer'@ from a @'Producer'@ stream of values.
--
streamServer
  :: forall m range a t. Monad m
  => (range -> Producer a m t)
  -> StreamServer m range a t
streamServer stream = StreamServer $ \range -> server (stream range)
 where
  server :: Producer a m t -> m (ServerHandleData m a t)
  server p =
    Pipes.next p >>= \case
      Left t        -> return (SendMsgStreamEnd t)
      Right (a, p') -> return (SendMsgData a (server p'))
