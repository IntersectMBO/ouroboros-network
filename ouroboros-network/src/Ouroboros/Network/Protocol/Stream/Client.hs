{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.Stream.Client
    ( StreamElement (..)
    , StreamClient (..)
    , ClientHandleData (..)
    , streamClientPeer
    , streamClient
    ) where

import Control.Monad (void)

import Protocol.Core
import Ouroboros.Network.Protocol.Stream.Type

import Ouroboros.Network.MonadClass.MonadSTM (MonadSTM (..))

-- | A specialised type used to put elements in a @'TBQueue'@.  Reading
-- @'EndOfStream'@ means no more elements will be written to the queue.
--
-- It is strict: the reader of a queue will pattern match on @'StreamElement'@,
-- this will force evaluation of carried data early (which will be either
-- written to disc or required by the running application anyway).
--
data StreamElement a
  = StreamElement !a
  | EndOfStream Bool
  -- ^ if @False@ the server signaled an error with @'MsgNoData'@ message.
  deriving (Eq, Ord, Show)

-- | Client which accumulates a values delivered by a corresponding server.
--
data StreamClient m range a t where
  SendMsgRequest 
    :: range
    -> ClientHandleData m a t
    -> StreamClient m range a t

data ClientHandleData m a t =
  ClientHandleData {
    recvMsgData      :: a -> m (ClientHandleData m a t),
    recvMsgStreamEnd :: m t,
    recvMsgNoData    :: m t
  }

-- | Interpret @'StreamClient'@ as a @'Peer'@.
--
streamClientPeer
  :: forall m range a t. Monad m
  => StreamClient m range a t
  -> Peer StreamProtocol (StreamMessage range a)
      (Yielding StIdle) (Finished StDone)
      m t
streamClientPeer (SendMsgRequest range client) =
  over (MsgRequest range) $ clientHandleData client
 where
  clientHandleData
    :: ClientHandleData m a t
    -> Peer StreamProtocol (StreamMessage range a)
        (Awaiting StBusy) (Finished StDone)
        m t
  clientHandleData ClientHandleData{recvMsgData,recvMsgStreamEnd,recvMsgNoData} = await $ \msg -> case msg of
    MsgData a -> lift $ do
      client' <- recvMsgData a
      return $ clientHandleData client'
    MsgStreamEnd -> lift $ done <$> recvMsgStreamEnd
    MsgNoData -> lift $ done <$> recvMsgNoData

-- | Create a @'StreamClient'@ which writes to a @'TBQueue'@.
--
streamClient
  :: forall m range a. MonadSTM m
  => TBQueue m (StreamElement a)
  -> range
  -> StreamClient m range a ()
streamClient queue range = SendMsgRequest range client
 where
  client :: ClientHandleData m a ()
  client = ClientHandleData {
    recvMsgData      = \a -> client <$ atomically (writeTBQueue queue (StreamElement a)),
    recvMsgStreamEnd = void $ atomically (writeTBQueue queue (EndOfStream True)),
    recvMsgNoData    = void $ atomically (writeTBQueue queue (EndOfStream False))
  }
