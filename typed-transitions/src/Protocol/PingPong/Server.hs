{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Protocol.PingPong.Server where

import Protocol.Core
import Protocol.PingPong.Type


data PingPongServer m a = PingPongServer {
    -- | The client sent us a ping message. We have no choices here, and
    -- the response is nullary, all we have are local effects.
    recvMsgPing  :: m (PingPongServer m a)

    -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone  :: a
  }


-- | Interpret a particular server action sequence into the server side of the
-- 'PingPong' protocol.
--
pingPongServerPeer
  :: Monad m
  => PingPongServer m a
  -> Peer PingPongProtocol PingPongMessage
          ('Awaiting 'StIdle) ('Finished 'StDone)
          m a
pingPongServerPeer PingPongServer{..} =

    -- In the 'StIdle' the server is awaiting a request message
    await $ \req ->

    -- The client got to choose between two messages and we have to handle
    -- either of them
    case req of

      -- The client sent the done transition, so we're in the 'StDone' state
      -- so all we can do is stop using 'done', with a return value.
      MsgDone -> done recvMsgDone

      -- The client sent us a ping request, so now we're in the 'StBusy' state
      -- which means it's the server's turn to send.
      MsgPing -> lift $ do
        next <- recvMsgPing
        pure $ over MsgPong (pingPongServerPeer next)

