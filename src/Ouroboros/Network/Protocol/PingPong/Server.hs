{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Ouroboros.Network.Protocol.PingPong.Server where

import Ouroboros.Network.Protocol.Typed
import Ouroboros.Network.Protocol.PingPong.Type


data ServerStream m a = ServerStream { 
    -- | The client sent us a ping message. We have no choices here, and
    -- the response is nullary, all we have are local effects.
    handlePing  :: m (ServerStream m a)

    -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , handleDone  :: a
  }


-- | Interpret a particular server action sequence into the server side of the
-- 'PingPong' protocol.
--
streamServer
  :: Monad m
  => ServerStream m a
  -> Peer PingPongProtocol PingPongMessage
          (Awaiting StIdle) (Finished StDone)
          m a
streamServer ServerStream{..} =

    -- In the 'StIdle' the server is awaiting a request message
    await $ \req ->

    -- The client got to choose between two messages and we have to handle
    -- either of them
    case req of

      -- The client sent the done transition, so we're in the 'StDone' state
      -- so all we can do is stop using 'done', with a return value.
      MsgDone -> done handleDone

      -- The client sent us a ping request, so now we're in the 'StBusy' state
      -- which means it's the server's turn to send.
      MsgPing -> hole $ do
        next <- handlePing
        pure $ over MsgPong (streamServer next)

