{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Network.TypedProtocol.ReqResp.Server where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.ReqResp.Type


data ReqRespServer req resp m a = ReqRespServer {
    -- | The client sent us a ping message. We have no choices here, and
    -- the response is nullary, all we have are local effects.
    recvMsgReq  :: req -> m (resp, ReqRespServer req resp m a)

    -- | The client terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }


-- | Interpret a particular server action sequence into the server side of the
-- 'ReqResp' protocol.
--
reqRespServerPeer
  :: Monad m
  => ReqRespServer req resp m a
  -> Peer (ReqResp req resp) AsServer StIdle m a
reqRespServerPeer ReqRespServer{..} =

    -- In the 'StIdle' the server is awaiting a request message
    Await (ClientAgency TokIdle) $ \msg ->

    -- The client got to choose between two messages and we have to handle
    -- either of them
    case msg of

      -- The client sent the done transition, so we're in the 'StDone' state
      -- so all we can do is stop using 'done', with a return value.
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

      -- The client sent us a ping request, so now we're in the 'StBusy' state
      -- which means it's the server's turn to send.
      MsgReq req -> Effect $ do
        (resp, next) <- recvMsgReq req
        pure $ Yield (ServerAgency TokBusy) (MsgResp resp) (reqRespServerPeer next)
