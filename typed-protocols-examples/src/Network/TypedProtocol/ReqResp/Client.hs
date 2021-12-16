{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Network.TypedProtocol.ReqResp.Client
  ( -- * Normal client
    ReqRespClient (..)
  , reqRespClientPeer
    -- * Pipelined client
  , ReqRespClientPipelined (..)
  , ReqRespSender (..)
  , reqRespClientPeerPipelined
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Pipelined
import           Network.TypedProtocol.ReqResp.Type

data ReqRespClient req resp m a where
  SendMsgReq     :: req
                 -> (resp -> m (ReqRespClient req resp m a))
                 -> ReqRespClient req resp m a

  SendMsgDone    :: m a -> ReqRespClient req resp m a


-- | Interpret a particular client action sequence into the client side of the
-- 'ReqResp' protocol.
--
reqRespClientPeer
  :: Monad m
  => ReqRespClient req resp m a
  -> Peer (ReqResp req resp) AsClient StIdle m a

reqRespClientPeer (SendMsgDone result) =
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $ do
      r <- result
      return $ Yield (ClientAgency TokIdle) MsgDone (Done TokDone r)

reqRespClientPeer (SendMsgReq req next) =

    -- Send our message.
    Yield (ClientAgency TokIdle) (MsgReq req) $

    -- The type of our protocol means that we're now into the 'StBusy' state
    -- and the only thing we can do next is local effects or wait for a reply.
    -- We'll wait for a reply.
    Await (ServerAgency TokBusy) $ \(MsgResp resp) ->

    -- Now in this case there is only one possible response, and we have
    -- one corresponding continuation 'kPong' to handle that response.
    -- The pong reply has no content so there's nothing to pass to our
    -- continuation, but if there were we would.
      Effect $ do
        client <- next resp
        pure $ reqRespClientPeer client


--
-- Pipelined client
--

-- | A request-response client designed for running the 'ReqResp' protocol in
-- a pipelined way.
--
data ReqRespClientPipelined req resp m a where
  -- | A 'PingPongSender', but starting with zero outstanding pipelined
  -- responses, and for any internal collect type @c@.
  ReqRespClientPipelined ::
      ReqRespSender          req resp Z c m a
   -> ReqRespClientPipelined req resp     m a


data ReqRespSender req resp n c m a where
  -- | Send a `Req` message but alike in `ReqRespClient` do not await for the
  -- resopnse, instead supply a monadic action which will run on a received
  -- `Pong` message.
  SendMsgReqPipelined
    :: req
    -> (resp -> m c)                       -- receive action
    -> ReqRespSender req resp (S n) c m a  -- continuation
    -> ReqRespSender req resp    n  c m a

  CollectPipelined
    :: Maybe (ReqRespSender req resp (S n) c m a)
    -> (c ->  ReqRespSender req resp    n  c m a)
    ->        ReqRespSender req resp (S n) c m a

  -- | Termination of the req-resp protocol.
  SendMsgDonePipelined
    :: a -> ReqRespSender req resp Z c m a


-- | Interpret a pipelined client as a 'PeerPipelined' on the client side of
-- the 'ReqResp' protocol.
--
reqRespClientPeerPipelined
  :: Monad m
  => ReqRespClientPipelined req resp                  m a
  -> PeerPipelined (ReqResp req resp) AsClient StIdle m a
reqRespClientPeerPipelined (ReqRespClientPipelined peer) =
    PeerPipelined (reqRespClientPeerSender peer)


reqRespClientPeerSender
  :: Monad m
  => ReqRespSender       req resp                  n c m a
  -> PeerSender (ReqResp req resp) AsClient StIdle n c m a

reqRespClientPeerSender (SendMsgDonePipelined result) =
  -- Send `MsgDone` and complete the protocol
  SenderYield
    (ClientAgency TokIdle)
    MsgDone
    (SenderDone TokDone result)

reqRespClientPeerSender (SendMsgReqPipelined req receive next) =
  -- Pipelined yield: send `MsgReq`, immediately follow with the next step.
  -- Await for a response in a continuation.
  SenderPipeline
    (ClientAgency TokIdle)
    (MsgReq req)
    -- response handler
    (ReceiverAwait (ServerAgency TokBusy) $ \(MsgResp resp) ->
        ReceiverEffect $ do
          x <- receive resp
          return (ReceiverDone x))
    -- run the next step of the req-resp protocol.
    (reqRespClientPeerSender next)

reqRespClientPeerSender (CollectPipelined mNone collect) =
  SenderCollect
    (fmap reqRespClientPeerSender mNone)
    (reqRespClientPeerSender . collect)

