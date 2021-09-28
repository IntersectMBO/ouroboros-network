{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.ReqResp.Client
  ( -- * Normal client
    ReqRespClient (..)
  , reqRespClientPeer
    -- * Pipelined client
  , ReqRespClientPipelined (..)
  , ReqRespIdle (..)
  , reqRespClientPeerPipelined
    -- * Request once
  , requestOnce
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Peer.Client
import           Network.TypedProtocol.Peer.Server (Server)
import           Network.TypedProtocol.Proofs (connectNonPipelined)
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
  -> Client (ReqResp req resp) NonPipelined Empty StIdle m a

reqRespClientPeer (SendMsgDone result) =
    -- We do an actual transition using 'yield', to go from the 'StIdle' to
    -- 'StDone' state. Once in the 'StDone' state we can actually stop using
    -- 'done', with a return value.
    Effect $ do
      r <- result
      return $ Yield MsgDone (Done r)

reqRespClientPeer (SendMsgReq req next) =

    -- Send our message.
    Yield (MsgReq req) $

    -- The type of our protocol means that we're now into the 'StBusy' state
    -- and the only thing we can do next is local effects or wait for a reply.
    -- We'll wait for a reply.
    Await $ \(MsgResp resp) ->

    -- Now in this case there is only one possible response, and we have
    -- one corresponding continuation 'kPong' to handle that response.
    -- The pong reply has no content so there's nothing to pass to our
    -- continuation, but if there were we would.
      Effect $ do
        client <- next resp
        pure $ reqRespClientPeer client



requestOnce :: forall req resp m.
               Monad m
            => (forall x. Server (ReqResp req resp) NonPipelined Empty StIdle m x)
            -> (req -> m resp)
requestOnce server req = (\(resp, _, _) -> resp)
                     <$> reqRespClientPeer client `connectNonPipelined` server
  where
    client :: ReqRespClient req resp m resp
    client = SendMsgReq req $ \resp -> pure $ SendMsgDone (pure resp)


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
      ReqRespIdle            req resp Empty m a
   -> ReqRespClientPipelined req resp       m a


data ReqRespIdle req resp (q :: Queue (ReqResp req resp)) m a where
  -- | Send a `Req` message but alike in `ReqRespClient` do not await for the
  -- resopnse, instead supply a monadic action which will run on a received
  -- `Pong` message.
  SendMsgReqPipelined
    :: req
    -> ReqRespIdle req resp (q |> Tr StBusy StIdle) m a  -- continuation
    -> ReqRespIdle req resp  q                      m a

  CollectPipelined
    :: Maybe      (ReqRespIdle req resp (Tr StBusy StIdle <| q) m a)
    -> (resp -> m (ReqRespIdle req resp                      q  m a))
    ->             ReqRespIdle req resp (Tr StBusy StIdle <| q) m a

  -- | Termination of the req-resp protocol.
  SendMsgDonePipelined
    :: a -> ReqRespIdle req resp Empty m a


-- | Interpret a pipelined client as a 'Peer' on the client side of
-- the 'ReqResp' protocol.
--
reqRespClientPeerPipelined
  :: Functor m
  => ReqRespClientPipelined req resp                          m a
  -> Client (ReqResp req resp) 'Pipelined Empty StIdle m a
reqRespClientPeerPipelined (ReqRespClientPipelined peer) =
    reqRespClientPeerIdle peer


reqRespClientPeerIdle
  :: forall req resp (q :: Queue (ReqResp req resp)) m a.
     Functor m
  => ReqRespIdle   req resp                      q        m a
  -> Client (ReqResp req resp) 'Pipelined q StIdle m a

reqRespClientPeerIdle = go
  where
    go :: forall (q' :: Queue (ReqResp req resp)).
          ReqRespIdle   req resp                      q'        m a
       -> Client (ReqResp req resp) 'Pipelined q' StIdle m a

    go (SendMsgReqPipelined req next) =
      -- Pipelined yield: send `MsgReq`, immediately follow with the next step.
      -- Await for a response in a continuation.
      YieldPipelined
        (MsgReq req)
        (go next)

    go (CollectPipelined mNone collect) =
      Collect
        (go <$> mNone)
        (\(MsgResp resp) -> CollectDone $ Effect $ go <$> collect resp)

    go (SendMsgDonePipelined result) =
      -- Send `MsgDone` and complete the protocol
      Yield
        MsgDone
        (Done result)
