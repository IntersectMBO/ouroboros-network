{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.ReqResp.Server where

import Protocol.Core
import Ouroboros.Network.Protocol.ReqResp.Type

newtype ReqRespServer m request response a = ReqRespServer {
    -- | The client requested data identified by `resourceId`.
    runReqRespServer :: request -> m (response, a)
  }

-- | Create server side of the @'ReqRespProtocol'@.
--
reqRespServerPeer
  :: Monad m
  => ReqRespServer m request response a
  -> Peer (ReqRespProtocol request response) (ReqRespMessage request response)
          (Awaiting StIdle) (Finished StDone)
          m a
reqRespServerPeer server =
  await $ \msg ->
  case msg of
    MsgRequest request -> lift $ do
      (resp, a) <- runReqRespServer server request
      return $ out (MsgResponse resp) (done a)
