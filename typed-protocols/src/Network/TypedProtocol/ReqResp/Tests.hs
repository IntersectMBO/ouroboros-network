{-# LANGUAGE NamedFieldPuns #-}
module Network.TypedProtocol.ReqResp.Tests where

import           Network.TypedProtocol.ReqResp.Client as Client
import           Network.TypedProtocol.ReqResp.Server as Server


-- TODO: this module needs to be fleshed out once the PingPong template is done

direct :: Monad m
       => ReqRespClient req resp m a
       -> ReqRespServer req resp m b
       -> m (a, b)

direct (Client.SendMsgDone clientResult) ReqRespServer{recvMsgDone} =
    pure (clientResult, recvMsgDone)

direct (Client.SendMsgReq req kPong) ReqRespServer{recvMsgReq} = do
    (resp, server') <- recvMsgReq req
    client' <- kPong resp
    direct client' server'
