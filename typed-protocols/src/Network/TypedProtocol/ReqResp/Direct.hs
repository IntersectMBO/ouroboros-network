{-# LANGUAGE NamedFieldPuns #-}
module Network.TypedProtocol.ReqResp.Direct where

import           Network.TypedProtocol.ReqResp.Client as Client
import           Network.TypedProtocol.ReqResp.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
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
