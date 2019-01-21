{-# LANGUAGE NamedFieldPuns #-}
module Network.TypedProtocol.PingPong.Direct where

import           Network.TypedProtocol.PingPong.Client as Client
import           Network.TypedProtocol.PingPong.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => PingPongClient m a
       -> PingPongServer m b
       -> m (a, b)

direct (Client.SendMsgDone clientResult) PingPongServer{recvMsgDone} =
    pure (clientResult, recvMsgDone)

direct (Client.SendMsgPing kPong) PingPongServer{recvMsgPing} = do
    server' <- recvMsgPing
    client' <- kPong
    direct client' server'
