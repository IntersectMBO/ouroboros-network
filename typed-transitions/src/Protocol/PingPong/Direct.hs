{-# LANGUAGE NamedFieldPuns #-}
module Protocol.Chain.Direct where

import Protocol.PingPong.Client as Client
import Protocol.PingPong.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => PingPongServer m a
       -> PingPongClient m b
       -> m (a, b)

direct PingPongServer{recvMsgDone} (Client.SendMsgStop clientResult) =
    pure (recvMsgDone, clientResult)

direct PingPongServer{recvMsgPing} (Client.SendMsgPing kPong) = do
    server' <- recvMsgPing
    client' <- kPong
    direct server' client'

