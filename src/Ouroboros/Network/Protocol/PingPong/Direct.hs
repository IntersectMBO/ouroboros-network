{-# LANGUAGE NamedFieldPuns #-}
module Ouroboros.Network.Protocol.PingPong.Direct where

import Ouroboros.Network.Protocol.PingPong.Client as Client
import Ouroboros.Network.Protocol.PingPong.Server as Server

-- | The 'ClientStream m' and 'ServerStream m' types are complementary. The
-- former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => ServerStream m a
       -> ClientStream m b
       -> m (a, b)

direct ServerStream{handleDone} (Client.Stop clientResult) =
    pure (handleDone, clientResult)

direct ServerStream{handlePing} (Client.Ping kPong) = do
    server' <- handlePing
    client' <- kPong
    direct server' client'

