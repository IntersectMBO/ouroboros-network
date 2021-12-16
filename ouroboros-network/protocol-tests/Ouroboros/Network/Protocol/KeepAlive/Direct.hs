{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.Protocol.KeepAlive.Direct where

import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server

direct :: Monad m
       => KeepAliveServer m a
       -> KeepAliveClient m b
       -> m (a, b)
direct KeepAliveServer { recvMsgDone }
       (SendMsgDone mdone) =
    (,) <$> recvMsgDone <*> mdone
direct KeepAliveServer { recvMsgKeepAlive }
       (SendMsgKeepAlive _cookie mclient) = do
    server <- recvMsgKeepAlive
    client <- mclient
    direct server client
