{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}

module Ouroboros.Network.Protocol.KeepAlive.Direct where

import Ouroboros.Network.Protocol.KeepAlive.Client
import Ouroboros.Network.Protocol.KeepAlive.Server


direct :: forall a b m. Monad m
       => KeepAliveServer m a
       -> KeepAliveClient m b
       -> m (a, b)
direct srv (KeepAliveClient clientM) = do
  go srv =<< clientM
 where
   go :: Monad m
      => KeepAliveServer m a
      -> KeepAliveClientSt m b
      -> m (a, b)
   go KeepAliveServer { recvMsgDone }
          (SendMsgDone mdone) =
       (,) <$> recvMsgDone <*> mdone
   go KeepAliveServer { recvMsgKeepAlive }
          (SendMsgKeepAlive _cookie mclient) = do
       server <- recvMsgKeepAlive
       client <- mclient
       go server client
