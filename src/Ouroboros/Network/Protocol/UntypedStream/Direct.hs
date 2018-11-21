{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.UntypedStream.Direct where

import Ouroboros.Network.Protocol.UntypedStream.Client
import Ouroboros.Network.Protocol.UntypedStream.Server

-- Note: `direct` does not handle `window` updates only because we don't have
-- the `MsgRenewWindow :: StreamMessage rng 'StBusy 'StFull` transition.
direct
  :: forall m rng a x y. Monad m
  => UntypedStreamServer m rng a x
  -> UntypedStreamClient m rng a y
  -> m (x, y)
direct (UntypedStreamServer{recvMsgRequest}) (SendMsgRequest rng _window _threshold client) = do
  server <- recvMsgRequest rng
  go server client
 where
  go :: ServerHandleData m a x -> ClientHandleData m a y -> m (x, y)
  go (SendMsgData a mserver) (ClientHandleData{recvMsgData}) = do
    client' <- recvMsgData a
    server  <- mserver
    go server client'
  go (SendMsgStreamEnd x) (ClientHandleData{recvMsgStreamEnd}) = do
    y <- recvMsgStreamEnd
    return (x, y)
