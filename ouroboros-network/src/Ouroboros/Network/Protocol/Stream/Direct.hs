{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.Protocol.Stream.Direct where

import Ouroboros.Network.Protocol.Stream.Client
import Ouroboros.Network.Protocol.Stream.Server

-- | Run @'StreamServer'@ directly against @'StreamClient'@.
--
direct
  :: forall m rng a x y. Monad m
  => StreamServer m rng a x
  -> StreamClient m rng a y
  -> m (x, y)
direct (StreamServer{recvMsgRequest}) (SendMsgRequest rng client) = do
  server <- recvMsgRequest rng
  go server client
 where
  go :: ServerHandleData m a x -> ClientHandleData m a y -> m (x, y)
  go (SendMsgData a mserver) ClientHandleData{recvMsgData} = do
    server <- mserver
    client' <- recvMsgData a
    go server client'
  go (SendMsgStreamEnd x) ClientHandleData{recvMsgStreamEnd} =
    (x,) <$> recvMsgStreamEnd
  go (SendMsgNoData x) ClientHandleData{recvMsgNoData} =
    (x,) <$> recvMsgNoData
