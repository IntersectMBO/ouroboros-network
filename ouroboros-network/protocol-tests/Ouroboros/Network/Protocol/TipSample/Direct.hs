{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Ouroboros.Network.Protocol.TipSample.Direct (direct) where

import           Ouroboros.Network.Protocol.TipSample.Client
import           Ouroboros.Network.Protocol.TipSample.Server


-- | The 'TipSampleClient' and 'TipSampleServer' are dual to each other in the
-- sense that they can be paired together using 'direct'.
--
direct :: forall tip m a b. Monad m
       => TipSampleClient tip m a
       -> TipSampleServer tip m b
       -> m (a, b)
direct (TipSampleClient mclient) (TipSampleServer mserver) = do
    client <- mclient
    server <- mserver
    directStIdle client server


directStIdle :: forall tip m a b. Monad m
             => ClientStIdle tip m a
             -> ServerStIdle tip m b
             -> m (a, b)

directStIdle (SendMsgFollowTip n slotNo handleTips) ServerStIdle { handleFollowTip } =
    handleFollowTip n slotNo >>= go handleTips
  where
    go :: HandleTips n tip m a
       -> SendTips n tip m b
       -> m (a, b)
    go (ReceiveTip k) (SendNextTip _ tip mSendTips) = do
      receiveTip <- k tip
      sendTips <- mSendTips
      go receiveTip sendTips
    go (ReceiveLastTip k) (SendLastTip _ tip serverIdle) = do
      clientIdle <- k tip
      directStIdle clientIdle serverIdle

directStIdle (SendMsgDone a)
             ServerStIdle { handleDone } =
    (a,) <$> handleDone
