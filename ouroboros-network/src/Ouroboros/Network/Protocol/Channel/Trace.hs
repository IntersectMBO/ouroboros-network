module Ouroboros.Network.Protocol.Channel.Trace where

import Cardano.BM.BaseTrace

import Protocol.Channel

-- | Trace every sent message before sending it, and every receiving message
-- (or Nothing for end of stream) after receiving it.
traceChannel
  :: ( Applicative n, Monad m )
  => BaseTrace n send
  -> BaseTrace m (Maybe recv)
  -> Duplex n m send recv
  -> Duplex n m send recv
traceChannel trSend trRecv = channelEffect (traceWith trSend) (traceWith trRecv)

traceSend
  :: ( Applicative m, Monad n )
  => BaseTrace m send
  -> Duplex m n send recv
  -> Duplex m n send recv
traceSend tr = channelSendEffect (traceWith tr)

traceRecv
  :: ( Monad m, Applicative n )
  => BaseTrace m (Maybe recv)
  -> Duplex n m send recv
  -> Duplex n m send recv
traceRecv tr = channelRecvEffect (traceWith tr)
