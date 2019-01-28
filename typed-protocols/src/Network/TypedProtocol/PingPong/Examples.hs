{-# LANGUAGE BangPatterns        #-}

module Network.TypedProtocol.PingPong.Examples where

import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Client

import           Control.Monad.Class.MonadSTM


-- | The standard stateless ping-pong server instance.
--
pingPongServerStandard
  :: Applicative m
  => PingPongServer m ()
pingPongServerStandard =
    PingPongServer {
      recvMsgPing = pure pingPongServerStandard,
      recvMsgDone = ()
    }


-- | An example ping-pong server instance that counts the number of ping
-- messages.
--
pingPongServerCount
  :: Applicative m
  => PingPongServer m Int
pingPongServerCount = go 0
  where
   go !c = PingPongServer {
      recvMsgPing = pure $ go (succ c),
      recvMsgDone = c
    }


-- | An example ping-pong client that sends pings as fast as possible forever‽
--
-- This may not be a good idea‼
--
pingPongClientFlood :: Applicative m => PingPongClient m a
pingPongClientFlood = SendMsgPing (pure pingPongClientFlood)


-- | An example ping-pong client that sends a fixed number of ping messages
-- and then stops.
--
pingPongClientCount :: Applicative m => Int -> PingPongClient m ()
pingPongClientCount 0 = SendMsgDone ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n-1)))


pingPongSenderCount
  :: MonadSTM m
  => TVar m Int
  -> Int
  -> PingPongSender m ()
pingPongSenderCount var = go
 where
  go 0 = SendMsgDonePipelined ()
  go n = SendMsgPingPipelined (atomically $ modifyTVar var succ)
                              (go (pred n))

