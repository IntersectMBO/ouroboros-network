{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PingPongExamples where

import           Ouroboros.Network.Protocol.PingPong.Server
import           Ouroboros.Network.Protocol.PingPong.Client


-- | The standard stateless ping-pong server instance.
--
pingPongServerStandard :: Applicative m => PingPongServer m ()
pingPongServerStandard =
    PingPongServer {
      recvMsgPing = pure pingPongServerStandard,
      recvMsgDone = ()
    }

-- | An example ping-pong server instance that counts the number of ping
-- messages.
--
pingPongServerCounting :: Applicative m => Int -> PingPongServer m Int
pingPongServerCounting !n =
    PingPongServer {
      recvMsgPing = pure (pingPongServerCounting (n+1)),
      recvMsgDone = n
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
pingPongClientCount 0 = SendMsgStop ()
pingPongClientCount n = SendMsgPing (pure (pingPongClientCount (n-1)))

