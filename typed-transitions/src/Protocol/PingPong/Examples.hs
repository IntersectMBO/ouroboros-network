{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Protocol.PingPong.Examples where

import           Control.Concurrent.Async
import           Data.Text (unpack)

import           Protocol.Channel (mvarChannels)
import           Protocol.Driver
import           Protocol.PingPong.Server
import           Protocol.PingPong.Client
import           Protocol.PingPong.Codec

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

-- | Client and server run concurrently, communcating via an MVar channel.
-- The input is how many pings to send, the output is how many the server has
-- received when it finishes. We'll find they're always equal.
demoCodec :: Int -> IO Int
demoCodec n = do
  (clientChannel, serverChannel) <- mvarChannels
  let clientPeer = pingPongClientPeer (pingPongClientCount n)
      serverPeer = pingPongServerPeer (pingPongServerCounting 0)
      -- Here we eliminate the 'Result' from the server and client by
      -- throwing an error, so that if one dies, the other doesn't starve,
      -- but also dies with unexpected end of input.
      throwOnUnexpected (Normal t) = pure t
      throwOnUnexpected (Unexpected text) = error (unpack text)
      client = throwOnUnexpected =<< useCodecWithDuplex clientChannel pingPongCodec clientPeer
      server = throwOnUnexpected =<< useCodecWithDuplex serverChannel pingPongCodec serverPeer
  ((), m) <- concurrently client server
  pure m
