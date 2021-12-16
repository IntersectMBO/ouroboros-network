{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.TypedProtocol.PingPong.Examples where

import           Network.TypedProtocol.PingPong.Client
import           Network.TypedProtocol.PingPong.Server

import           Network.TypedProtocol.Pipelined


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


--
-- Pipelined examples
--

-- | A pipelined ping-pong client that sends eagerly rather than waiting to
-- collect any replies. This is maximum pipelining in some sense, and
-- correspondingly it gives minimum choice to the environment (drivers).
--
-- It returns the interleaving of ping indexes sent, and collected.
--
pingPongClientPipelinedMax
  :: forall m. Monad m
  => Int
  -> PingPongClientPipelined m [Either Int Int]
pingPongClientPipelinedMax c =
    PingPongClientPipelined (go [] Zero 0)
  where
    go :: [Either Int Int] -> Nat o -> Int
       -> PingPongSender o Int m [Either Int Int]
    go acc o        n | n < c
                      = SendMsgPingPipelined
                          (return n)
                          (go (Left n : acc) (Succ o) (succ n))
    go acc Zero     _ = SendMsgDonePipelined (reverse acc)
    go acc (Succ o) n = CollectPipelined
                          Nothing
                          (\n' -> go (Right n' : acc) o n)


-- | A pipelined ping-pong client that sends eagerly but always tries to
-- collect any replies if they are available.  This allows pipelining but
-- keeps it to a minimum, and correspondingly it gives maximum choice to the
-- environment (drivers).
--
-- It returns the interleaving of ping indexes sent, and collected.
--
pingPongClientPipelinedMin
  :: forall m. Monad m
  => Int
  -> PingPongClientPipelined m [Either Int Int]
pingPongClientPipelinedMin c =
    PingPongClientPipelined (go [] Zero 0)
  where
    go :: [Either Int Int] -> Nat o -> Int
       -> PingPongSender o Int m [Either Int Int]
    go acc (Succ o) n = CollectPipelined
                          (if n < c then Just (ping acc (Succ o) n)
                                    else Nothing)
                          (\n' -> go (Right n' : acc) o n)
    go acc Zero     n | n < c
                      = ping acc Zero n
    go acc Zero     _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int] -> Nat o -> Int
         -> PingPongSender o Int m [Either Int Int]
    ping acc o      n = SendMsgPingPipelined
                          (return n)
                          (go (Left n : acc) (Succ o) (succ n))


-- | A pipelined ping-pong client that sends eagerly up to some maximum limit
-- of outstanding requests. It is also always ready to collect any replies if
-- they are available.  This allows limited pipelining and correspondingly
-- limited choice to the environment (drivers).
--
-- It returns the interleaving of ping indexes sent, and collected.
--
pingPongClientPipelinedLimited
  :: forall m. Monad m
  => Int -> Int
  -> PingPongClientPipelined m [Either Int Int]
pingPongClientPipelinedLimited omax c =
    PingPongClientPipelined (go [] Zero 0)
  where
    go :: [Either Int Int] -> Nat o -> Int
       -> PingPongSender o Int m [Either Int Int]
    go acc (Succ o) n = CollectPipelined
                          (if n < c && int (Succ o) < omax
                             then Just (ping acc (Succ o) n)
                             else Nothing)
                          (\n' -> go (Right n' : acc) o n)
    go acc Zero     n | n < c
                      = ping acc Zero n
    go acc Zero     _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int] -> Nat o -> Int
         -> PingPongSender o Int m [Either Int Int]
    ping acc o      n = SendMsgPingPipelined
                          (return n)
                          (go (Left n : acc) (Succ o) (succ n))

    -- this isn't supposed to be efficient, it's just for the example
    int :: Nat n -> Int
    int Zero     = 0
    int (Succ n) = succ (int n)
