{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Network.TypedProtocol.PingPong.Examples where

import           Network.TypedProtocol.PingPong.Client
import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Type

import           Network.TypedProtocol.Core


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


data F st st' where
    F :: Int
      -> F StBusy StIdle


lengthQ :: SingQueueF F q -> Int
lengthQ SingEmptyF      = 0
lengthQ (SingConsF _ q) = 1 + lengthQ q

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
    PingPongClientPipelined (go [] SingEmptyF 0)
  where
    go :: forall (q :: Queue PingPong).
          [Either Int Int]
       -> SingQueueF F q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q                    n | n < c
                                  = sendMsgPing acc q n
    go acc SingEmptyF           _ = SendMsgDonePipelined (reverse acc)
    go acc (SingConsF (F n') q) n = CollectPipelined
                                      Nothing
                                     (pure $ go (Right n' : acc) q n)

    sendMsgPing :: forall (q :: Queue PingPong).
                   [Either Int Int]
                -> SingQueueF F q
                -> Int
                -> PingPongClientIdle q m [Either Int Int]
    sendMsgPing acc q n =
      SendMsgPingPipelined
        (go (Left n : acc)
            (q |> F n)
            (succ n))


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
    PingPongClientPipelined (go [] SingEmptyF 0)
  where
    go :: forall (q :: Queue PingPong).
          [Either Int Int]
       -> SingQueueF F q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q@(SingConsF (F n') q') n = CollectPipelined
                                         (if n < c then Just (ping acc q n)
                                                   else Nothing)
                                         (pure $ go (Right n' : acc) q' n)
    go acc SingEmptyF              n | n < c
                                     = ping acc SingEmptyF n
    go acc SingEmptyF              _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int]
         -> SingQueueF F q
         -> Int
         -> PingPongClientIdle q m [Either Int Int]
    ping acc q n = SendMsgPingPipelined
                     (go (Left n : acc) (q |> F n) (succ n))


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
    PingPongClientPipelined (go [] SingEmptyF 0)
  where
    go :: forall (q :: Queue PingPong).
         [Either Int Int]
       -> SingQueueF F q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q@(SingConsF (F n') q') n = CollectPipelined
                                         (if n < c && lengthQ q < omax
                                            then Just (ping acc q n)
                                            else Nothing)
                                         (pure $ go (Right n' : acc) q' n)
    go acc SingEmptyF              n | n < c
                                     = ping acc SingEmptyF n
    go acc SingEmptyF              _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int] -> SingQueueF F q -> Int
         -> PingPongClientIdle q m [Either Int Int]
    ping acc q      n = SendMsgPingPipelined
                          (go (Left n : acc) (q |> F n) (succ n))
