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

data SingQueuePP (q :: Queue PingPong) where
    SingEmptyPP :: SingQueuePP Empty
    SingConsPP  :: Int
                -> SingQueuePP q
                -> SingQueuePP (Tr StBusy StIdle <| q)

snocPP :: Int
       -> SingQueuePP q
       -> SingQueuePP (q |> Tr StBusy StIdle)
snocPP n  SingEmptyPP      = SingConsPP n SingEmptyPP
snocPP n (SingConsPP n' q) = SingConsPP n' (n `snocPP` q)

lengthPP :: SingQueuePP q -> Int
lengthPP SingEmptyPP      = 0
lengthPP (SingConsPP _ q) = 1 + lengthPP q

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
    PingPongClientPipelined (go [] SingEmptyPP 0)
  where
    go :: forall (q :: Queue PingPong).
          [Either Int Int]
       -> SingQueuePP q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q                 n | n < c
                               = sendMsgPing acc q n
    go acc SingEmptyPP       _ = SendMsgDonePipelined (reverse acc)
    go acc (SingConsPP n' q) n = CollectPipelined
                                   Nothing
                                   (pure $ go (Right n' : acc) q n)

    sendMsgPing :: forall (q :: Queue PingPong).
                   [Either Int Int]
                -> SingQueuePP q
                -> Int
                -> PingPongClientIdle q m [Either Int Int]
    sendMsgPing acc q n =
      SendMsgPingPipelined
        (go (Left n : acc)
            (n `snocPP` q)
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
    PingPongClientPipelined (go [] SingEmptyPP 0)
  where
    go :: forall (q :: Queue PingPong).
          [Either Int Int]
       -> SingQueuePP q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q@(SingConsPP n' q') n = CollectPipelined
                                      (if n < c then Just (ping acc q n)
                                                else Nothing)
                                      (pure $ go (Right n' : acc) q' n)
    go acc SingEmptyPP          n | n < c
                                  = ping acc SingEmptyPP n
    go acc SingEmptyPP          _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int]
         -> SingQueuePP q
         -> Int
         -> PingPongClientIdle q m [Either Int Int]
    ping acc q n = SendMsgPingPipelined
                     (go (Left n : acc) (n `snocPP` q) (succ n))


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
    PingPongClientPipelined (go [] SingEmptyPP 0)
  where
    go :: forall (q :: Queue PingPong).
         [Either Int Int]
       -> SingQueuePP q
       -> Int
       -> PingPongClientIdle q m [Either Int Int]
    go acc q@(SingConsPP n' q') n = CollectPipelined
                                      (if n < c && lengthPP q < omax
                                         then Just (ping acc q n)
                                         else Nothing)
                                      (pure $ go (Right n' : acc) q' n)
    go acc SingEmptyPP n | n < c
                         = ping acc SingEmptyPP n
    go acc SingEmptyPP _ = SendMsgDonePipelined (reverse acc)

    ping :: [Either Int Int] -> SingQueuePP q -> Int
         -> PingPongClientIdle q m [Either Int Int]
    ping acc q      n = SendMsgPingPipelined
                          (go (Left n : acc) (n `snocPP` q) (succ n))
