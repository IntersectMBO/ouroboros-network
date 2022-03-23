{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad (replicateM)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer (Tracer (..), nullTracer)

import           Criterion
import           Criterion.Main

import           Control.Exception (AsyncException (..))
import           Data.Foldable (traverse_)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver.Simple

import           Network.TypedProtocol.PingPong.Client
import           Network.TypedProtocol.PingPong.Codec
-- import qualified Network.TypedProtocol.PingPong.Codec.CBOR as CBOR
import           Network.TypedProtocol.PingPong.Examples
import           Network.TypedProtocol.PingPong.Server
import           Network.TypedProtocol.PingPong.Type


prop_channel :: forall m. (MonadSTM m, MonadAsync m, MonadCatch m, MonadTimer m)
             => Maybe (DiffTime, DiffTime)
             -> Int
             -> Tracer m (Role, TraceSendRecv PingPong)
             -> m Bool
prop_channel delay n tr = do
    ((), n') <- runConnectedPeers createChannel
                                  tr
                                  codecPingPongId client server
    return (n' == n)
  where
    createChannel :: forall a. m (Channel m a, Channel m a)
    createChannel =
      case delay of
        Nothing       -> createConnectedChannels
        Just (d1, d2) -> (\(a, b) -> (delayChannel d1 a, delayChannel d2 b))
                     <$> createConnectedChannels

    client = pingPongClientPeer (pingPongClientCount n)
    server = pingPongServerPeer  pingPongServerCount

--
-- timers, delays, timeouts
--

prop_threadDelay :: forall m. MonadDelay m => m ()
prop_threadDelay = threadDelay 1

prop_registerDelay :: forall m. MonadTimer m => m ()
prop_registerDelay = registerDelay 1 >>= \v -> atomically (readTVar v >>= check)

prop_timeout_fail :: forall m. MonadTimer m => m (Maybe ())
prop_timeout_fail = timeout 1 (threadDelay 2)

prop_timeout_succeed :: forall m. MonadTimer m => m (Maybe ())
prop_timeout_succeed = timeout 2 (threadDelay 1)


--
-- threads, async
--

prop_threads :: forall m. (MonadFork m, MonadDelay m, MonadSay m) => Int -> m ()
prop_threads n = do
    threads <- replicateM n (forkIO $ threadDelay 2
                                   >> say ""
                            )
    threadDelay 1
    traverse_ (\tid -> throwTo tid ThreadKilled) threads


prop_async :: forall m. (MonadAsync m, MonadDelay m, MonadSay m) => Int -> m ()
prop_async n = do
    threads <- replicateM n (async $ threadDelay 1
                                  >> say ""
                            )
    traverse_ wait threads


main :: IO ()
main = defaultMain
    [ env (let !n  = 10000
               !d1 = 1
               !d2 = 2
           in pure (n, d1, d2))
           $ \ ~(n, d1, d2) ->
      bgroup "ping-pong"
      [ bench "stm channel without delay" $
        whnf id (runSimOrThrow (prop_channel Nothing n nullTracer))
      , bench "stm channel with delay" $
        whnf id (runSimOrThrow (prop_channel (Just (d1, d2)) n nullTracer))
      , bench "events" $
        nf id ( selectTraceEventsSay
              $ runSimTrace
              $ prop_channel Nothing n (Tracer $ say . show))
      ]
    , env (pure ()) $ \_ ->
      bgroup "delays"
      [ bench "threadDelay" $
        whnf id (runSimOrThrow prop_threadDelay)
      , bench "registerDelay" $
        whnf id (runSimOrThrow prop_registerDelay)
      , bgroup "timeout"
        [ bench "fail" $
          whnf id (runSimOrThrow prop_timeout_fail)
        , bench "succeed" $
          whnf id (runSimOrThrow prop_timeout_succeed)
        ]
      ]
    ,
      bgroup "threads"
      [ env (pure 50) $ \n ->
        bgroup "50"
        [ bench "async silent" $
          whnf id (runSimOrThrow (prop_async n))
        , bench "forkIO silent" $
          whnf id (runSimOrThrow (prop_threads n))
        , bench "async say" $
          nf id ( selectTraceEventsSay
                $ runSimTrace
                $ prop_async n)
        , bench "forkIO say" $
          nf id ( selectTraceEventsSay
                $ runSimTrace
                $ prop_threads n)
        ]
      , env (pure 250) $ \n ->
        bgroup "250"
        [ bench "async" $
          whnf id (runSimOrThrow (prop_async n))
        , bench "forkIO" $
          whnf id (runSimOrThrow (prop_threads n))
        ]
      ]
    ]
