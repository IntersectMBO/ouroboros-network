{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Network.TypedProtocol.PingPong.Tests where

import           Control.Monad.Class.MonadFork (MonadFork (..))
import           Control.Monad.Class.MonadST (MonadST (..))
import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import           Control.Monad.Class.MonadProbe
                   ( MonadProbe (..)
                   , MonadRunProbe (..)
                   , withProbe
                   )
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Class.MonadTimer (MonadTimer (..))
import           Control.Monad.ST.Lazy (runST)
import           Control.Monad.IOSim (SimM)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import           Data.Functor.Identity (Identity (..))
import           Data.List (foldl')


import           Network.TypedProtocol.Codec (transformCodec)
import           Network.TypedProtocol.Channel
                   ( Channel
                   , createConnectedChannels
                   , createPipeConnectedChannels
                   )
import           Network.TypedProtocol.Driver (runPeer)
import           Network.TypedProtocol.Proofs

import           Network.TypedProtocol.PingPong.Type
import           Network.TypedProtocol.PingPong.Client
                   ( PingPongClient(..)
                   , pingPongClientPeer
                   , pingPongClientPeerSender
                   )
import           Network.TypedProtocol.PingPong.Codec
                   ( codecPingPong
                   )
import           Network.TypedProtocol.PingPong.Server
                   ( PingPongServer(..)
                   , pingPongServerPeer
                   )
import           Network.TypedProtocol.PingPong.Examples
                   ( pingPongClientCount
                   , pingPongSenderCount
                   , pingPongServerCount
                   )

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck (testProperty)


-- | The 'PingPongClient m' and 'PingPongServer m' types are complementary.
-- The former can be used to feed the latter directly, in the same thread.
-- That's demonstrated here by constructing 'direct'.
--
direct :: Monad m
       => PingPongClient m a
       -> PingPongServer m b
       -> m (a, b)

direct (SendMsgDone clientResult) PingPongServer{recvMsgDone} =
    pure (clientResult, recvMsgDone)

direct (SendMsgPing kPong) PingPongServer{recvMsgPing} = do
    server' <- recvMsgPing
    client' <- kPong
    direct client' server'


-- | Run a simple ping\/pong client and server, without going via the 'Peer'
-- representation at all.
--
prop_direct :: NonNegative Int -> Bool
prop_direct (NonNegative n) =
    case runIdentity
           (direct (pingPongClientCount n)
                    pingPongServerCount)

      of ((), n') -> n' == n


-- | Run a simple ping\/pong client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: NonNegative Int -> Bool
prop_connect (NonNegative n) =
  case runIdentity
         (connect (pingPongClientPeer (pingPongClientCount n))
                  (pingPongServerPeer  pingPongServerCount))

    of ((), n', TerminalStates TokDone TokDone) -> n == n'


tests :: TestTree
tests = testGroup "Network.TypedProtocol.PingPong"
  [ testProperty "direct" prop_direct
  , testProperty "connect" prop_connect
  , testProperty "connect_piplined ST" prop_connect_pipelined_ST
  , testProperty "connect_piplined IO" prop_connect_pipelined_IO
  , testProperty "channel ST" prop_channel_ST
  , testProperty "channel IO" prop_channel_IO
  , testProperty "pipe" prop_pipe
  ]

runExperiment
  :: forall m n.
     ( MonadSTM m
     , MonadTimer m
     , MonadProbe m
     , MonadRunProbe m n
     )
  => (Probe m Property -> m ())
  -> n Property
runExperiment exp_ = isValid <$> withProbe exp_
 where
  isValid = foldl' (\acu (_,p) -> acu .&&. p) (property True)

connect_pipelined_experiment
  :: ( MonadSTM m
     , MonadProbe m
     )
  => Positive Int
  -> Probe m Property
  -> m ()
connect_pipelined_experiment (Positive x) probe = do
  var <- atomically $ newTVar 0
  let c = fromIntegral x
      client = pingPongSenderCount var c
  (_, b, _) <- connectPipelined (pingPongClientPeerSender client) (pingPongServerPeer pingPongServerCount)
  res <- atomically $ readTVar var
  probeOutput probe (c === b)
  probeOutput probe (c === res)

prop_connect_pipelined_ST
  :: Positive Int
  -> Property
prop_connect_pipelined_ST p = runST $ runExperiment @(SimM _)
  (connect_pipelined_experiment p)

prop_connect_pipelined_IO
  :: Positive Int
  -> Property
prop_connect_pipelined_IO p = ioProperty $ runExperiment
  (connect_pipelined_experiment p)

channel_experiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     , MonadFail m
     )
  => Channel m ByteString
  -> Channel m ByteString
  -> Positive Int
  -> Probe m Property
  -> m ()
channel_experiment clientChannel serverChannel (Positive x) probe = do
  serverVar <- newEmptyTMVarIO
  let c = fromIntegral x
      clientPeer = pingPongClientPeer $ pingPongClientCount c
      serverPeer = pingPongServerPeer pingPongServerCount
      codec      = transformCodec BSC.pack BSC.unpack codecPingPong

  fork $ do
    Right res <- runPeer codec serverChannel serverPeer
    atomically $ putTMVar serverVar res
  fork $ runPeer codec clientChannel clientPeer >> return ()

  res <- atomically $ takeTMVar serverVar
  probeOutput probe (res === c)

prop_channel_ST
  :: Positive Int
  -> Property
prop_channel_ST p =
  runST $ runExperiment $ \probe -> do
    (clientChannel, serverChannel) <- createConnectedChannels
    channel_experiment clientChannel serverChannel p probe

prop_channel_IO
  :: Positive Int
  -> Property
prop_channel_IO p =
  ioProperty $ runExperiment $ \probe -> do
    (clientChannel, serverChannel) <- createConnectedChannels
    channel_experiment clientChannel serverChannel p probe

prop_pipe
  :: Positive Int
  -> Property
prop_pipe p =
  ioProperty $ runExperiment $ \probe -> do
    (clientChannel, serverChannel) <- createPipeConnectedChannels
    channel_experiment clientChannel serverChannel p probe
