{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Network.TypedProtocol.PingPong where

import           Control.Monad.Class.MonadFork (MonadFork (..))
import           Control.Monad.Class.MonadST (MonadST (..))
import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import           Control.Monad.Class.MonadProbe
  ( MonadProbe (..)
  , MonadRunProbe (..)
  , withProbe
  )
import           Control.Monad.Class.MonadTimer (MonadTimer (..))
import           Control.Monad.ST.Lazy (runST)
import           Control.Monad.Free (Free)
import           Control.Monad.IOSim (SimF)

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
import           Network.TypedProtocol.Driver (connect, runPeer)
import qualified Network.TypedProtocol.Pipelined as Pipelined

import           Network.TypedProtocol.PingPong.Client
  ( pingPongClientCount
  , pingPongClientPeer
  , pingPongSenderCount
  , pingPongClientPeerSender
  )
import           Network.TypedProtocol.PingPong.Codec (pingPongCodec)
import           Network.TypedProtocol.PingPong.Server
  ( pingPongServerCount
  , pingPongServerPeer
  )
import           Network.TypedProtocol.PingPong.Direct (direct)

import           Test.Tasty
import           Test.QuickCheck
import           Test.Tasty.QuickCheck (testProperty)

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

prop_direct
  :: Positive Int
  -> Property
prop_direct (Positive x) =
  let c = fromIntegral x
  in case runIdentity $ direct (pingPongClientCount c) pingPongServerCount of
    (_, c') -> c === c'

prop_connect
  :: Positive Int
  -> Property
prop_connect (Positive x) =
  let c = fromIntegral x
  in case runIdentity $ connect (pingPongClientPeer $ pingPongClientCount c) (pingPongServerPeer pingPongServerCount) of
    (_, c') -> c === c'

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
  (_, b) <- Pipelined.connect (pingPongClientPeerSender client) (pingPongServerPeer pingPongServerCount)
  res <- atomically $ readTVar var
  probeOutput probe (c === b)
  probeOutput probe (c === res)

prop_connect_pipelined_ST
  :: Positive Int
  -> Property
prop_connect_pipelined_ST p = runST $ runExperiment @(Free (SimF _))
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
      codec = transformCodec BSC.pack BSC.unpack pingPongCodec

  fork $ do
    res <- runPeer codec serverChannel serverPeer
    atomically $ putTMVar serverVar res
  fork $ runPeer codec clientChannel clientPeer

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
