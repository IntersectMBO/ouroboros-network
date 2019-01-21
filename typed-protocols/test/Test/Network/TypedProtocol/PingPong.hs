{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Network.TypedProtocol.PingPong where

import           Control.Monad.Class.MonadSTM (MonadSTM (..))
import           Control.Monad.Class.MonadProbe (MonadProbe (..), MonadRunProbe (..), withProbe)
import           Control.Monad.Class.MonadTimer (MonadTimer (..))
import           Control.Monad.ST.Lazy (runST)
import           Control.Monad.Free (Free)
import           Control.Monad.IOSim (SimF)

import           Data.Functor.Identity (Identity (..))
import           Data.List (foldl')


import           Network.TypedProtocol.Core (connect)
import qualified Network.TypedProtocol.Pipelined as Pipelined

import           Network.TypedProtocol.PingPong.Client
  ( pingPongClientFixed
  , pingPongClientPeer
  , pingPongSenderCount
  , pingPongClientPeerSender
  )
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
  in case runIdentity $ direct (pingPongClientFixed c) pingPongServerCount of
    (_, c') -> c === c'

prop_connect
  :: Positive Int
  -> Property
prop_connect (Positive x) =
  let c = fromIntegral x
  in case runIdentity $ connect (pingPongClientPeer $ pingPongClientFixed c) (pingPongServerPeer pingPongServerCount) of
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
