{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.KeepAlive (tests) where

import           Control.Monad (void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Typeable (Typeable)
import           System.Random


import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.Channel
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Server

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "KeepAlive"
    [ testProperty "KeepAlive Convergence" prop_keepAlive_convergence]

runKeepAliveClient
    :: forall m peer header block.
        ( MonadAsync m
        , MonadFork m
        , MonadMask m
        , MonadMonotonicTime m
        , MonadST m
        , MonadSTM m
        , MonadTimer m
        , MonadThrow (STM m)
        , Ord peer)
    => Tracer m (TraceKeepAliveClient peer)
    -> StdGen
    -> ControlMessageSTM m
    -> FetchClientRegistry peer header block m
    -> peer
    -> Channel m BL.ByteString
    -> KeepAliveInterval
    -> m ((), Maybe BL.ByteString)
runKeepAliveClient tracer rng controlMessageSTM registry peer channel keepAliveInterval =
    let kacApp dqCtx = runPeerWithLimits
                         nullTracer
                         codecKeepAlive
                         (byteLimitsKeepAlive (fromIntegral . BL.length))
                         timeLimitsKeepAlive
                         channel
                         $ keepAliveClientPeer
                         $ keepAliveClient tracer rng controlMessageSTM peer dqCtx keepAliveInterval in
    bracketKeepAliveClient registry peer kacApp

runKeepAliveServer
    :: forall m.
        ( MonadAsync m
        , MonadFork m
        , MonadMask m
        , MonadMonotonicTime m
        , MonadST m
        , MonadSTM m
        , MonadTimer m
        , MonadThrow (STM m)
        )
    => Channel m BL.ByteString
    -> m ((), Maybe BL.ByteString)
runKeepAliveServer channel =
    runPeerWithLimits
        nullTracer
        codecKeepAlive
        (byteLimitsKeepAlive (fromIntegral . BL.length))
        timeLimitsKeepAlive
        channel
        $ keepAliveServerPeer
        $ keepAliveServer

runKeepAliveClientAndServer
    :: forall m peer header block.
        ( MonadAsync m
        , MonadFork m
        , MonadMask m
        , MonadMonotonicTime m
        , MonadSay m
        , MonadST m
        , MonadSTM m
        , MonadTimer m
        , MonadThrow (STM m)
        , Ord peer
        , Show peer)
    => NetworkDelay
    -> Int
    -> Tracer m (TraceKeepAliveClient peer)
    -> ControlMessageSTM m
    -> FetchClientRegistry peer header block m
    -> peer
    -> KeepAliveInterval
    -> m (Async m ((), Maybe BL.ByteString), Async m ((), Maybe BL.ByteString))
runKeepAliveClientAndServer (NetworkDelay nd) seed tracer controlMessageSTM registry peer keepAliveInterval = do
    (clientChannel, serverChannel) <- createConnectedChannels

    clientAsync <- async $ runKeepAliveClient tracer (mkStdGen seed) controlMessageSTM registry peer
                               (delayChannel nd clientChannel) keepAliveInterval
    serverAsync <- async $ runKeepAliveServer serverChannel
    return (clientAsync, serverAsync)

newtype NetworkDelay = NetworkDelay {
      unNetworkDelay :: DiffTime
    } deriving Show

instance Arbitrary NetworkDelay where
    arbitrary = do
        m <- choose (1, 1000 :: Int) -- A delay between 1 and 1000 ms
        return $ NetworkDelay $ (fromIntegral m) / 1000

prop_keepAlive_convergenceM
    :: forall m.
        ( Eq (Async m ())
        , MonadAsync m
        , MonadFork m
        , MonadMask m
        , MonadMonotonicTime m
        , MonadSay m
        , MonadST m
        , MonadSTM m
        , MonadTimer m
        , MonadThrow (STM m)
        )
    => Tracer m (TraceKeepAliveClient String)
    -> NetworkDelay
    -> Int
    -> m ()
prop_keepAlive_convergenceM tracer (NetworkDelay nd) seed = do
    registry <- newFetchClientRegistry
    controlMessageV <- newTVarIO Continue
    let controlMessageSTM = readTVar controlMessageV
        clientId = "client"
        timeConstant = 1000 -- Same as in PeerGSV's <> definition
        keepAliveInterval = 10

    (c_aid, s_aid) <- runKeepAliveClientAndServer (NetworkDelay nd) seed tracer controlMessageSTM
                          registry clientId (KeepAliveInterval keepAliveInterval)
    threadDelay $ timeConstant * keepAliveInterval

    atomically $ writeTVar controlMessageV Terminate
    void $ wait c_aid
    void $ wait s_aid

    -- XXX Must be larger than the KeepAliveInterval timeout or we leak threads in the SIM
    -- Can be removed after #2631 is merged.
    threadDelay (keepAliveInterval + 128)

-- Test that our estimate of PeerGSV's G terms converge to
-- a given constant delay.
prop_keepAlive_convergence :: NetworkDelay -> Int -> Property
prop_keepAlive_convergence nd seed =
    let trace = selectTraceEventsDynamic $ runSimTrace $ prop_keepAlive_convergenceM dynamicTracer nd seed in
    verifyConvergence trace
  where
    verifyConvergence :: [TraceKeepAliveClient String] -> Property
    verifyConvergence [] = property False
    verifyConvergence [e] = property $ validTrace lastG e
    verifyConvergence (e:es) =
        if validTrace validG e then verifyConvergence es
                               else property False

    validTrace :: (GSV -> Bool) -> TraceKeepAliveClient String -> Bool
    validTrace vg (AddSample _ rtt PeerGSV{outboundGSV, inboundGSV}) =
        unNetworkDelay nd == rtt && vg outboundGSV && vg inboundGSV

    validG :: GSV -> Bool
    validG (GSV g _ _) = g >= 0 && g < 2 * unNetworkDelay nd

    lastG :: GSV -> Bool
    lastG (GSV g _ _) =
        let low = 0.95 * (unNetworkDelay nd) / 2
            high = 1.05 * (unNetworkDelay nd) / 2 in
        g >= low && g <= high

dynamicTracer :: Typeable a => Tracer (IOSim s) a
dynamicTracer = Tracer traceM
