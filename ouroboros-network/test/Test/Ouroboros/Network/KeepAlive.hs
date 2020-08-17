{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Network.KeepAlive (tests) where

import           Control.Arrow ((&&&))
import           Control.Monad (void)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadThrow
import           Control.Monad.IOSim ( runSimTrace, selectTraceEventsSay
                                     , traceResult )
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           System.Random
import           Text.Printf


import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientRegistry
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Limits
import           Ouroboros.Network.DeltaQ
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server
import           Ouroboros.Network.Protocol.KeepAlive.Codec

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


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

newtype NetworkDelay = NetworkDelay DiffTime deriving Show

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
    => NetworkDelay
    -> Int
    -> m Property
prop_keepAlive_convergenceM (NetworkDelay nd) seed = do
    registry <- newFetchClientRegistry
    controlMessageV <- newTVarM Continue
    let controlMessageSTM = readTVar controlMessageV
        clientId = "client"
        timeConstant = 1000 -- Same as in PeerGSV's <> definition
        keepAliveInterval = 10

    (c_aid, s_aid) <- runKeepAliveClientAndServer (NetworkDelay nd) seed verboseTracer controlMessageSTM
                          registry clientId (KeepAliveInterval keepAliveInterval)
    threadDelay $ timeConstant * keepAliveInterval
    dqLive <- atomically $ readPeerGSVs registry

    atomically $ writeTVar controlMessageV Terminate
    void $ wait c_aid
    void $ wait s_aid

    -- XXX Must be larger than the KeepAliveInterval timeout or we leak threads in the SIM
    threadDelay (keepAliveInterval + 128)
    case M.lookup clientId dqLive of
         Nothing  -> return $ property False
         Just gsv -> do
             dqDead <- atomically $ readPeerGSVs registry

             return $ property $ (not $ M.member "client" dqDead) && gsvCheck gsv

  where
    gsvCheck :: PeerGSV -> Bool
    gsvCheck PeerGSV{outboundGSV, inboundGSV} =
        gCheck outboundGSV && gCheck inboundGSV

    gCheck :: GSV -> Bool
    gCheck (GSV g _ _) =
        let low = 0.95 * nd / 2
            high = 1.05 * nd / 2 in
        g >= low && g <= high

-- Test that our estimate of PeerGSV's G terms converge to
-- a given constant delay.
prop_keepAlive_convergence :: NetworkDelay -> Int -> Property
prop_keepAlive_convergence nd seed = do
    let (_output, r_e) = (selectTraceEventsSay &&& traceResult True)
                             (runSimTrace $ prop_keepAlive_convergenceM nd seed)
    ioProperty $ do
        --printf "new testcase %s\n" (show nd)
        --mapM_ (printf "%s\n") _output
        case r_e of
             Left  _ -> return $ property False
             Right r -> return r

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !Time
    , wtatWithinThread :: !String
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

verboseTracer :: forall a m.
                       ( MonadAsync m
                       , MonadFork m
                       , MonadMask m
                       , MonadSay m
                       , MonadST m
                       , MonadSTM m
                       , MonadThrow (STM m)
                       , MonadMonotonicTime m
                       , MonadTimer m
                       , Eq (Async m ())
                       , Show a
                       )
               => Tracer m a
verboseTracer = threadAndTimeTracer $ showTracing $ Tracer say

threadAndTimeTracer :: forall a m.
                       ( MonadAsync m
                       , MonadFork m
                       , MonadMask m
                       , MonadSay m
                       , MonadST m
                       , MonadSTM m
                       , MonadThrow (STM m)
                       , MonadMonotonicTime m
                       , MonadTimer m
                       , Eq (Async m ())
                       )
                    => Tracer m (WithThreadAndTime a) -> Tracer m a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getMonotonicTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now (show tid) s
