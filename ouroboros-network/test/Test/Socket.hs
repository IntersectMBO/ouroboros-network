{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
module Test.Socket (tests) where

import qualified Data.ByteString.Lazy as BL

import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Control.Concurrent (ThreadId)
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer

import           Ouroboros.Network.Mux
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Socket

import           Ouroboros.Network.Block (Tip, decodeTip, encodeTip)
import           Ouroboros.Network.IOManager
import           Ouroboros.Network.Magic
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.NodeToNode
import qualified Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import qualified Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (cborTermVersionDataCodec, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (acceptableVersion)
import           Ouroboros.Network.Testing.Serialise
import           Ouroboros.Network.Util.ShowProxy

import           Test.ChainGenerators (TestBlockChainAndUpdates (..))

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf
import           Text.Show.Functions ()


--
-- The list of all tests
--

tests :: TestTree
tests =
  testGroup "Socket"
  [ testProperty "socket sync demo" prop_socket_demo
  ]


defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

-- | The bundle of mini-protocols in our test protocol: only chain sync
--
testProtocols1 :: RunMiniProtocol appType bytes m a b
               -> OuroborosApplication appType addr bytes m a b
testProtocols1 chainSync =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = chainSync
      }
    ]


--
-- Properties
--

-- | Test chainsync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates


demo :: forall block .
        ( Chain.HasHeader block
        , Serialise (Chain.HeaderHash block)
        , Serialise block
        , Eq block
        , Show block
        , ShowProxy block
        )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = withIOManager $ \iocp -> do
    producerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")
    consumerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    let producerAddress = Socket.addrAddress producerAddressInfo
        consumerAddress = Socket.addrAddress consumerAddressInfo

    producerVar <- newTVarIO (CPS.initChainProducerState chain0)
    consumerVar <- newTVarIO chain0
    done <- atomically newEmptyTMVar
    networkState <- newNetworkMutableState

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        initiatorApp
          :: OuroborosApplication InitiatorMode Socket.SockAddr
                                  BL.ByteString IO () Void
        initiatorApp = testProtocols1 chainSyncInitator

        chainSyncInitator =
          InitiatorProtocolOnly $
              MuxPeer nullTracer
                      codecChainSync
                      (ChainSync.chainSyncClientPeer
                        (ChainSync.chainSyncClientExample consumerVar
                        (consumerClient done target consumerVar)))

        server :: ChainSync.ChainSyncServer block (Point block) (Tip block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar

        responderApp
          :: OuroborosApplication ResponderMode Socket.SockAddr
                                  BL.ByteString IO Void ()
        responderApp = testProtocols1 chainSyncResponder

        chainSyncResponder =
          ResponderProtocolOnly $
            MuxPeer nullTracer
                    codecChainSync
                    (ChainSync.chainSyncServerPeer server)

        codecChainSync = ChainSync.codecChainSync encode             decode
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode)

    withServerNode
      (socketSnocket iocp)
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      producerAddress
      nodeToNodeHandshakeCodec
      noTimeLimitsHandshake
      (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
      acceptableVersion
      (simpleSingletonVersions
        NodeToNodeV_1
        (NodeToNodeVersionData {
          networkMagic  = NetworkMagic 0,
          diffusionMode = InitiatorAndResponderDiffusionMode })
        (SomeResponderApplication responderApp))
      nullErrorPolicies
      $ \realProducerAddress _ -> do
      withAsync
        (connectToNode
          (socketSnocket iocp)
          nodeToNodeHandshakeCodec
          noTimeLimitsHandshake
          (cborTermVersionDataCodec nodeToNodeCodecCBORTerm)
          nullNetworkConnectTracers
          acceptableVersion
          (simpleSingletonVersions
            NodeToNodeV_1
            (NodeToNodeVersionData {
              networkMagic  = NetworkMagic 0,
              diffusionMode = InitiatorOnlyDiffusionMode })
            initiatorApp)
          (Just consumerAddress)
          realProducerAddress)
        $ \ _connAsync -> do
          void $ forkIO $ sequence_
              [ do
                  threadDelay 10e-4 -- just to provide interest
                  atomically $ do
                    p <- readTVar producerVar
                    let Just p' = CPS.applyChainUpdate update p
                    writeTVar producerVar p'
              | update <- updates
              ]

          atomically $ takeTMVar done

  where
    checkTip target consumerVar = atomically $ do
      chain <- readTVar consumerVar
      return (Chain.headPoint chain == target)

    -- A simple chain-sync client which runs until it recieves an update to
    -- a given point (either as a roll forward or as a roll backward).
    consumerClient :: StrictTMVar IO Bool
                   -> Point block
                   -> StrictTVar IO (Chain block)
                   -> ChainSync.Client block (Point block) (Tip block) IO ()
    consumerClient done target chain =
      ChainSync.Client
        { ChainSync.rollforward = \_ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.rollbackward = \_ _ -> checkTip target chain >>= \b ->
            if b then do
                    atomically $ putTMVar done True
                    pure $ Left ()
                 else
                    pure $ Right $ consumerClient done target chain
        , ChainSync.points = \_ -> pure $ consumerClient done target chain
        }

data WithThreadAndTime a = WithThreadAndTime {
      wtatOccuredAt    :: !UTCTime
    , wtatWithinThread :: !ThreadId
    , wtatEvent        :: !a
    }

instance (Show a) => Show (WithThreadAndTime a) where
    show WithThreadAndTime {wtatOccuredAt, wtatWithinThread, wtatEvent} =
        printf "%s: %s: %s" (show wtatOccuredAt) (show wtatWithinThread) (show wtatEvent)

_verboseTracer :: Show a => Tracer IO a
_verboseTracer = threadAndTimeTracer $ showTracing stdoutTracer

threadAndTimeTracer :: Tracer IO (WithThreadAndTime a) -> Tracer IO a
threadAndTimeTracer tr = Tracer $ \s -> do
    !now <- getCurrentTime
    !tid <- myThreadId
    traceWith tr $ WithThreadAndTime now tid s
