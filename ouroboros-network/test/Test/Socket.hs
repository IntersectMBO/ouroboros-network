{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
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
#if defined(mingw32_HOST_OS)
import qualified Data.ByteString as BS
#endif

import           Data.Int (Int64)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Void (Void)
import qualified Network.Socket as Socket

import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (ThreadId)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTimer
import           Control.Concurrent (ThreadId)
import           Control.Tracer

-- TODO: remove Mx prefixes
import qualified Network.Mux as Mx hiding (MiniProtocolLimits(..))
import           Ouroboros.Network.Mux as Mx

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
import           Ouroboros.Network.Protocol.Handshake.Type
                     (cborTermVersionDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Testing.Serialise

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


defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

data TestProtocols1 = ChainSyncPr
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ChainSyncPr = MiniProtocolNum 2

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ChainSyncPr  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSyncPr = defaultMiniProtocolLimit


--
-- Properties
--

-- | Test chainsync over a socket bearer
prop_socket_demo :: TestBlockChainAndUpdates -> Property
prop_socket_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates


demo :: forall block .
        ( Chain.HasHeader block, Serialise (Chain.HeaderHash block)
        , Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = withIOManager $ \iocp -> do
    producerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "6061")
    consumerAddressInfo:_ <- Socket.getAddrInfo Nothing (Just "127.0.0.1") (Just "0")

    let producerAddress = Socket.addrAddress producerAddressInfo
        consumerAddress = Socket.addrAddress consumerAddressInfo

    producerVar <- newTVarM (CPS.initChainProducerState chain0)
    consumerVar <- newTVarM chain0
    done <- atomically newEmptyTMVar
    networkState <- newNetworkMutableState

    let Just expectedChain = Chain.applyChainUpdates updates chain0
        target = Chain.headPoint expectedChain

        initiatorApp :: OuroborosApplication Mx.InitiatorApp (ConnectionId Socket.SockAddr) TestProtocols1 IO BL.ByteString () Void
        initiatorApp = simpleInitiatorApplication $
          \ChainSyncPr ->
              MuxPeer nullTracer
                      codecChainSync
                      (ChainSync.chainSyncClientPeer
                        (ChainSync.chainSyncClientExample consumerVar
                        (consumerClient done target consumerVar)))

        server :: ChainSync.ChainSyncServer block (Tip block) IO ()
        server = ChainSync.chainSyncServerExample () producerVar

        responderApp :: OuroborosApplication Mx.ResponderApp (ConnectionId Socket.SockAddr) TestProtocols1 IO BL.ByteString Void ()
        responderApp = simpleResponderApplication $
          \ChainSyncPr ->
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
      producerAddress
      cborTermVersionDataCodec
      (\(DictVersion _) -> acceptableVersion)
      (simpleSingletonVersions
        (0::Int)
        (NodeToNodeVersionData $ NetworkMagic 0)
        (DictVersion nodeToNodeCodecCBORTerm)
        (SomeResponderApplication responderApp))
      nullErrorPolicies
      $ \_ _ -> do
      withAsync
        (connectToNode
          (socketSnocket iocp)
          cborTermVersionDataCodec
          nullNetworkConnectTracers
          (simpleSingletonVersions (0::Int) (NodeToNodeVersionData $ NetworkMagic 0) (DictVersion nodeToNodeCodecCBORTerm) initiatorApp)
          (Just consumerAddress)
          producerAddress)
        $ \ _connAsync -> do
          void $ fork $ sequence_
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
                   -> ChainSync.Client block (Tip block) IO ()
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
