{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Pipe (tests) where

import           Codec.Serialise (Serialise (..))
import           Control.Exception
import           Control.Monad
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadTimer
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Void (Void)
import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Tracer

import qualified Network.Mux.Bearer.Pipe as Mx
import           Ouroboros.Network.Mux

#if defined(mingw32_HOST_OS)
import           Data.Bits ((.|.))

import qualified System.Win32.NamedPipes as Win32.NamedPipes
import qualified System.Win32.Async      as Win32.Async
import qualified System.Win32            as Win32
#else
import           System.Process (createPipe)
import           System.IO (hClose)
#endif

import           Ouroboros.Network.Block (encodeTip, decodeTip)
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Server as ChainSync

activeTracer :: Show a => Tracer IO a
activeTracer = nullTracer
--activeTracer = showTracing stdoutTracer

--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "Pipe"
   [ testProperty "pipe sync demo" (withMaxSuccess 32 prop_pipe_demo)
   ]

--
-- Properties
--

prop_pipe_demo :: TestBlockChainAndUpdates -> Property
prop_pipe_demo (TestBlockChainAndUpdates chain updates) =
    ioProperty $ demo chain updates

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

-- | The enumeration of all mini-protocols in our demo protocol.
data DemoProtocols = ChainSync
  deriving (Eq, Ord, Enum, Bounded, Show)

instance ProtocolEnum DemoProtocols where
  fromProtocolEnum ChainSync = MiniProtocolNum 2

instance MiniProtocolLimits DemoProtocols where
  maximumMessageSize ChainSync  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSync = defaultMiniProtocolLimit

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        (Chain.HasHeader block, Serialise (Chain.HeaderHash block), Serialise block, Eq block, Show block )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = do
-- instrumentation of pipes is system dependet; on Windows we use NamedPipes
-- and async IO using I/O completion ports, on other systems we default to
-- posix anonymous pipes.
#if defined(mingw32_HOST_OS)
  -- using named pipe
  Win32.Async.withIOManager $ \iocp ->
    let pipeName = "\\\\.\\pipe\\demo-pipe" in
    bracket
      ((,) <$> Win32.NamedPipes.createNamedPipe
                  -- TODO: clean exports of `Win32.NamedPipes`:
                  -- 'fFILE_FLAG_OVERLAPPED' should be re-exported.
                  pipeName
                  (Win32.NamedPipes.pIPE_ACCESS_DUPLEX .|. Win32.fILE_FLAG_OVERLAPPED)
                  (Win32.NamedPipes.pIPE_TYPE_BYTE .|. Win32.NamedPipes.pIPE_READMODE_BYTE)
                  Win32.NamedPipes.pIPE_UNLIMITED_INSTANCES
                  maxBound
                  maxBound
                  0
                  Nothing
           <*> Win32.NamedPipes.createFile
                 pipeName
                 (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE)
                 (Win32.fILE_SHARE_NONE)
                 Nothing
                 Win32.oPEN_EXISTING
                 Win32.fILE_FLAG_OVERLAPPED
                 Nothing)
      (\(namedPipe, file) -> Win32.closeHandle namedPipe >> Win32.closeHandle file)
      $ \ (namedPipe, file) -> do
        Win32.Async.associateWithIOCompletionPort (Left namedPipe)  iocp
        Win32.Async.connectNamedPipe namedPipe
        Win32.Async.associateWithIOCompletionPort (Left file) iocp
        let chan1 = Mx.pipeChannelFromNamedPipe namedPipe
            chan2 = Mx.pipeChannelFromNamedPipe file
#else
    -- using posix pipes
    bracket
      ((,) <$> createPipe <*> createPipe)
      (\((a, b), (x, y)) -> do
        hClose a
        hClose b
        hClose x
        hClose y)
      $ \((hndRead1, hndWrite1), (hndRead2, hndWrite2)) -> do
        let chan1 = Mx.pipeChannelFromHandles hndRead1 hndWrite2
            chan2 = Mx.pipeChannelFromHandles hndRead2 hndWrite1
#endif
        producerVar <- atomically $ newTVar (CPS.initChainProducerState chain0)
        consumerVar <- atomically $ newTVar chain0
        done <- atomically newEmptyTMVar

        let Just expectedChain = Chain.applyChainUpdates updates chain0
            target = Chain.headPoint expectedChain

            consumerApp :: OuroborosApplication InitiatorApp String DemoProtocols IO BL.ByteString () Void
            consumerApp = simpleInitiatorApplication $
              \ChainSync ->
                MuxPeer nullTracer
                        (ChainSync.codecChainSync encode             (fmap const decode)
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode))
                        (ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar)))

            server :: ChainSyncServer block (Tip block) IO ()
            server = ChainSync.chainSyncServerExample () producerVar

            producerApp ::OuroborosApplication ResponderApp String DemoProtocols IO BL.ByteString Void ()
            producerApp = simpleResponderApplication $
              \ChainSync ->
                MuxPeer nullTracer
                        (ChainSync.codecChainSync encode             (fmap const decode)
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode))
                        (ChainSync.chainSyncServerPeer server)

        _ <- async $ Mx.runMuxWithPipes activeTracer (toApplication producerApp "producer") chan1
        _ <- async $ Mx.runMuxWithPipes activeTracer (toApplication consumerApp "consumer") chan2

        void $ fork $ sequence_
            [ do threadDelay 10e-4 -- 1 milliseconds, just to provide interest
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
