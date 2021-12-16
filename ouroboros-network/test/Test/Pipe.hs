{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
import           Data.Void (Void)
import           Test.ChainGenerators (TestBlockChainAndUpdates (..))
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Tracer

import qualified Network.Mux.Bearer.Pipe as Mx
import qualified Network.Mux.Compat as Mx (muxStart)
import           Ouroboros.Network.Mux

#if defined(mingw32_HOST_OS)
import           Data.Bits ((.|.))

import           System.IOManager
import qualified System.Win32 as Win32
import qualified System.Win32.Async as Win32.Async
import qualified System.Win32.NamedPipes as Win32.NamedPipes
#else
import           System.IO (hClose)
import           System.Process (createPipe)
#endif

import           Ouroboros.Network.Block (decodeTip, encodeTip)
import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.MockChain.Chain (Chain, ChainUpdate, Point)
import qualified Ouroboros.Network.MockChain.Chain as Chain
import qualified Ouroboros.Network.MockChain.ProducerState as CPS
import           Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import           Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import           Ouroboros.Network.Util.ShowProxy

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

defaultMiniProtocolLimit :: Int
defaultMiniProtocolLimit = 3000000

-- | The bundle of mini-protocols in our demo protocol: only chain sync
--
demoProtocols :: RunMiniProtocol appType bytes m a b
              -> OuroborosApplication appType addr bytes m a b
demoProtocols chainSync =
    OuroborosApplication $ \_connectionId _shouldStopSTM -> [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolLimits = MiniProtocolLimits {
                               maximumIngressQueue = defaultMiniProtocolLimit
                             },
        miniProtocolRun    = chainSync
      }
    ]

-- | A demonstration that we can run the simple chain consumer protocol
-- over a pipe with full message serialisation, framing etc.
--
demo :: forall block .
        ( Chain.HasHeader block
        , Serialise (Chain.HeaderHash block)
        , Serialise block
        , Eq block
        , Show block
        , ShowProxy block
        )
     => Chain block -> [ChainUpdate block block] -> IO Bool
demo chain0 updates = do
-- instrumentation of pipes is system dependet; on Windows we use NamedPipes
-- and async IO using I/O completion ports, on other systems we default to
-- posix anonymous pipes.
#if defined(mingw32_HOST_OS)
  -- using named pipe
  withIOManager $ \ioManager ->
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
           <*> Win32.NamedPipes.connect
                 pipeName
                 (Win32.gENERIC_READ .|. Win32.gENERIC_WRITE)
                 (Win32.fILE_SHARE_NONE)
                 Nothing
                 Win32.oPEN_EXISTING
                 Win32.fILE_FLAG_OVERLAPPED
                 Nothing)
      (\(namedPipe, file) -> Win32.closeHandle namedPipe >> Win32.closeHandle file)
      $ \ (namedPipe, file) -> do
        associateWithIOManager ioManager (Left namedPipe)
        Win32.Async.connectNamedPipe namedPipe
        associateWithIOManager ioManager (Left file)
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

            consumerApp :: OuroborosApplication InitiatorMode String BL.ByteString IO () Void
            consumerApp = demoProtocols chainSyncInitator

            chainSyncInitator =
              InitiatorProtocolOnly $
                MuxPeer nullTracer
                        (ChainSync.codecChainSync encode             decode
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode))
                        (ChainSync.chainSyncClientPeer
                          (ChainSync.chainSyncClientExample consumerVar
                          (consumerClient done target consumerVar)))

            server :: ChainSyncServer block (Point block) (Tip block) IO ()
            server = ChainSync.chainSyncServerExample () producerVar

            producerApp ::OuroborosApplication ResponderMode String BL.ByteString IO Void ()
            producerApp = demoProtocols chainSyncResponder

            chainSyncResponder =
              ResponderProtocolOnly $
                MuxPeer nullTracer
                        (ChainSync.codecChainSync encode             decode
                                                  encode             decode
                                                  (encodeTip encode) (decodeTip decode))
                        (ChainSync.chainSyncServerPeer server)

        let clientBearer = Mx.pipeAsMuxBearer activeTracer chan1
            serverBearer = Mx.pipeAsMuxBearer activeTracer chan2

        _ <- async $
              Mx.muxStart
                activeTracer
                (toApplication
                  (ConnectionId "producer" "consumer")
                  (continueForever (Proxy :: Proxy IO))
                  producerApp)
                clientBearer
        _ <- async $
              Mx.muxStart
                activeTracer
                (toApplication
                  (ConnectionId "consumer" "producer")
                  (continueForever (Proxy :: Proxy IO))
                  consumerApp)
                serverBearer

        void $ forkIO $ sequence_
            [ do threadDelay 10e-4 -- 1 milliseconds, just to provide interest
                 atomically $ do
                     p <- readTVar producerVar
                     let Just p' = CPS.applyChainUpdate update p
                     writeTVar producerVar p'
                 | update <- updates
            ]

        -- TODO: use new mechanism to collect mini-protocol result:
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
