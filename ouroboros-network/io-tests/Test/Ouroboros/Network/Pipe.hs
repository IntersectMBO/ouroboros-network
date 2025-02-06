{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans     #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.Ouroboros.Network.Pipe (tests) where

import Codec.Serialise (Serialise (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadTimer.SI
import Data.ByteString.Lazy qualified as BL
import Data.Monoid.Synchronisation
import Data.Void (Void)
import Test.ChainGenerators (TestBlockChainAndUpdates (..))
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Control.Tracer

import Network.Mux qualified as Mx
import Network.Mux.Bearer qualified as Mx
import Network.Mux.Bearer.Pipe qualified as Mx
import Ouroboros.Network.Mux

#if defined(mingw32_HOST_OS)
import Data.Bits ((.|.))

import System.IOManager
import System.Win32 qualified as Win32
import System.Win32.Async qualified as Win32.Async
import System.Win32.NamedPipes qualified as Win32.NamedPipes
#else
import System.IO (hClose)
import System.Process (createPipe)
#endif

import Ouroboros.Network.Block (decodeTip, encodeTip)
import Ouroboros.Network.Context
import Ouroboros.Network.Mock.Chain (Chain, ChainUpdate, Point)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Mock.ProducerState qualified as CPS
import Ouroboros.Network.Protocol.ChainSync.Client as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Codec as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Examples as ChainSync
import Ouroboros.Network.Protocol.ChainSync.Server as ChainSync
import Ouroboros.Network.Util.ShowProxy

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
demoProtocols :: RunMiniProtocolWithMinimalCtx appType addr bytes m a b
              -> OuroborosApplicationWithMinimalCtx appType addr bytes m a b
demoProtocols chainSync =
    OuroborosApplication [
      MiniProtocol {
        miniProtocolNum    = MiniProtocolNum 2,
        miniProtocolStart  = StartOnDemand,
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
        producerVar <- newTVarIO (CPS.initChainProducerState chain0)
        consumerVar <- newTVarIO chain0
        done <- newEmptyTMVarIO

        let Just expectedChain = Chain.applyChainUpdates updates chain0
            target = Chain.headPoint expectedChain

            consumerApp :: OuroborosApplicationWithMinimalCtx
                             Mx.InitiatorMode String BL.ByteString IO () Void
            consumerApp = demoProtocols chainSyncInitator

            chainSyncInitator =
              InitiatorProtocolOnly $
                mkMiniProtocolCbFromPeer $ \_ctx -> ( nullTracer
                                     , ChainSync.codecChainSync encode             decode
                                                                encode             decode
                                                     (encodeTip encode) (decodeTip decode)
                                     , ChainSync.chainSyncClientPeer
                                          (ChainSync.chainSyncClientExample consumerVar
                                            (consumerClient done target consumerVar))
                                     )

            server :: ChainSyncServer block (Point block) (Tip block) IO ()
            server = ChainSync.chainSyncServerExample () producerVar id

            producerApp ::OuroborosApplicationWithMinimalCtx
                            Mx.ResponderMode String BL.ByteString IO Void ()
            producerApp = demoProtocols chainSyncResponder

            chainSyncResponder =
              ResponderProtocolOnly $
                mkMiniProtocolCbFromPeer $ \_ctx -> ( nullTracer
                                     , ChainSync.codecChainSync encode             decode
                                                                encode             decode
                                                     (encodeTip encode) (decodeTip decode)
                                     , ChainSync.chainSyncServerPeer server
                                     )

        clientBearer <- Mx.getBearer Mx.makePipeChannelBearer (-1) activeTracer chan1 Nothing
        serverBearer <- Mx.getBearer Mx.makePipeChannelBearer (-1) activeTracer chan2 Nothing

        _ <- async $ do
              clientMux <- Mx.new (toMiniProtocolInfos (\_ _ -> Nothing) consumerApp)
              let initCtx = MinimalInitiatorContext (ConnectionId "consumer" "producer")
              resOps <- sequence
                [ Mx.runMiniProtocol
                    clientMux
                    miniProtocolNum
                    miniProtocolDir
                    Mx.StartEagerly
                    (\a -> do
                      r <- action a
                      return (r, Nothing)
                    )
                | MiniProtocol{miniProtocolNum, miniProtocolRun}
                    <- getOuroborosApplication consumerApp
                , (miniProtocolDir, action) <-
                    case miniProtocolRun of
                      InitiatorProtocolOnly initiator ->
                        [(Mx.InitiatorDirectionOnly, void . runMiniProtocolCb initiator initCtx)]
                ]
              withAsync (Mx.run nullTracer clientMux clientBearer) $ \aid -> do
                _ <- atomically $ runFirstToFinish $ foldMap FirstToFinish resOps
                Mx.stop clientMux
                wait aid

        _ <- async $ do
              serverMux <- Mx.new (toMiniProtocolInfos (\_ _ -> Nothing) producerApp)
              let respCtx = ResponderContext (ConnectionId "consumer" "producer")
              resOps <- sequence
                [ Mx.runMiniProtocol
                    serverMux
                    miniProtocolNum
                    miniProtocolDir
                    Mx.StartEagerly
                    (\a -> do
                      r <- action a
                      return (r, Nothing)
                    )
                | MiniProtocol{miniProtocolNum, miniProtocolRun}
                    <- getOuroborosApplication producerApp
                , (miniProtocolDir, action) <-
                    case miniProtocolRun of
                      ResponderProtocolOnly responder ->
                        [(Mx.ResponderDirectionOnly, void . runMiniProtocolCb responder respCtx)]
                ]
              withAsync (Mx.run nullTracer serverMux serverBearer) $ \aid -> do
                _ <- atomically $ runFirstToFinish $ foldMap FirstToFinish resOps
                Mx.stop serverMux
                wait aid

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
        , ChainSync.points = \_ -> pure $ Right $ consumerClient done target chain
        }
