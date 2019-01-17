{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.ChainSync where

import Control.Monad (unless)
import Control.Monad.ST.Lazy (runST)
import Control.Monad.Free (Free)
import Data.Functor.Identity (Identity (..))
import Data.ByteString (ByteString)
import System.Process (createPipe)

import Codec.Serialise.Class (Serialise)
import Codec.CBOR.Encoding (Encoding)

import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadProbe

import Control.Monad.IOSim (SimF)

import Protocol.Core (Those (..), connect)
import Protocol.Codec
import Protocol.Channel
import Protocol.Driver (Result (..), useCodecWithDuplex)

import Ouroboros.Network.Pipe (pipeDuplex)
import qualified Ouroboros.Network.ChainProducerState as ChainProducerState

import Ouroboros.Network.Protocol.ChainSync.Client
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Direct
import Ouroboros.Network.Protocol.ChainSync.Codec.Cbor
import qualified Ouroboros.Network.Protocol.ChainSync.Examples as ChainSyncExamples

import Ouroboros.Network.Testing.ConcreteBlock (Block (..))
import Test.ChainProducerState (ChainProducerStateForkTest (..))
import Test.Ouroboros.Network.Testing.Utils (runExperiment, tmvarChannels)

import Test.QuickCheck hiding (Result)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.ChainSyncProtocol"
  [ -- testProperty "direct" (prop_direct @Int @Int)
  -- , testProperty "connect" (prop_connect @Int @Int)
  {--
    - , testProperty "ReqRespDemoST" (prop_reqRespDemoExperimentST @Int @Int)
    - , testProperty "ReqRespDemoIO" (prop_reqRespDemoExperimentIO @Int @Int)
    - , testProperty "ResRespPipe" (prop_reqRespPipeExperiment @Int @Int)
    --}
  ]

chainSyncExperiment
  :: forall m.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     )
  => ChainProducerStateForkTest
  -> Probe m Property
  -> m ()
chainSyncExperiment (ChainProducerStateForkTest cps fork) probe = do
  cpsvar <- atomically $ newTVar cps
  chainvar <- atomically $ newTVar fork
  let server = ChainSyncExamples.chainSyncServerExample
        (error "chainSyncServerExample: lazy in the result type")
        cpsvar
      client = ChainSyncExamples.chainSyncClientExample chainvar ChainSyncExamples.pureClient
  _ <- direct server client

  pchain <- ChainProducerState.producerChain <$> atomically (readTVar cpsvar)
  cchain <- atomically (readTVar chainvar)

  probeOutput probe (pchain === cchain)

