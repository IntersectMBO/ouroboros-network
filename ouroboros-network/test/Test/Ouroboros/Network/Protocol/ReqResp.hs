{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.ReqResp where

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

import Ouroboros.Network.Protocol.ReqResp.Client
import Ouroboros.Network.Protocol.ReqResp.Server
import Ouroboros.Network.Protocol.ReqResp.Direct
import Ouroboros.Network.Protocol.ReqResp.Codec.Cbor

import Test.Ouroboros.Network.Testing.Utils (runExperiment, tmvarChannels)

import Test.QuickCheck hiding (Result)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.ReqRespProtocol"
  [ testProperty "direct" (prop_direct @Int @Int)
  , testProperty "connect" (prop_connect @Int @Int)
  , testProperty "ReqRespDemoST" (prop_reqRespDemoExperimentST @Int @Int)
  , testProperty "ReqRespDemoIO" (prop_reqRespDemoExperimentIO @Int @Int)
  , testProperty "ResRespPipe" (prop_reqRespPipeExperiment @Int @Int)
  ]

prop_direct
  :: forall request response.
     ( Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_direct request response =
  case runIdentity $ direct (ReqRespServer $ \req -> return (response, req)) (Request request return) of
    (request', response') -> request' === request .&&. response' === response

prop_connect
  :: forall request response.
     ( Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_connect request response =
  case runIdentity $ connect server client of
    These request' response' -> request' === request .&&. response' === response
    _                        -> property False
 where
  server = reqRespServerPeer (ReqRespServer $ \req -> return (response, req))
  client = reqRespClientPeer (Request request return)

reqRespDemoExperiment
  :: forall m request response.
     ( MonadST m
     , MonadSTM m
     , MonadProbe m
     , Serialise request
     , Serialise response
     , Eq request
     , Eq response
     , Show request
     , Show response
     )
  => Duplex m m Encoding ByteString
  -> Duplex m m Encoding ByteString
  -> request
  -> response
  -> Probe m Property
  -> m ()
reqRespDemoExperiment clientChan serverChan request response probe = withLiftST @m $ \liftST -> do

  doneVar <- atomically $ newTVar False
  
  let serverPeer = reqRespServerPeer (ReqRespServer $ \req -> return (response, req))
      clientPeer = reqRespClientPeer (Request request return)

      codec = hoistCodec liftST codecReqResp

  fork $ do
    result <- useCodecWithDuplex serverChan codec serverPeer
    case result of
      Normal request' -> probeOutput probe (request' === request)
      Unexpected _    -> probeOutput probe (property False)

  fork $ do
    result <- useCodecWithDuplex clientChan codec clientPeer
    case result of
      Normal response' -> probeOutput probe (response' === response)
      Unexpected _     -> probeOutput probe (property False)
    atomically $ writeTVar doneVar True

  atomically $ do
    done <- readTVar doneVar
    unless done retry

prop_reqRespDemoExperimentST
  :: forall request response.
     ( Serialise request
     , Serialise response
     , Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_reqRespDemoExperimentST request response =
  runST $ runExperiment $ \probe -> do
    (clientChan, serverChan) <- tmvarChannels
    reqRespDemoExperiment @(Free (SimF _)) clientChan serverChan request response probe

prop_reqRespDemoExperimentIO
  :: forall request response.
     ( Serialise request
     , Serialise response
     , Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_reqRespDemoExperimentIO request response =
  ioProperty $ runExperiment $ \probe -> do
    (clientChan, serverChan) <- tmvarChannels
    reqRespDemoExperiment clientChan serverChan request response probe

prop_reqRespPipeExperiment
  :: forall request response.
     ( Serialise request
     , Serialise response
     , Eq request
     , Eq response
     , Show request
     , Show response
     )
  => request
  -> response
  -> Property
prop_reqRespPipeExperiment request response =
  ioProperty $ runExperiment $ \probe -> do
    (serRead, cliWrite) <- createPipe
    (cliRead, serWrite) <- createPipe
    let clientChan = pipeDuplex cliRead cliWrite
        serverChan = pipeDuplex serRead serWrite
    reqRespDemoExperiment clientChan serverChan request response probe
