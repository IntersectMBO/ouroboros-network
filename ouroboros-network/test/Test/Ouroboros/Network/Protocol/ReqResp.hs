{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Test.Ouroboros.Network.Protocol.ReqResp where

import Data.Functor.Identity (Identity (..))
import Data.ByteString (ByteString)
import System.Process (createPipe)

import Codec.Serialise.Class (Serialise)
import Codec.CBOR.Encoding (Encoding)

import Control.Monad (void)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM

import Control.Monad.IOSim (runSimOrThrow)

import Protocol.Core (Those (..), connect)
import Protocol.Codec
import Protocol.Channel
import Protocol.Driver (Result (..), useCodecWithDuplex)

import Ouroboros.Network.Pipe (pipeDuplex)

import Ouroboros.Network.Protocol.ReqResp.Client
import Ouroboros.Network.Protocol.ReqResp.Server
import Ouroboros.Network.Protocol.ReqResp.Direct
import Ouroboros.Network.Protocol.ReqResp.Codec.Cbor

import Test.Ouroboros.Network.Testing.Utils (tmvarChannels)

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
     , MonadFork m
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
  -> m Property
reqRespDemoExperiment clientChan serverChan request response =
  withLiftST @m $ \liftST -> do

  let serverPeer = reqRespServerPeer (ReqRespServer $ \req -> return (response, req))
      clientPeer = reqRespClientPeer (Request request return)

      codec = hoistCodec liftST codecReqResp

  serverResultVar <- newEmptyTMVarM
  void $ fork $ do
    result <- useCodecWithDuplex serverChan codec serverPeer
    atomically (putTMVar serverResultVar result)

  clientResultVar <- newEmptyTMVarM
  void $ fork $ do
    result <- useCodecWithDuplex clientChan codec clientPeer
    atomically (putTMVar clientResultVar result)

  (serverResult, clientResult) <- atomically $
    (,) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

  case (serverResult, clientResult) of
    (Normal request', Normal response') ->
      return $ request' === request .&. response' === response
    
    _ -> return $ property False

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
  runSimOrThrow $ do
    (clientChan, serverChan) <- tmvarChannels
    reqRespDemoExperiment clientChan serverChan request response

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
  ioProperty $ do
    (clientChan, serverChan) <- tmvarChannels
    reqRespDemoExperiment clientChan serverChan request response

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
  ioProperty $ do
    (serRead, cliWrite) <- createPipe
    (cliRead, serWrite) <- createPipe
    let clientChan = pipeDuplex cliRead cliWrite
        serverChan = pipeDuplex serRead serWrite
    reqRespDemoExperiment clientChan serverChan request response
