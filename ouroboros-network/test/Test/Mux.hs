{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Mux (tests) where

import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.ST (stToIO)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Protocol.Codec
import           Protocol.Driver (Result (..), useCodecWithDuplex)

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Protocol.ReqResp.Client
import           Ouroboros.Network.Protocol.ReqResp.Codec.Cbor
import           Ouroboros.Network.Protocol.ReqResp.Server
import           Ouroboros.Network.Serialise

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"        prop_mux_snd_recv
  ]

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving (Eq, Show)

instance Arbitrary DummyPayload where
    arbitrary = do
       len  <- choose (0, 2 * 1024 * 1024)
       p <- arbitrary
       let blob = BL.replicate len p
       return $ DummyPayload blob

instance Serialise DummyPayload where
    encode a = encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> decodeBytes

data MuxSTMCtx m = MuxSTMCtx {
      writeQueue :: TBQueue m BL.ByteString
    , readQueue  :: TBQueue m BL.ByteString
    , sduSize    :: Word16
}

startMuxSTM :: Mx.MiniProtocolDescriptions IO
            -> TBQueue IO BL.ByteString
            -> TBQueue IO BL.ByteString
            -> Word16
            -> IO ()
startMuxSTM mpds wq rq mtu = do
    let ctx = MuxSTMCtx wq rq mtu
    jobs <- Mx.muxJobs mpds (writeMux ctx) (readMux ctx) (sduSizeMux ctx)
    aids <- mapM async jobs
    fork (watcher aids)
  where
    watcher as = do
        (_,r) <- waitAnyCatchCancel as
        case r of
             Left  e -> print $ "Mux Bearer died due to " ++ show e
             Right _ -> return ()

sduSizeMux :: (Monad m)
           => MuxSTMCtx m
           -> m Word16
sduSizeMux ctx = return $ sduSize ctx

writeMux :: (MonadTimer m, MonadSTM m)
         => MuxSTMCtx m
         -> Mx.MuxSDU
         -> m (Time m)
writeMux ctx sdu = do
    ts <- getMonotonicTime
    let buf = Mx.encodeMuxSDU sdu -- XXX Timestamp isn't set
    atomically $ writeTBQueue (writeQueue ctx) buf
    return ts

readMux :: (MonadTimer m, MonadSTM m)
        => MuxSTMCtx m
        -> m (Mx.MuxSDU, Time m)
readMux ctx = do
    buf <- atomically $ readTBQueue (readQueue ctx)
    let (hbuf, payload) = BL.splitAt 8 buf
    case Mx.decodeMuxSDUHeader hbuf of
         Nothing     -> error "failed to decode header" -- XXX
         Just header -> do
             ts <- getMonotonicTime
             return (header {Mx.msBlob = payload}, ts)

prop_mux_snd_recv :: DummyPayload
                  -> DummyPayload
                  -> Property
prop_mux_snd_recv request response = ioProperty $ do
    let sduLen = 14000

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    let server_w = client_r
        server_r = client_w

    let client_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        (clientInit clientResultVar)
                        dummyCallback)
                    ]
        server_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [(Mx.ChainSync, Mx.MiniProtocolDescription Mx.ChainSync
                        dummyCallback
                        (serverRsp serverResultVar))
                    ]

    startMuxSTM client_mps client_w client_r sduLen
    startMuxSTM server_mps server_w server_r sduLen

    (serverResult, clientResult) <- atomically $
        (,) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

    case (serverResult, clientResult) of
         (Normal request', Normal response') ->
             return $ request' === request .&. response' === response

         _ -> return $ property False

  where
    serverPeer = reqRespServerPeer (ReqRespServer $ \req -> return (response, req))

    clientPeer = reqRespClientPeer (Request request return)

    codec = hoistCodec stToIO codecReqResp

    clientInit clientResultVar clientChan = do
        result <- useCodecWithDuplex clientChan codec clientPeer
        atomically (putTMVar clientResultVar result)

    serverRsp serverResultVar serverChan = do
        result <- useCodecWithDuplex serverChan codec serverPeer
        atomically (putTMVar serverResultVar result)

    dummyCallback _ = forever $
        threadDelay 1000000

