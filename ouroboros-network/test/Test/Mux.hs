{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Mux (tests) where

import qualified Codec.CBOR.Encoding as CBOR (Encoding)
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.ST (stToIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.List (nub)
import qualified Data.Map.Strict as M
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTimer
import           Protocol.Channel
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
  , testProperty "2 miniprotols"           prop_mux_2_minis
  , testProperty "starvation"              prop_mux_starvation
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
    , traceQueue :: Maybe (TBQueue m (Mx.MiniProtocolId, Mx.MiniProtocolMode, Time m))
}

startMuxSTM :: Mx.MiniProtocolDescriptions IO
            -> TBQueue IO BL.ByteString
            -> TBQueue IO BL.ByteString
            -> Word16
            -> Maybe (TBQueue IO (Mx.MiniProtocolId, Mx.MiniProtocolMode, Time IO))
            -> IO ()
startMuxSTM mpds wq rq mtu trace = do
    let ctx = MuxSTMCtx wq rq mtu trace
    jobs <- Mx.muxJobs mpds (writeMux ctx) (readMux ctx) (sduSizeMux ctx)
    aids <- mapM async jobs
    void $ fork (watcher aids)
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
             case traceQueue ctx of
                  Just q  -> atomically $ do
                      full <- isFullTBQueue q
                      if full then return ()
                              else writeTBQueue q (Mx.msId header, Mx.msMode header, ts)
                  Nothing -> return ()
             return (header {Mx.msBlob = payload}, ts)

prop_mux_snd_recv :: DummyPayload
                  -> DummyPayload
                  -> Property
prop_mux_snd_recv request response = ioProperty $ do
    let sduLen = 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let server_w = client_r
        server_r = client_w

    (verify, client_mp, server_mp) <- setupMiniReqRsp Mx.ChainSync (return ()) endMpsVar request response

    let client_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.ChainSync, client_mp )
                    ]
        server_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.ChainSync, server_mp )
                    ]

    startMuxSTM client_mps client_w client_r sduLen Nothing
    startMuxSTM server_mps server_w server_r sduLen Nothing

    property <$> verify

-- Stub for a mpdInitiator or mpdResponder that doesn't send or receive any data.
dummyCallback :: (MonadTimer m) => Duplex m m CBOR.Encoding BS.ByteString -> m ()
dummyCallback _ = forever $
    threadDelay 1000000

-- | Create a verification function, a MiniProtocolDescription for the client side and a
-- MiniProtocolDescription for the server side for a RequestResponce protocol.
setupMiniReqRsp :: Mx.MiniProtocolId
                -> IO ()
                -> TVar IO Int
                -> DummyPayload
                -> DummyPayload
                -> IO (IO Bool
                      , Mx.MiniProtocolDescription IO
                      , Mx.MiniProtocolDescription IO)
setupMiniReqRsp mid serverAction mpsEndVar request response = do
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    let client_mp = Mx.MiniProtocolDescription mid (clientInit clientResultVar) dummyCallback
        server_mp = Mx.MiniProtocolDescription mid dummyCallback (serverRsp serverResultVar)

    return (verifyCallback serverResultVar clientResultVar, client_mp, server_mp)
  where
    verifyCallback serverResultVar clientResultVar = do
        (serverResult, clientResult) <- atomically $
            (,) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

        case (serverResult, clientResult) of
             (Normal request', Normal response') ->
                 return $ request' == request && response' == response

             _ -> return False

    serverPeer = reqRespServerPeer (ReqRespServer $ \req -> serverAction >> return (response, req))

    clientPeer = reqRespClientPeer (Request request return)

    codec = hoistCodec stToIO codecReqResp

    clientInit clientResultVar clientChan = do
        result <- useCodecWithDuplex clientChan codec clientPeer
        atomically (putTMVar clientResultVar result)
        end

    serverRsp serverResultVar serverChan = do
        result <- useCodecWithDuplex serverChan codec serverPeer
        atomically (putTMVar serverResultVar result)
        end

    end = do
        atomically $ modifyTVar' mpsEndVar (\a -> a - 1)
        atomically $ do
            c <- readTVar mpsEndVar
            unless (c == 0) retry

waitOnAllClients :: TVar IO Int
                 -> Int
                 -> IO ()
waitOnAllClients clientVar clientTot = do
        atomically $ modifyTVar' clientVar (+ 1)
        atomically $ do
            c <- readTVar clientVar
            unless (c == clientTot) retry

prop_mux_2_minis :: DummyPayload
                 -> DummyPayload
                 -> DummyPayload
                 -> DummyPayload
                 -> Property
prop_mux_2_minis request0 response0 response1 request1 = ioProperty $ do
    let sduLen = 14000

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 4

    let server_w = client_r
        server_r = client_w

    (verify_0, client_mp0, server_mp0) <- setupMiniReqRsp Mx.ChainSync  (return ()) endMpsVar request0 response0
    (verify_1, client_mp1, server_mp1) <- setupMiniReqRsp Mx.BlockFetch (return ()) endMpsVar request1 response1

    let client_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.ChainSync, client_mp0 )
                    , ( Mx.BlockFetch, client_mp1 )
                    ]
        server_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.ChainSync, server_mp0 )
                    , ( Mx.BlockFetch, server_mp1 )
                    ]

    startMuxSTM client_mps client_w client_r sduLen Nothing
    startMuxSTM server_mps server_w server_r sduLen Nothing

    res0 <- verify_0
    res1 <- verify_1

    return $ property $ res0 && res1

prop_mux_starvation :: DummyPayload
                    -> DummyPayload
                    -> Property
prop_mux_starvation response0 response1 =
    let sduLen        = 1260 in
    (BL.length (unDummyPayload response0) > 2* (fromIntegral sduLen)) &&
    (BL.length (unDummyPayload response1) > 2* (fromIntegral sduLen)) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- atomically $ newTVar 0
    endMpsVar <- atomically $ newTVar 4
    traceQueueVar <- atomically $ newTBQueue 100
    let server_w = client_r
        server_r = client_w

    (verify_short, client_short, server_short) <-
        setupMiniReqRsp Mx.ChainSync (waitOnAllClients activeMpsVar 2) endMpsVar request response0
    (verify_long, client_long, server_long) <-
        setupMiniReqRsp Mx.BlockFetch (waitOnAllClients activeMpsVar 2) endMpsVar request response1

    let client_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.BlockFetch, client_short )
                    , ( Mx.ChainSync,  client_long )
                    ]
        server_mps = Mx.MiniProtocolDescriptions $ M.fromList
                    [ ( Mx.BlockFetch, server_short )
                    , ( Mx.ChainSync,  server_long )
                    ]

    startMuxSTM client_mps client_w client_r sduLen $ Just traceQueueVar
    startMuxSTM server_mps server_w server_r sduLen Nothing

    res_short <- verify_short
    res_long <- verify_long

    trace <- atomically $ flushTBQueue traceQueueVar []
    let es = map (\(e, _, _) -> e) trace
        ls = dropWhile (\e -> e == head es) es

   -- We can't make 100% sure that both servers start responding at the same time
   -- but once they are both up and running messages should alternate between
   -- Mx.BlockFetch and Mx.ChainSync hence length (nub $ take 2 ls) == 2.
    return $ property $ res_short && res_long && length (nub $ take 2 ls) == 2


  where
    flushTBQueue q acc = do
        e <- isEmptyTBQueue q
        if e then return $ reverse acc
             else do
                 a <- readTBQueue q
                 flushTBQueue q (a : acc)
