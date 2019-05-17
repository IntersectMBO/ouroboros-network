{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Mux
    ( TestProtocols1 (..)
    , TestProtocols2 (..)
    , TestProtocols3 (..)
    , DummyPayload (..)
    , setupMiniReqRsp
    , tests
    ) where

import           Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad
import qualified Data.Binary.Put as Bin
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (isNothing)
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer (nullTracer)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server

import           Ouroboros.Network.Time
import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Interface
import           Ouroboros.Network.Mux.Types (MuxBearer)
import qualified Ouroboros.Network.Mux.Types as Mx
import           Ouroboros.Network.Protocol.ReqResp.Codec

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"     prop_mux_snd_recv
  , testProperty "2 miniprotocols"      prop_mux_2_minis
  , testProperty "starvation"           prop_mux_starvation
  , testProperty "unknown miniprotocol" prop_mux_unknown_miniprot
  , testProperty "too short header"     prop_mux_short_header
  ]

data TestProtocols1 = ChainSync1
  deriving (Eq, Ord, Enum, Bounded)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ChainSync1 = 2

  toProtocolEnum 2 = Just ChainSync1
  toProtocolEnum _ = Nothing

-- |
-- Allows to run both chain-sync and block-fetch protocols.
--
data TestProtocols2 = ChainSync2 | BlockFetch2
  deriving (Eq, Ord, Enum, Bounded)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ChainSync2  = 2
  fromProtocolEnum BlockFetch2 = 3

  toProtocolEnum 2 = Just ChainSync2
  toProtocolEnum 3 = Just BlockFetch2
  toProtocolEnum _ = Nothing

-- |
-- Allow to run a singly req-resp protocol.
--
data TestProtocols3 = ReqResp1
  deriving (Eq, Ord, Enum, Bounded)

instance Mx.ProtocolEnum TestProtocols3 where
  fromProtocolEnum ReqResp1 = 4

  toProtocolEnum 4 = Just ReqResp1
  toProtocolEnum _ = Nothing

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving Eq

instance Show DummyPayload where
    show d = printf "DummyPayload %d\n" (BL.length $ unDummyPayload d)

instance Arbitrary DummyPayload where
    arbitrary = do
       len  <- choose (0, 2 * 1024 * 1024)
       p <- arbitrary
       let blob = BL.replicate len p
       return $ DummyPayload blob

instance Serialise DummyPayload where
    encode a = CBOR.encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> CBOR.decodeBytes

data InvalidSDU = InvalidSDU {
      isTimestamp :: !Mx.RemoteClockModel
    , isId      :: !Word16
    , isLength  :: !Word16
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x\n"
                    (Mx.unRemoteClockModel $ isTimestamp a)
                    (isId a)
                    (isLength a)

instance Arbitrary InvalidSDU where
    arbitrary = do
        ts  <- arbitrary
        mid <- choose (5, 0xffff) -- TxSubmission with 4 is the highest valid mid
        len <- arbitrary

        return $ InvalidSDU (Mx.RemoteClockModel ts) mid len


startMuxSTM :: (Ord ptcl, Enum ptcl, Bounded ptcl, Mx.ProtocolEnum ptcl)
            => MuxApplication appType ptcl IO
            -> TBQueue IO BL.ByteString
            -> TBQueue IO BL.ByteString
            -> Word16
            -> Maybe (TBQueue IO (Mx.MiniProtocolId ptcl, Time IO))
            -> IO (Async IO (Maybe SomeException))
startMuxSTM app wq rq mtu trace = async spawn
  where
    spawn = bracket (queuesAsMuxBearer wq rq mtu trace) Mx.close $ \bearer -> do
        res_e <- try $ Mx.muxStart app bearer
        case res_e of
             Left  e -> return (Just e)
             Right _ -> return Nothing


queuesAsMuxBearer
  :: forall ptcl m.
     ( MonadSTM   m
     , MonadTime  m
     , MonadThrow m
     , Mx.ProtocolEnum ptcl
     )
  => TBQueue m BL.ByteString
  -> TBQueue m BL.ByteString
  -> Word16
  -> Maybe (TBQueue m (Mx.MiniProtocolId ptcl, Time m))
  -> m (MuxBearer ptcl m)
queuesAsMuxBearer writeQueue readQueue sduSize traceQueue = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read    = readMux,
          Mx.write   = writeMux,
          Mx.close   = return (),
          Mx.sduSize = sduSizeMux,
          Mx.state   = mxState
        }
    where
      readMux :: m (Mx.MuxSDU ptcl, Time m)
      readMux = do
          buf <- atomically $ readTBQueue readQueue
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeMuxSDUHeader hbuf of
              Left  e      -> throwM e
              Right header -> do
                  ts <- getMonotonicTime
                  case traceQueue of
                        Just q  -> atomically $ do
                            full <- isFullTBQueue q
                            if full then return ()
                                    else writeTBQueue q (Mx.msId header, ts)
                        Nothing -> return ()
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.MuxSDU ptcl
               -> m (Time m)
      writeMux sdu = do
          ts <- getMonotonicTime
          let ts32 = timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          atomically $ writeTBQueue writeQueue buf
          return ts

      sduSizeMux :: m Word16
      sduSizeMux = return $ sduSize

-- | Verify that an initiator and a responder can send and receive messages from each other.
-- Large DummyPayloads will be split into sduLen sized messages and the testcases will verify
-- that they are correctly reassembled into the original message.
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

    (verify, clientApp, serverApp) <- setupMiniReqRsp
                                        (return ()) endMpsVar request response

    clientAsync <- startMuxSTM (MuxClientApplication $ \ReqResp1 -> clientApp) client_w client_r sduLen Nothing
    serverAsync <- startMuxSTM (MuxServerApplication $ \ReqResp1 -> serverApp) server_w server_r sduLen Nothing

    (\a b c -> a .&&. isNothing b .&&. isNothing c) <$> verify <*> wait clientAsync <*> wait serverAsync

-- |
-- Create a verification function, a MuxApplication for the client and server
-- side of @'ReqResp'@ protocol.
--
setupMiniReqRsp :: IO ()        -- | Action performed by responder before processing the response
                -> TVar IO Int  -- | Total number of miniprotocols.
                -> DummyPayload -- | Request, sent from initiator.
                -> DummyPayload -- | Response, sent from responder after receive the request.
                -> IO ( IO Bool
                      , Channel IO ByteString -> IO ()
                      , Channel IO ByteString -> IO ()
                      )
setupMiniReqRsp serverAction mpsEndVar request response = do
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    return ( verifyCallback serverResultVar clientResultVar
           , clientApp clientResultVar
           , serverApp serverResultVar
           )
  where
    verifyCallback serverResultVar clientResultVar =
        atomically $ (&&) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

    plainServer :: [DummyPayload] -> ReqRespServer DummyPayload DummyPayload IO Bool
    plainServer reqs = ReqRespServer {
        recvMsgReq  = \req -> serverAction >> return (response, plainServer (req:reqs)),
        recvMsgDone = pure $ reverse reqs == [request]
    }

    plainClient :: [DummyPayload] -> ReqRespClient DummyPayload DummyPayload IO Bool
    plainClient = go []
      where
        go resps []         = SendMsgDone (pure $ reverse resps == [response])
        go resps (req:reqs) = SendMsgReq req $ \resp -> return (go (resp:resps) reqs)

    serverPeer = reqRespServerPeer (plainServer [])
    clientPeer = reqRespClientPeer (plainClient [request])

    clientApp clientResultVar clientChan = do
        result <- runPeer nullTracer codecReqResp clientChan clientPeer
        atomically (putTMVar clientResultVar result)
        end

    serverApp serverResultVar serverChan = do
        result <- runPeer nullTracer codecReqResp serverChan serverPeer
        atomically (putTMVar serverResultVar result)
        end

    -- Wait on all miniprotocol jobs before letting a miniprotocol thread exit.
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

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
prop_mux_2_minis :: DummyPayload
                 -> DummyPayload
                 -> DummyPayload
                 -> DummyPayload
                 -> Property
prop_mux_2_minis request0 response0 response1 request1 = ioProperty $ do
    let sduLen = 14000

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 4 -- Two initiators and two responders.

    let server_w = client_r
        server_r = client_w

    (verify_0, client_mp0, server_mp0) <-
        setupMiniReqRsp (return ()) endMpsVar request0 response0
    (verify_1, client_mp1, server_mp1) <-
        setupMiniReqRsp (return ()) endMpsVar request1 response1

    let client_mps ChainSync2  = client_mp0
        client_mps BlockFetch2 = client_mp1

        server_mps ChainSync2  = server_mp0
        server_mps BlockFetch2 = server_mp1

    clientAsync <- startMuxSTM (MuxClientApplication client_mps) client_w client_r sduLen Nothing
    serverAsync <- startMuxSTM (MuxServerApplication server_mps) server_w server_r sduLen Nothing

    (\a b c d -> a .&&. b .&&. isNothing c .&&. isNothing d)
      <$> verify_0
      <*> verify_1
      <*> wait clientAsync
      <*> wait serverAsync

-- | Attempt to verify that capacity is diveded fairly between two active miniprotocols.
-- Two initiators send a request over two different miniprotocols and the corresponding responders
-- each send a large reply back. The Mux bearer should alternate between sending data for the two
-- responders.
prop_mux_starvation :: DummyPayload
                    -> DummyPayload
                    -> Property
prop_mux_starvation response0 response1 =
    let sduLen        = 1260 in
    (BL.length (unDummyPayload response0) > 2 * fromIntegral sduLen) &&
    (BL.length (unDummyPayload response1) > 2 * fromIntegral sduLen) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- atomically $ newTVar 0
    endMpsVar <- atomically $ newTVar 4          -- 2 active initiators and 2 active responders
    traceQueueVar <- atomically $ newTBQueue 100 -- At most track 100 packets per test run

    let server_w = client_r
        server_r = client_w

    (verify_short, client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar request response0
    (verify_long, client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar request response1

    let client_mps BlockFetch2 = client_short
        client_mps ChainSync2  = client_long

        server_mps BlockFetch2 = server_short
        server_mps ChainSync2  = server_long

    clientAsync <- startMuxSTM (MuxClientApplication client_mps) client_w client_r sduLen (Just traceQueueVar)
    serverAsync <- startMuxSTM (MuxServerApplication server_mps) server_w server_r sduLen Nothing

    -- First verify that all messages where received correctly
    res_short <- verify_short
    res_long <- verify_long
    c_e <- isNothing <$> wait clientAsync
    s_e <- isNothing <$> wait serverAsync

    -- Then look at the message trace to check for starvation.
    trace <- atomically $ flushTBQueue traceQueueVar []
    let es = map (\(e, _) -> e) trace
        ls = dropWhile (\e -> e == head es) es
        fair = verifyStarvation ls

    return $ property $ res_short .&&. res_long .&&. fair .&&. c_e .&&. s_e

  where
   -- We can't make 100% sure that both servers start responding at the same time
   -- but once they are both up and running messages should alternate between
   -- Mx.BlockFetch and Mx.ChainSync
    verifyStarvation :: Eq ptcl => [Mx.MiniProtocolId ptcl] -> Bool
    verifyStarvation []     = True
    verifyStarvation [_]    = True
    verifyStarvation (m:ms) =
        let longRun = takeWhile (\e -> e /= m) ms in
        if length longRun > 1 && elem m ms
           then False
           else verifyStarvation ms

    flushTBQueue q acc = do
        e <- isEmptyTBQueue q
        if e then return $ reverse acc
             else do
                 a <- readTBQueue q
                 flushTBQueue q (a : acc)

-- | Send message for an unknown miniprotocol and verify that the correct exception is thrown.
prop_mux_unknown_miniprot :: InvalidSDU
                          -> Property
prop_mux_unknown_miniprot badSdu = ioProperty $ do
    (client_w, serverAsync) <- failingServer

    atomically $ writeTBQueue client_w (encodeInvalidMuxSDU badSdu)

    res <- wait serverAsync
    case res of
         Just e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxUnknownMiniProtocol
                  Nothing -> return False
         Nothing -> return False

encodeInvalidMuxSDU :: InvalidSDU -> BL.ByteString
encodeInvalidMuxSDU sdu = Bin.runPut enc
  where
    enc = do
        Bin.putWord32be $ Mx.unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isId sdu
        Bin.putWord16be $ isLength sdu

-- | Send a too short SDU header and verify that the correct exception is thrown.
prop_mux_short_header :: InvalidSDU
                      -> Word8
                      -> Property
prop_mux_short_header badSdu shortCutOffArg = ioProperty $ do
    let shortCutOff = fromIntegral $ min shortCutOffArg 7 -- XXX Quickcheck 'Giving up' hack
    (client_w, serverAsync) <- failingServer

    atomically $ writeTBQueue client_w (BL.take shortCutOff $ encodeInvalidMuxSDU badSdu)

    res <- wait serverAsync
    case res of
         Just e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxDecodeError
                  Nothing -> return False
         Nothing -> return False

-- | Start a ReqResp server that is expected to fail, returns the server's read channel
failingServer :: IO (TBQueue IO BL.ByteString, Async IO (Maybe SomeException))
failingServer  = do
    server_r <- atomically $ newTBQueue 10
    server_w <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let sduLen = 1260
        request  = DummyPayload $ BL.replicate 4 0xa
        response = request

    (_, _, server_mp) <- setupMiniReqRsp (return ()) endMpsVar request response
    let server_mps ChainSync1 = server_mp
    serverAsync <- startMuxSTM (MuxServerApplication server_mps) server_w server_r sduLen Nothing

    return (server_r, serverAsync)
