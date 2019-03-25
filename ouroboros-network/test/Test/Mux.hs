{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Test.Mux (
      NetworkMagic (..)
    , NetworkMagicWithBufSize (..)
    , TestProtocols1 (..)
    , TestProtocols3 (..)
    , DummyPayload (..)
    , setupMiniReqRsp
    , tests
    , version0
    , version0'
    , version1) where

import           Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad
import qualified Data.Binary.Put as Bin
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Data.Word
import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTimer
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.ReqResp.Client
import           Network.TypedProtocol.ReqResp.Server

import qualified Ouroboros.Network.Mux as Mx
import           Ouroboros.Network.Mux.Types (MuxBearer)
import qualified Ouroboros.Network.Mux.Types as Mx
import qualified Ouroboros.Network.Mux.Control as Mx
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

newtype NetworkMagic = NetworkMagic Word32
  deriving (Eq, Ord, Show)

instance Arbitrary NetworkMagic where
    arbitrary = NetworkMagic <$> arbitrary
    shrink (NetworkMagic nm) = map NetworkMagic (shrink nm)

instance Serialise NetworkMagic where
  encode (NetworkMagic nm) = encode nm
  decode = NetworkMagic <$> CBOR.decodeWord32

type instance Mx.MuxVersion 0 = NetworkMagic

data NetworkMagicWithBufSize = NetworkMagicWithBufSize Word32 Word16
  deriving (Eq, Ord, Show)

type instance Mx.MuxVersion 1 = NetworkMagicWithBufSize

instance Arbitrary NetworkMagicWithBufSize where
    arbitrary = NetworkMagicWithBufSize <$> arbitrary <*> arbitrary
    shrink (NetworkMagicWithBufSize nm s) =
      [ NetworkMagicWithBufSize nm' s
      | nm' <- shrink nm
      ]
      ++
      [ NetworkMagicWithBufSize nm s'
      | s' <- shrink s
      ]

instance Serialise NetworkMagicWithBufSize where
  encode (NetworkMagicWithBufSize nm s) =
         encodeListLen 2
      <> encode nm
      <> encode s
  decode =
         decodeListLen
      *> (NetworkMagicWithBufSize <$> decode <*> decode)

version0 :: Mx.SomeVersion
version0 = Mx.SomeVersion
  Mx.SNat0
  (NetworkMagic 0xcafebeeb)

version0' :: Mx.SomeVersion
version0' = Mx.SomeVersion
  Mx.SNat0
  (NetworkMagic 0xa5a5a5a)

version1 :: Mx.SomeVersion
version1 = Mx.SomeVersion
  Mx.SNat1
  (NetworkMagicWithBufSize 0xbeebcafe 10)

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

-- |
-- Allows to run a single chain-sync protocol.
--
data TestProtocols1 = ChainSync1
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ChainSync1 = 2

  toProtocolEnum 2 = Just ChainSync1
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ChainSync1  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSync1 = defaultMiniProtocolLimit

-- |
-- Allows to run both chain-sync and block-fetch protocols.
--
data TestProtocols2 = ChainSync2 | BlockFetch2
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ChainSync2  = 2
  fromProtocolEnum BlockFetch2 = 3

  toProtocolEnum 2 = Just ChainSync2
  toProtocolEnum 3 = Just BlockFetch2
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols2 where
  maximumMessageSize ChainSync2   = defaultMiniProtocolLimit
  maximumMessageSize BlockFetch2  = defaultMiniProtocolLimit
  maximumIngressQueue ChainSync2  = defaultMiniProtocolLimit
  maximumIngressQueue BlockFetch2 = defaultMiniProtocolLimit


-- |
-- Allow to run a singly req-resp protocol.
--
data TestProtocols3 = ReqResp1
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols3 where
  fromProtocolEnum ReqResp1 = 4

  toProtocolEnum 4 = Just ReqResp1
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols3 where
  maximumMessageSize ReqResp1  = defaultMiniProtocolLimit
  maximumIngressQueue ReqResp1 = defaultMiniProtocolLimit

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving Eq

instance Show DummyPayload where
    show d = printf "DummyPayload %d\n" (BL.length $ unDummyPayload d)

instance Arbitrary DummyPayload where
    arbitrary = do
       n <- choose (0, 128)
       len <- oneof [ return n
                    , return $ defaultMiniProtocolLimit - n - cborOverhead
                    , choose (0, defaultMiniProtocolLimit - cborOverhead)
                    ]
       p <- arbitrary
       let blob = BL.replicate len p
       return $ DummyPayload blob
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload

instance Serialise DummyPayload where
    encode a = CBOR.encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> CBOR.decodeBytes

data InvalidSDU = InvalidSDU {
      isTimestamp :: !Mx.RemoteClockModel
    , isIdAndMode :: !Word16
    , isLength    :: !Word16
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x\n"
                    (Mx.unRemoteClockModel $ isTimestamp a)
                    (isIdAndMode a)
                    (isLength a)

instance Arbitrary InvalidSDU where
    arbitrary = do
        ts  <- arbitrary
        mid <- choose (5, 0xffff) -- TxSubmission with 4 is the highest valid mid
        len <- arbitrary

        return $ InvalidSDU (Mx.RemoteClockModel ts) mid len


data MuxSTMCtx ptcl m = MuxSTMCtx {
      writeQueue :: TBQueue m BL.ByteString
    , readQueue  :: TBQueue m BL.ByteString
    , sduSize    :: Word16
    , traceQueue :: Maybe (TBQueue m (Mx.MiniProtocolId ptcl, Mx.MiniProtocolMode, Time m))
}

startMuxSTM :: ( MonadAsync m, MonadCatch m, MonadSay m, MonadSTM m, MonadThrow m, MonadTimer m
               , Ord ptcl, Enum ptcl, Bounded ptcl, Mx.ProtocolEnum ptcl, Show ptcl
               , Mx.MiniProtocolLimits ptcl )
            => [Mx.SomeVersion]
            -> (Mx.SomeVersion -> Maybe (Mx.MiniProtocolDescriptions ptcl m))
            -> Mx.MuxStyle
            -> TBQueue m BL.ByteString
            -> TBQueue m BL.ByteString
            -> Word16
            -> Maybe (TBQueue m (Mx.MiniProtocolId ptcl, Mx.MiniProtocolMode, Time m))
            -> m (TBQueue m (Maybe SomeException))
startMuxSTM versions mpds style wq rq mtu trace = do
    let ctx = MuxSTMCtx wq rq mtu trace
    resq <- atomically $ newTBQueue 10
    void $ async (spawn ctx resq)

    return resq
  where
    spawn ctx resq = do
        bearer <- queuesAsMuxBearer ctx
        res_e <- try $ Mx.muxStart versions mpds bearer style (Just $ rescb resq)
        case res_e of
             Left  e -> rescb resq $ Just e
             Right _ -> return ()

    rescb resq e_m = atomically $ writeTBQueue resq e_m


-- | Helper function to check if jobs in 'startMuxSTM' exited successfully.
-- Only checks the first return value
normallExit :: MonadSTM m
            => TBQueue m (Maybe SomeException)
            -> m Bool
normallExit resq = do
    res <- atomically $ readTBQueue resq
    case res of
         Just _  -> return False
         Nothing -> return True


queuesAsMuxBearer
  :: forall ptcl m.
     ( MonadSTM   m
     , MonadTimer m
     , MonadThrow m
     , Mx.ProtocolEnum ptcl
     )
  => MuxSTMCtx ptcl m
  -> m (MuxBearer ptcl m)
queuesAsMuxBearer ctx = do
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
          buf <- atomically $ readTBQueue (readQueue ctx)
          let (hbuf, payload) = BL.splitAt 8 buf
          case Mx.decodeMuxSDUHeader hbuf of
              Left  e      -> throwM e
              Right header -> do
                  ts <- getMonotonicTime
                  case traceQueue ctx of
                        Just q  -> atomically $ do
                            full <- isFullTBQueue q
                            if full then return ()
                                    else writeTBQueue q (Mx.msId header, Mx.msMode header, ts)
                        Nothing -> return ()
                  return (header {Mx.msBlob = payload}, ts)

      writeMux :: Mx.MuxSDU ptcl
               -> m (Time m)
      writeMux sdu = do
          ts <- getMonotonicTime
          let buf = Mx.encodeMuxSDU sdu -- XXX Timestamp isn't set
          atomically $ writeTBQueue (writeQueue ctx) buf
          return ts

      sduSizeMux :: m Word16
      sduSizeMux = return $ sduSize ctx

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

    (verify, client_mp, server_mp) <- setupMiniReqRsp
                                        (return ()) endMpsVar request response

    let client_mps, server_mps
          :: Mx.SomeVersion
          -> Maybe (Mx.MiniProtocolDescriptions TestProtocols1 IO)
        client_mps _ = Just (\_ -> client_mp)
        server_mps _ = Just (\_ -> server_mp)

    client_resVar <- startMuxSTM [version1] client_mps Mx.StyleClient client_w client_r sduLen Nothing
    server_resVar <- startMuxSTM [version1] server_mps Mx.StyleServer server_w server_r sduLen Nothing

    v <- verify
    c_e <- normallExit client_resVar
    s_e <- normallExit server_resVar
    return $ property $ v .&&. c_e .&&. s_e

-- | Create a verification function, a MiniProtocolDescription for the client side and a
-- MiniProtocolDescription for the server side for a RequestResponce protocol.
setupMiniReqRsp :: forall m ptcl. ( MonadST m, MonadSTM m, MonadThrow m )
                => m ()        -- | Action performed by responder before processing the response
                -> TVar m Int  -- | Total number of miniprotocols.
                -> DummyPayload -- | Request, sent from initiator.
                -> DummyPayload -- | Response, sent from responder after receive the request.
                -> m (m Bool
                      , Mx.MiniProtocolDescription ptcl m
                      , Mx.MiniProtocolDescription ptcl m)
setupMiniReqRsp serverAction mpsEndVar request response = do
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    let client_mp = Mx.MiniProtocolDescription (Just $ clientInit clientResultVar) Nothing
        server_mp = Mx.MiniProtocolDescription Nothing (Just $ serverRsp serverResultVar)

    return (verifyCallback serverResultVar clientResultVar, client_mp, server_mp)
  where
    verifyCallback serverResultVar clientResultVar = do
        (request', response') <- atomically $
            (,) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar
        return $ head request' == request && head response' == response

    plainServer :: [DummyPayload] -> ReqRespServer DummyPayload DummyPayload m [DummyPayload]
    plainServer reqs = ReqRespServer {
        recvMsgReq  = \req -> serverAction >> return (response, plainServer (req:reqs)),
        recvMsgDone = pure $ reverse reqs
    }

    plainClient :: [DummyPayload] -> ReqRespClient DummyPayload DummyPayload m [DummyPayload]
    plainClient = clientGo []

    clientGo resps []         = SendMsgDone (pure $ reverse resps)
    clientGo resps (req:reqs) =
      SendMsgReq req $ \resp ->
      return (clientGo (resp:resps) reqs)

    serverPeer = reqRespServerPeer (plainServer [])
    clientPeer = reqRespClientPeer (plainClient [request])

    clientInit clientResultVar clientChan = do
        result <- runPeer codecReqResp clientChan clientPeer
        atomically (putTMVar clientResultVar result)
        end

    serverRsp serverResultVar serverChan = do
        result <- runPeer codecReqResp serverChan serverPeer
        atomically (putTMVar serverResultVar result)
        end

    -- Wait on all miniprotocol jobs before letting a miniprotocol thread exit.
    end = do
        atomically $ modifyTVar' mpsEndVar (\a -> a - 1)
        atomically $ do
            c <- readTVar mpsEndVar
            unless (c == 0) retry

waitOnAllClients :: MonadSTM m => TVar m Int
                 -> Int
                 -> m ()
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

    client_res <- startMuxSTM [version0] (\_ -> Just client_mps) Mx.StyleClient client_w client_r sduLen Nothing
    server_res <- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r sduLen Nothing

    res0 <- verify_0
    res1 <- verify_1
    c_e <- normallExit client_res
    s_e <- normallExit server_res

    return $ property $ res0 .&&. res1 .&&. c_e .&&. s_e

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
    endMpsVar <- atomically $ newTVar 4          -- 2 active initoators and 2 active responders
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

    client_res <- startMuxSTM [version0] (\_ -> Just client_mps) Mx.StyleClient client_w client_r sduLen (Just traceQueueVar)
    server_res <- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r sduLen Nothing

    -- First verify that all messages where received correctly
    res_short <- verify_short
    res_long <- verify_long
    c_e <- normallExit client_res
    s_e <- normallExit server_res

    -- Then look at the message trace to check for starvation.
    trace <- atomically $ flushTBQueue traceQueueVar []
    let es = map (\(e, _, _) -> e) trace
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
    (client_w, resq) <- failingServer

    atomically $ writeTBQueue client_w (encodeInvalidMuxSDU badSdu)

    res <- atomically $ readTBQueue resq
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
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | Send a too short SDU header and verify that the correct exception is thrown.
prop_mux_short_header :: InvalidSDU
                      -> Word8
                      -> Property
prop_mux_short_header badSdu shortCutOffArg = ioProperty $ do
    let shortCutOff = fromIntegral $ min shortCutOffArg 7 -- XXX Quickcheck 'Giving up' hack
    (client_w, resq) <- failingServer

    atomically $ writeTBQueue client_w (BL.take shortCutOff $ encodeInvalidMuxSDU badSdu)

    res <- atomically $ readTBQueue resq
    case res of
         Just e  ->
             case fromException e of
                  Just me -> return $ Mx.errorType me == Mx.MuxDecodeError
                  Nothing -> return False
         Nothing -> return False

-- | Start a ReqResp server that is expected to fail, returns the server's read channel
failingServer :: forall m. ( MonadAsync m, MonadCatch m, MonadSay m, MonadST m, MonadSTM m
                           , MonadThrow m, MonadTimer m )
              => m (TBQueue m BL.ByteString, TBQueue m (Maybe SomeException))
failingServer  = do
    server_r <- atomically $ newTBQueue 10
    server_w <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let sduLen = 1260
        request  = DummyPayload $ BL.replicate 4 0xa
        response = request

    (_, _, server_mp) <- setupMiniReqRsp (return ()) endMpsVar request response
    let server_mps ChainSync1 = server_mp
    server_res <- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r sduLen Nothing

    return (server_r, server_res)


