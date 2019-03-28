{-# LANGUAGE BangPatterns        #-}
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
import           Codec.CBOR.Write (toLazyByteString)
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
import           Control.Monad.IOSim (runSimStrictShutdown)
import           Network.TypedProtocol.Channel (recv)
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
  [ testProperty "mux send receive" prop_mux_snd_recv
  , testProperty "2 miniprotocols"  prop_mux_2_minis
  , testProperty "starvation"       prop_mux_starvation
  , testProperty "demuxing (Sim)"   prop_demux_sdu_sim
  , testProperty "demuxing (IO)"    prop_demux_sdu_io
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

smallMiniProtocolLimit :: Int64
smallMiniProtocolLimit = 16*1024

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
-- Allows to run a single chain-sync protocol with a very small message size
--
data TestProtocolsSmall = ChainSyncSmall
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocolsSmall where
  fromProtocolEnum ChainSyncSmall = 2

  toProtocolEnum 2 = Just ChainSyncSmall
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocolsSmall where
  maximumMessageSize ChainSyncSmall  = smallMiniProtocolLimit
  maximumIngressQueue ChainSyncSmall = smallMiniProtocolLimit

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
       n <- choose (1, 128)
       len <- oneof [ return n
                    , return $ defaultMiniProtocolLimit - n - cborOverhead
                    , choose (1, defaultMiniProtocolLimit - cborOverhead)
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
      isTimestamp  :: !Mx.RemoteClockModel
    , isIdAndMode  :: !Word16
    , isLength     :: !Word16
    , isRealLength :: !Int64
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x\n"
                    (Mx.unRemoteClockModel $ isTimestamp a)
                    (isIdAndMode a)
                    (isLength a)


data ArbitrarySDU = ArbitraryInvalidSDU InvalidSDU Mx.MuxBearerState Mx.MuxErrorType
                  | ArbitraryValidSDU DummyPayload Mx.MuxBearerState (Maybe Mx.MuxErrorType)
                  deriving Show

instance Arbitrary ArbitrarySDU where
    arbitrary = oneof [ unknownMiniProtocol
                      , invalidLenght
                      , validSdu
                      , tooLargeSdu
                      ]
      where
        validSdu = do
            b <- arbitrary

            -- Valid SDUs before version negotiation does only make sense for single SDUs.
            state <- if BL.length (unDummyPayload b) < 0xffff
                         then arbitrary
                         else return Mx.Mature
            let err_m = if state == Mx.Larval || state == Mx.Connected
                            then Just Mx.MuxUnknownMiniProtocol
                            else Nothing

            return $ ArbitraryValidSDU b state err_m

        tooLargeSdu = do
            l <- choose (1 + smallMiniProtocolLimit , 2 * smallMiniProtocolLimit)
            p <- arbitrary
            let b = DummyPayload $ BL.replicate l p

            return $ ArbitraryValidSDU b Mx.Mature (Just Mx.MuxIngressQueueOverRun)

        unknownMiniProtocol = do
            ts  <- arbitrary
            mid <- choose (6, 0xffff) -- ClientChainSynWithBlocks with 5 is the highest valid mid
            len <- arbitrary
            state <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid len
                                          (8 + fromIntegral len))
                                         state
                                         Mx.MuxUnknownMiniProtocol
        invalidLenght = do
            ts  <- arbitrary
            mid <- arbitrary
            len <- arbitrary
            realLen <- choose (0, 7) -- Size of mux header is 8
            state <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid len realLen)
                                         state
                                         Mx.MuxDecodeError

instance Arbitrary Mx.MuxBearerState where
     arbitrary = elements [ Mx.Larval
                          , Mx.Connected
                          , Mx.Mature
                          , Mx.Dying
                          , Mx.Dead
                          ]

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
            -> m (Async m (Maybe SomeException))
startMuxSTM versions mpds style wq rq mtu trace = do
    let ctx = MuxSTMCtx wq rq mtu trace
    resq <- atomically $ newTBQueue 10
    async (spawn ctx resq)

  where
    spawn ctx resq = do
        bearer <- queuesAsMuxBearer ctx
        res_e <- try $ Mx.muxStart versions mpds bearer style (Just $ rescb resq)
        case res_e of
             Left  e -> return $ Just e
             Right _ -> atomically $ readTBQueue resq

    rescb resq e_m = atomically $ writeTBQueue resq e_m

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

    caid <- startMuxSTM [version1] client_mps Mx.StyleClient client_w client_r sduLen Nothing
    said <- startMuxSTM [version1] server_mps Mx.StyleServer server_w server_r sduLen Nothing

    r <- waitBoth caid said
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> property <$> verify

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

    caid <- startMuxSTM [version0] (\_ -> Just client_mps) Mx.StyleClient client_w client_r sduLen Nothing
    said<- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r sduLen Nothing

    r <- waitBoth caid said
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> do
             res0 <- verify_0
             res1 <- verify_1

             return $ property $ res0 .&&. res1

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

    caid <- startMuxSTM [version0] (\_ -> Just client_mps) Mx.StyleClient client_w client_r sduLen (Just traceQueueVar)
    said <- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r sduLen Nothing

    r <- waitBoth caid said
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> do
             -- First verify that all messages where received correctly
             res_short <- verify_short
             res_long <- verify_long

             -- Then look at the message trace to check for starvation.
             trace <- atomically $ flushTBQueue traceQueueVar []
             let es = map (\(e, _, _) -> e) trace
                 ls = dropWhile (\e -> e == head es) es
                 fair = verifyStarvation ls

             return $ property $ res_short .&&. res_long .&&. fair

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

encodeInvalidMuxSDU :: InvalidSDU -> BL.ByteString
encodeInvalidMuxSDU sdu =
    let header = Bin.runPut enc in
    BL.append header $ BL.replicate (fromIntegral $ isLength sdu) 0xa
  where
    enc = do
        Bin.putWord32be $ Mx.unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | Verify ingress processing of valid and invalid SDUs.
prop_demux_sdu :: forall m.
                    ( MonadAsync m
                    , MonadCatch m
                    , MonadSay m
                    , MonadST m
                    , MonadSTM m
                    , MonadTimer m)
                 => ArbitrarySDU
                 -> m Property
prop_demux_sdu a = do
    r <- run a
    return $ tabulate "SDU type" [stateLabel a] $
             tabulate "SDU Violation " [violationLabel a] r

  where
    run (ArbitraryValidSDU sdu state (Just Mx.MuxIngressQueueOverRun)) = do
        stopVar <- newEmptyTMVarM

        let server_mp = Mx.MiniProtocolDescription Nothing (Just $ serverRsp stopVar)
        -- To trigger MuxIngressQueueOverRun we use a special test protocol with
        -- an ingress queue which is less than 0xffff so that it can be triggered by a
        -- single segment.
        let server_small ChainSyncSmall = server_mp

        (client_w, said) <- plainServer server_small
        setup state client_w

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === Mx.MuxIngressQueueOverRun
                    Nothing -> return $ property False
            Nothing -> return $ property False

    run (ArbitraryValidSDU sdu state err_m) = do
        stopVar <- newEmptyTMVarM

        let server_x = Mx.MiniProtocolDescription Nothing (Just $ serverRsp stopVar)
        let server_std ChainSync1 = server_x

        (client_w, said) <- plainServer server_std

        setup state client_w

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ Mx.errorType me === err
                                    Nothing  -> return $ property False
                    Nothing -> return $ property False
            Nothing -> return $ err_m === Nothing

    run (ArbitraryInvalidSDU badSdu state err) = do
        stopVar <- newEmptyTMVarM

        let server_x = Mx.MiniProtocolDescription Nothing (Just $ serverRsp stopVar)
        let server_std ChainSync1 = server_x

        (client_w, said) <- plainServer server_std

        setup state client_w
        atomically $ writeTBQueue client_w $ BL.take (isRealLength badSdu) $ encodeInvalidMuxSDU badSdu
        atomically $ putTMVar stopVar BL.empty

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === err
                    Nothing -> return $ property False
            Nothing -> return $ property False

    plainServer server_mps = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        said <- startMuxSTM [version0] (\_ -> Just server_mps) Mx.StyleServer server_w server_r 1280 Nothing

        return (server_r, said)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | e == BL.empty = return ()
        loop e = do
            msg_m <- recv chan
            case msg_m of
                 Just msg ->
                     let e_m = BL.stripPrefix msg e in
                     case e_m of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"

    writeSdu _ payload | payload == BL.empty = return ()
    writeSdu queue payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu' = Mx.MuxSDU (Mx.RemoteClockModel 0) (Mx.AppProtocolId ChainSyncSmall)
                              Mx.ModeInitiator
                             (fromIntegral $ BL.length frag) frag
            !pkt = Mx.encodeMuxSDU (sdu' :: Mx.MuxSDU TestProtocolsSmall)

        atomically $ writeTBQueue queue pkt
        writeSdu queue rest

    -- Unless we are in Larval or Connected we fake version negotiation before
    -- we run the test.
    setup state q | state /= Mx.Larval && state /= Mx.Connected = do
        let msg = Mx.MsgInitReq [version0]
            blob = toLazyByteString $ Mx.encodeControlMsg msg
            pkt = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol Mx.ModeInitiator
                            (fromIntegral $ BL.length blob) blob
        atomically $ writeTBQueue q $ Mx.encodeMuxSDU (pkt :: Mx.MuxSDU TestProtocols1)
        return ()
    setup _ _ = return ()

    stateLabel (ArbitraryInvalidSDU _ state _) = "Invalid " ++ versionLabel state
    stateLabel (ArbitraryValidSDU _ state _)   = "Valid " ++ versionLabel state

    versionLabel Mx.Larval    = "before version negotiation"
    versionLabel Mx.Connected = "before version negotiation"
    versionLabel _            = "after version negotiation"

    violationLabel (ArbitraryValidSDU _ _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ _ err) = sduViolation $ Just err

    sduViolation (Just Mx.MuxUnknownMiniProtocol) = "unknown miniprotocol"
    sduViolation (Just Mx.MuxDecodeError        ) = "decode error"
    sduViolation (Just Mx.MuxIngressQueueOverRun) = "ingress queue overrun"
    sduViolation (Just _                        ) = "unknown violation"
    sduViolation Nothing                          = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                     -> Property
prop_demux_sdu_sim badSdu =
    let r_e =  runSimStrictShutdown $ prop_demux_sdu badSdu in
    case r_e of
         Left  _ -> property False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                    -> Property
prop_demux_sdu_io badSdu = ioProperty $ prop_demux_sdu badSdu

