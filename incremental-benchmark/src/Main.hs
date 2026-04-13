{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes         #-}

module Main where

import Debug.Trace qualified as DT

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as Read
import Codec.CBOR.Write qualified as Write

import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.ST (ST)
import Control.Tracer
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Word
import System.Environment

import Cardano.Chain.Slotting (EpochSlots (..))
import Ouroboros.Consensus.Cardano.Block
    (CodecConfig, StandardCrypto, CardanoBlock)
import Ouroboros.Consensus.Cardano.Node (protocolClientInfoCardano)
import Ouroboros.Consensus.Node.ProtocolInfo (pClientInfoCodecConfig)
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common
    (SerialiseHFC(decodeDiskHfcBlock))
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Protocol.Codec.Utils

import Network.Mux as Mx
import Network.Mux.Bearer as Mx
import Network.Socket as Socket

import Test.Mux.ReqResp
-- import Data.Functor.Contravariant ((>$<))

serverAddr :: SockAddr
serverAddr = SockAddrInet 1234 $ Socket.tupleToHostAddress (10, 0, 0, 1)

codecConfig :: CodecConfig (CardanoBlock StandardCrypto)
codecConfig = pClientInfoCodecConfig . protocolClientInfoCardano $ EpochSlots 21600

largeBlockMultiplier :: Int
largeBlockMultiplier = 150

type Marshall = MsgReqResp () Response -> LBS.ByteString
type UnMarshall =
   ByteChannel IO ->
   Maybe LBS.ByteString ->
   IO (Either Read.DeserialiseFailure (MsgReqResp () Response, Maybe LBS.ByteString))

data Response = BlockCBOR !ByteString | Block !(CardanoBlock StandardCrypto)
  deriving Show


startServerMiniProtocol
  :: Mux ResponderMode IO
  -> Word16
  -> Int
  -> Marshall
  -> UnMarshall
  -> ByteString
  -> IO ()
startServerMiniProtocol mux num times encode' decode' block = do
    void $ runMiniProtocol mux
      (MiniProtocolNum num) ResponderDirectionOnly
      StartOnDemand (\chan ->
                      runServerBurst encode' decode'
                                     nullTracer chan serverReqResp
                    )
  where
    serverReqResp :: ReqRespServerBurst () Response IO Int
    serverReqResp = ReqRespServerBurst $ \_ -> pure (go times)
      where
        go n | n > 0 = SendMsgResp (BlockCBOR block) (pure $ go (n - 1))
             | otherwise = SendMsgDoneServer . pure $ fromIntegral num

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> do
      block <- BS.readFile "183867428_53d0a6c09e4ce48c221f24a31e64eb3331c7952d886be8e950f7194012fea009_raw.cbor"
      let largeBlock = mconcat . replicate largeBlockMultiplier $ block
      sock <- socket AF_INET Stream defaultProtocol
      setSocketOption sock ReuseAddr 1
      bind sock serverAddr
      listen sock 1
      forConcurrently_ [1..2] $ \_ -> do
        (sock',_) <- accept sock
        bearer <- getBearer makeSocketBearer 1.0 sock' Nothing
        serverMux <- Mx.new nullTracers (protocols ResponderDirectionOnly)
        startServerMiniProtocol serverMux 3 20   (encodeNonIncremental "") (decodeNonIncremental "") block
        startServerMiniProtocol serverMux 4 400 (encodeIncremental "") (decodeIncremental 1 "") block
        startServerMiniProtocol serverMux 1 20 (encodeNonIncremental "") (decodeNonIncremental "") block
        startServerMiniProtocol serverMux 2 20 (encodeIncremental "") (decodeIncremental 1 "") block
        startServerMiniProtocol serverMux 11 1 (encodeNonIncremental "") (decodeNonIncremental "") largeBlock
        startServerMiniProtocol serverMux 12 1 (encodeIncremental "") (decodeIncremental largeBlockMultiplier "") largeBlock
        Mx.run serverMux bearer
    ["client"] -> do
      let action = concurrently_ client competitor -- in action

          mkRunProtocol mux experiment n encode' decode' =
            void . atomically =<<
              runMiniProtocol mux (MiniProtocolNum n) InitiatorDirectionOnly
                              StartEagerly
                              (\chan ->
                                 let clientReqResp :: ReqRespClientBurst () Response IO ()
                                     clientReqResp = DT.traceEvent ("DURATION " ++ experiment ++ " client") $ SendMsgReqBurst () loop
                                     handler !_blk = do
                                       DT.traceEventIO $ "STEP " ++ experiment ++ " client recv wait"
                                       pure loop
                                     loop = AwaitResp { handleMsgDone = DT.traceEventIO ("END " ++ experiment)
                                                      , handleMsgResp = handler }
                                  in runClientBurst (encode' experiment) (decode' experiment) nullTracer chan clientReqResp
                              )
          client = do
            sock <- Socket.socket AF_INET Stream defaultProtocol
            Socket.connect sock serverAddr
            withReadBufferIO $ \buf -> do
              bearer <- getBearer Mx.makeSocketBearer 1.0 sock buf
              let tracers = nullTracers --TracersI nullTracer nullTracer (show >$< stdoutTracer)
              clientMux <- Mx.new tracers (protocols InitiatorDirectionOnly)
              void . forkIO $ Mx.run clientMux bearer

              let mkRun = mkRunProtocol clientMux
                  runWarmup = mkRun "warmup" 3 encodeNonIncremental decodeNonIncremental
                  runNonIncremental = mkRun "small-nonincremental" 1 encodeNonIncremental decodeNonIncremental
                  runIncremental    = mkRun "small-incremental" 2 encodeIncremental (decodeIncremental 1)
                  runLargeNonIncremental = mkRun "large-nonincremental" 11 encodeNonIncremental decodeNonIncremental
                  runLargeIncremental = mkRun "large-incremental" 12 encodeIncremental (decodeIncremental largeBlockMultiplier)

              runWarmup
              runNonIncremental
              runIncremental
              runLargeNonIncremental
              runLargeIncremental

          -- this application runs on a seperate capability. Its purpose
          -- is to compete for heap allocations of bytestrings to cause
          -- some fragmentation. It uses half the bandwidth in the benchmark.
          competitor = do
            sock <- Socket.socket AF_INET Stream defaultProtocol
            Socket.connect sock serverAddr
            let tracers = nullTracers --TracersI nullTracer nullTracer (show >$< stdoutTracer)
            mux <- Mx.new tracers (protocols InitiatorDirectionOnly)
            bearer <- getBearer Mx.makeSocketBearer 1.0 sock Nothing
            void . forkIO $ Mx.run mux bearer

            mkRunProtocol mux "competitor" 4 encodeIncremental (decodeIncremental 1)

      action

    _otherwise -> error "usage: incremental-benchmark (client | server)\nreceived: "


protocols :: MiniProtocolDirection mode -> [MiniProtocolInfo mode]
protocols miniProtocolDir =
  [ MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 1,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Just 1
    },
    MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 2,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Just 1
    },
    MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 3,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Nothing
    },
    MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 4,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Just 0
    },
    MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 11,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Just 1
    },
    MiniProtocolInfo {
      miniProtocolNum        = MiniProtocolNum 12,
      miniProtocolDir,
      miniProtocolLimits     = defaultProtocolLimits,
      miniProtocolCapability = Just 1
    }
  ]

defaultProtocolLimits :: MiniProtocolLimits
defaultProtocolLimits =
    MiniProtocolLimits {
      maximumIngressQueue = 10_000_000
    }


encodeNonIncremental, encodeIncremental :: String -> MsgReqResp () Response -> LBS.ByteString
encodeNonIncremental experiment msg = Write.toLazyByteString cbor
  where
    cbor = case msg of
      MsgReq () -> DT.traceEvent ("STEP " ++ experiment ++ " encoder recv wait") $ CBOR.encodeListLen 1 <> CBOR.encodeWord 0
      MsgResp resp
        | BlockCBOR bs <- resp -> CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encodeBytes bs
        | otherwise -> error "impossible"
      MsgDone -> CBOR.encodeListLen 1 <> CBOR.encodeWord 2

encodeIncremental _experiment msg = Write.toLazyByteString cbor
  where
    cbor = case msg of
      MsgReq () -> CBOR.encodeListLen 1 <> CBOR.encodeWord 0
      MsgResp resp
        | BlockCBOR bs <- resp -> CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> CBOR.encodePreEncoded bs
        | otherwise -> error "impossible"
      MsgDone -> CBOR.encodeListLen 1 <> CBOR.encodeWord 2


decodeNonIncremental :: String -> UnMarshall
decodeNonIncremental experiment channel trailing = runCBORDecoderWithChannel channel trailing decoder
  where
    decoder = do
      len <- CBOR.decodeListLen
      tag <- CBOR.decodeWord
      case (len, tag) of
        (1, 0) -> pure $ MsgReq ()
        (2, 1) -> do
          bs0 <- BS.fromStrict <$> CBOR.decodeBytes
          let loop bs =
                case Read.deserialiseFromBytes (decodeDiskHfcBlock codecConfig) bs of
                  Left (Read.DeserialiseFailure _ reason) -> fail reason
                  Right (trailing', mkA)
                    | not (LBS.null trailing') ->
                        loop trailing'
                    | otherwise                ->
                        case mkA bs of
                          Left err  -> fail $ show err
                          Right res -> DT.traceEvent ("DURATION " ++ experiment ++ " decoder") $ pure $! MsgResp $! Block res
          DT.traceEvent ("STEP " ++ experiment ++ " decoder run cbor " ++ show (LBS.length bs0) <> " bytes") $ loop bs0
        (1, 2) -> pure MsgDone
        _      -> fail $ "decode MsgReqResp: unknown tag " ++ show tag


newtype Annotator a = Annotator { runAnnotator :: LBS.ByteString -> a }

decodeIncremental :: Int -> String -> UnMarshall
decodeIncremental times experiment channel trailing = runCBORAnnDecoderWithChannel experiment channel trailing decoder
  where
    decoder = do
      len <- CBOR.decodeListLen
      tag <- CBOR.decodeWord
      case (len, tag) of
        (1, 0) -> pure . Annotator $ \_ -> MsgReq ()
        (2, 1) -> do
          let loop times'
                | times' > 1 = decodeDiskHfcBlock codecConfig >> loop (times' - 1)
                | otherwise = do
                    startOffset <- CBOR.peekByteOffset
                    fblock <- decodeDiskHfcBlock codecConfig
                    endOffset <- CBOR.peekByteOffset
                    DT.traceEvent ("DURATION " ++ experiment ++ " decoder") $
                      pure . Annotator $ \bs ->
                      case fblock (bytesBetweenOffsets startOffset endOffset bs) of
                        Left err -> error $ show err
                        Right block -> MsgResp $! Block block
          loop times
        (1, 2) -> pure . Annotator $ \_ -> MsgDone
        _      -> fail $ "decode MsgReqResp: unknown tag " ++ show tag


runCBORAnnDecoderWithChannel
  :: forall a.
     String
  -> ByteChannel IO
  -> Maybe LBS.ByteString
  -> (forall s. CBOR.Decoder s (Annotator a))
  -> IO (Either Read.DeserialiseFailure (a, Maybe LBS.ByteString))

runCBORAnnDecoderWithChannel experiment Channel{recv} trailing decoder =
    stToIO (Read.deserialiseIncremental decoder) >>= go' stToIO [] trailing
  where
    go' :: (ST s (Read.IDecode s (Annotator a)) -> IO (Read.IDecode s (Annotator a)))
        -> [LBS.ByteString]
        -> Maybe LBS.ByteString
        -> Read.IDecode s (Annotator a)
        -> IO (Either Read.DeserialiseFailure (a, Maybe LBS.ByteString))
    go' stToIO' = go
      where
        go !bytes (Just trailing') (Read.Partial k) =
          stToIO' (k (Just (BS.toStrict trailing'))) >>= go (trailing' : bytes) Nothing
        go !bytes Nothing         (Read.Partial k) =
          DT.traceEvent ("STEP " ++ experiment ++ " decoder recv wait") recv >>= \bs -> do
            let (chunks :: Maybe Int) = LBS.foldlChunks (\count _chunk -> count + 1) 0 <$> bs
            -- putStrLn $ "STEP " ++ experiment ++ " # of chunks = " <> show chunks
            DT.traceEventIO $ "STEP " ++ experiment ++ " decoder run cbor " ++ show (LBS.length <$> bs) <> " bytes (or maybe less)"
            decoded <- stToIO' (k (fmap BS.toStrict bs))
            go (maybe bytes (: bytes) bs) Nothing decoded
        go !bytes _ (Read.Done trailing' _ f) = do
          let trailing'' | BS.null trailing' = Nothing
                         | otherwise = Just $ BS.fromStrict trailing'
              !result = runAnnotator f $ mconcat (reverse bytes)
          return $ Right (result, trailing'')
        go _bytes _ (Read.Fail _ _ failure)   =
          return $ Left failure
