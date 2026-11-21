{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Production progress-adapter checks using staged CBOR and ordinary mux runners.
module Test.Ouroboros.Network.Driver.Progress (tests) where

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.Strict qualified as Strict
import Control.DeepSeq (NFData (rnf))
import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM, unless, void, when)
import Control.Monad.Class.MonadAsync (cancel, concurrently, link, withAsync)
import Control.Monad.Class.MonadST (stToIO)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time, getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Tracer (Tracer, mkTracer, nullTracer)
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Word (Word64)

import Codec.CBOR.Decoding qualified as CBOR
import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.CBOR (convertCborDecoderLBS)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver (Driver (..), runPipelinedPeerWithDriver)
import Network.TypedProtocol.Peer
import Network.TypedProtocol.ReqResp.Type

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Limits
import Ouroboros.Network.Driver.Progress
import Ouroboros.Network.Driver.Simple (DecoderFailure)
import Ouroboros.Network.Mux (MiniProtocolCb (..))
import Ouroboros.Network.Protocol.Codec.Utils qualified as Utils

import Network.Mux qualified as Mux
import Network.Mux.Bearer.Queues (QueueChannel (..), queueChannelAsBearer)
import Network.Mux.Timeout (TimeoutFn, withTimeoutSerial)
import Network.Mux.Types (Bearer (..))

import Test.Ouroboros.Network.Orphans ()

import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests = testGroup "Progress adapter"
    [ testProperty "complete elements precede the tail and retain their scope" $
        once $ ioProperty $ bounded fragmented
    , testProperty "coalesced replies retain separate scopes and original bytes" $
        once $ ioProperty $ bounded coalesced
    , testProperty "annotated driver preserves read times across trailing replies" $
        once $ ioProperty $ bounded annotatedReadTimes
    , testProperty "blocked sends are timestamped only after send returns" $
        once $ ioProperty $ bounded blockedSend
    , testProperty "a malformed tail aborts the original scope after useful progress" $
        once $ ioProperty $ bounded malformedTail
    , testProperty "generated fragmentation and empty chunks preserve elements and annotations" $
        forAll (listOf (chooseInt (0, 8))) $ \cuts -> ioProperty $ bounded (fragmentation cuts)
    , testProperty "message, protocol, element, count and input bounds precede callbacks" $
        once $ ioProperty $ bounded progressLimits
    , testProperty "invalid and unobserved ranges cannot reach the consumer" $
        once $ ioProperty $ bounded invalidRanges
    , testProperty "callback exceptions abort once while retaining accepted prefixes" $
        once $ ioProperty $ bounded callbackFailures
    , testProperty "async interruption cleans up every receive boundary once" $
        once $ ioProperty $ bounded asyncBoundaries
    , testProperty "the existing fatal timeout aborts a stalled response" $
        once $ ioProperty $ bounded fatalTimeout
    , testProperty "a retained emitter cannot revive a completed scope" $
        once $ ioProperty $ bounded lateEmitter
    , testProperty "cancelling a blocked send produces no completion observation" $
        once $ ioProperty $ bounded cancelledSend
    , testProperty "no-element contexts retain final messages and terminal accounting" $
        once $ ioProperty $ bounded noElementProgress
    , testProperty "input and await hooks bracket fragmented decoding" $
        once $ ioProperty $ bounded inputBoundaries
    , testProperty "empty reads and EOF never acquire input-publication protection" $
        once $ ioProperty $ bounded emptyPublication
    , testProperty "shared byte-span annotations slice the element and retain the full input origin" $
        once $ property byteSpanAnnotations
    , testProperty "bounded limits widen before sums and native-word narrowing" $
        once $ ioProperty $ bounded fixedWidthBounds
    ]
  where
    bounded action = do
      result <- timeout 5 action
      pure $ case result of
        Nothing -> counterexample "progress probe timed out" False
        Just p  -> p

-- 'Batch' deliberately retains an annotation so the test catches re-encoding
-- instead of preserving the message's actual (possibly non-canonical) bytes.
data Batch = Batch [Word] BL.ByteString
  deriving (Eq, Show)

instance NFData Batch where
    rnf (Batch values raw) = rnf values `seq` rnf raw

type BatchProtocol = ReqResp () Batch
type Emitter = Word64 -> Word64 -> Word -> IO ()

progressValue :: ProgressEvent a -> a
progressValue event = let WithByteSpan (value, _, _) = progressElement event in value

progressStart :: ProgressEvent a -> Int64
progressStart event = let WithByteSpan (_, start, _) = progressElement event in start

progressEnd :: ProgressEvent a -> Int64
progressEnd event = let WithByteSpan (_, _, end) = progressElement event in end

-- This compiles the proposed factory shape: the receiver installs a scope,
-- decode snapshots it once, and each monadic continuation closes over it.
-- CBOR's ST decoder handles one syntactic stage at a time. Effects occur
-- between stages, without changing the core DecodeStep or Peer interpreter.
progressCodec :: IO Emitter -> AnnotatedCodec BatchProtocol CBOR.DeserialiseFailure IO BL.ByteString
progressCodec getEmitter = Codec
    { encode = \case
        MsgReq () -> CBOR.toLazyByteString $ CBOR.encodeListLen 1 <> CBOR.encodeWord 0
        MsgResp (Batch values _) -> encodeReply values
        MsgDone -> CBOR.toLazyByteString $ CBOR.encodeListLen 1 <> CBOR.encodeWord 2
    , decode = \case
        SingBusy -> do
          emit <- getEmitter
          header <- stage $ do
            outer <- CBOR.decodeListLen
            tag <- CBOR.decodeWord
            unless (outer == 2 && tag == 1) $ fail "invalid batch envelope"
            count <- CBOR.decodeListLen
            unless (count > 0 && count <= 8) $ fail "invalid fixture batch count"
            pos <- CBOR.peekByteOffset
            pure (count, fromIntegral pos)
          bindStep header $ \(count, pos) trailing -> items emit count pos [] trailing
        SingIdle -> stage $ do
          _ <- CBOR.decodeListLenOf 1
          tag <- CBOR.decodeWord
          case tag of
            0 -> pure $ Annotator $ \_ -> SomeMessage $ MsgReq ()
            2 -> pure $ Annotator $ \_ -> SomeMessage MsgDone
            _ -> fail "invalid idle message"
        tok@SingDone -> notActiveState tok
    }
  where
    items :: Emitter -> Int -> Word64 -> [Word] -> Maybe BL.ByteString
          -> IO (DecodeStep BL.ByteString CBOR.DeserialiseFailure IO
                    (Annotator BL.ByteString (StBusy :: BatchProtocol)))
    items emit count pos acc trailing = do
      next <- stage (Utils.decodeWithByteSpan CBOR.decodeWord)
      supplied <- supply trailing next
      bindStep supplied $ \(WithByteSpan (value, start, localEnd)) rest -> do
        -- Each fresh staged decoder has its own byte-offset origin.
        let end = pos + fromIntegral localEnd
        emit (pos + fromIntegral start) end value
        if count == 1
          then pure $ DecodeDone
                 (Annotator $ \raw -> SomeMessage $ MsgResp $ Batch (reverse (value : acc)) raw)
                 rest
          else items emit (count - 1) end (value : acc) rest

    stage :: (forall s. CBOR.Decoder s a) -> IO (DecodeStep BL.ByteString CBOR.DeserialiseFailure IO a)
    stage decoder = convertCborDecoderLBS decoder stToIO

-- Sequence independently parsed CBOR stages, preserving both lazy chunks and
-- trailing bytes. This is the small missing combinator being tested, rather
-- than a change to the typed-protocols decoder algebra.
bindStep :: DecodeStep bytes failure IO a
         -> (a -> Maybe bytes -> IO (DecodeStep bytes failure IO b))
         -> IO (DecodeStep bytes failure IO b)
bindStep (DecodeDone value trailing) k = k value trailing
bindStep (DecodeFail failure) _ = pure $ DecodeFail failure
bindStep (DecodePartial more) k = pure $ DecodePartial $ \input -> more input >>= (`bindStep` k)

supply :: Maybe bytes -> DecodeStep bytes failure IO a -> IO (DecodeStep bytes failure IO a)
supply Nothing step = pure step
supply (Just input) (DecodePartial more) = more $ Just input
supply (Just _) _ = error "fixture stage unexpectedly completed without input"

encodeReply :: [Word] -> BL.ByteString
encodeReply values = CBOR.toLazyByteString $
    CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <>
    CBOR.encodeListLen (fromIntegral $ length values) <> foldMap CBOR.encodeWord values

-- A real pipelined receiver installs each immutable scope immediately before
-- ReceiverAwait. Collected final messages use the ordinary typed-message path.
twoRequests :: (String -> IO ())
            -> Peer BatchProtocol AsClient (Pipelined Z Batch) StIdle IO [Batch]
twoRequests install =
    YieldPipelined ReflClientAgency (MsgReq ()) (receiver "A") $
    YieldPipelined ReflClientAgency (MsgReq ()) (receiver "B") $
    Collect Nothing $ \first ->
    Collect Nothing $ \second ->
    Yield ReflClientAgency MsgDone $
    Done ReflNobodyAgency [first, second]
  where
    receiver :: String -> Receiver BatchProtocol AsClient StBusy StIdle IO Batch
    receiver name = ReceiverEffect $ do
      install name
      pure $ ReceiverAwait ReflServerAgency $ \(MsgResp response) -> ReceiverDone response

runFixture :: IO Emitter -> (String -> IO ()) -> Channel IO BL.ByteString -> IO [Batch]
runFixture getEmitter install channel = withTimeoutSerial $ \timeoutFn -> do
    let getScope = do
          emitter <- getEmitter
          pure quietScope { receiveProgress = \event -> emitter
            (fromIntegral (progressStart event))
            (fromIntegral (progressEnd event)) (progressValue event) }
        driver = fixtureDriver timeoutFn getScope nullTracer (const $ pure ()) channel
    (result, state) <- runPipelinedPeerWithDriver driver (PeerPipelined $ twoRequests install)
    unless (maybe True BL.null (progressTrailingBytes state)) $ fail "unexpected final trailing input"
    pure result

fragmented :: IO Property
fragmented = do
    input <- newTQueueIO
    events <- newTVarIO []
    installed <- newTVarIO (\_ _ _ -> pure ())
    sawPartial <- newEmptyTMVarIO
    firstElement <- newEmptyTMVarIO
    recvCount <- newTVarIO (0 :: Int)
    let emitter scope _ pos value = do
          atomically $ modifyTVar events (++ [(scope, pos, value)])
          -- Deliberately replace mutable context during an in-flight decode.
          -- The second element must still use the originally captured emitter.
          if scope == "A" && value == 24
            then atomically $ do
              writeTVar installed (emitter "replacement")
              void $ tryPutTMVar firstElement ()
            else pure ()
        install name = atomically $ writeTVar installed (emitter name)
        channel = Channel
          { send = const $ pure ()
          , recv = do
              atomically $ do
                n <- readTVar recvCount
                writeTVar recvCount (n + 1)
                if n == 1 then void $ tryPutTMVar sawPartial () else pure ()
              atomically $ Just <$> readTQueue input
          }
        producer = do
          -- Header plus the first byte of an incomplete CBOR word.
          atomically $ writeTQueue input $ BL.pack [0x82, 0x01, 0x82, 0x18]
          atomically $ readTMVar sawPartial
          before <- atomically $ readTVar events
          atomically $ writeTQueue input $ BL.singleton 0x18
          -- The tail is withheld until the complete-element callback occurs.
          atomically $ readTMVar firstElement
          atomically $ writeTQueue input $
            BL.pack [0x18, 0x19] <> encodeReply [3]
          pure before
    -- Use the same structured concurrency implementation as the real runner.
    (before, replies) <- concurrently producer $
      runFixture (atomically $ readTVar installed) install channel
    observed <- atomically $ readTVar events
    pure $ counterexample (show (before, observed, replies)) $ conjoin
      [ before === []
      , observed === [("A", 5, 24), ("A", 7, 25), ("B", 4, 3)]
      , replies === [Batch [24, 25] (encodeReply [24, 25]), Batch [3] (encodeReply [3])]
      ]

coalesced :: IO Property
coalesced = do
    -- Encode 1 with a longer valid CBOR representation. A re-encoded annotation
    -- would lose the extra byte. Both replies arrive through one channel read.
    let first = BL.pack [0x82, 0x01, 0x81, 0x18, 0x01]
        second = encodeReply [7, 8]
    events <- newTVarIO []
    installed <- newTVarIO (\_ _ _ -> pure ())
    readCount <- newTVarIO (0 :: Int)
    let install scope = atomically $ writeTVar installed $ \_ pos value ->
          atomically $ modifyTVar events (++ [(scope, pos, value)])
        channel = Channel
          { send = const $ pure ()
          , recv = atomically $ do
              n <- readTVar readCount
              writeTVar readCount (n + 1)
              pure $ if n == 0 then Just (first <> second) else Nothing
          }
    replies <- runFixture (atomically $ readTVar installed) install channel
    observed <- atomically $ readTVar events
    count <- atomically $ readTVar readCount
    pure $ counterexample (show (observed, replies, count)) $ conjoin
      [ count === 1
      , observed === [("A", 5, 1), ("B", 4, 7), ("B", 5, 8)]
      , replies === [Batch [1] first, Batch [7, 8] second]
      ]

productionCodec :: ProgressEmitter IO Word
                -> AnnotatedCodec BatchProtocol CBOR.DeserialiseFailure IO BL.ByteString
productionCodec emit = progressCodec $ pure $ \start end value ->
    emit (WithByteSpan (value, fromIntegral start, fromIntegral end))

quietScope :: ReceiveScope IO Word
quietScope = ReceiveScope
    { receiveLimits = ProgressLimits 1024 512 8
    , receiveStarted = pure ()
    , receiveInputStarting = pure ()
    , receiveInput = const $ pure ()
    , receiveAwaitInput = pure ()
    , receiveProgress = const $ pure ()
    , receiveComplete = \_ _ -> pure ()
    , receiveAbort = const $ pure ()
    }

fixtureDriver :: TimeoutFn IO -> IO (ReceiveScope IO Word)
              -> Tracer IO (TraceSendRecv BatchProtocol)
              -> (Time -> IO ()) -> Channel IO BL.ByteString
              -> Driver BatchProtocol pr ProgressState IO
fixtureDriver timeoutFn getScope tracer sent =
    progressDriverWithLimits tracer timeoutFn productionCodec
      (ProtocolSizeLimits (const 1024) (fromIntegral . BL.length))
      (ProtocolTimeLimits $ const $ Just 2) 4096 (const getScope) (const sent)

-- Exercise the existing mux protocol runner and MiniProtocolCb signature.
withMuxPair :: Tracer IO Mux.ChannelTrace
            -> (Bearer IO -> Bearer IO)
            -> (Mux.Mux Mux.InitiatorMode IO -> Mux.Mux Mux.ResponderMode IO -> IO a)
            -> IO a
withMuxPair tracer adapt action = do
    a <- Strict.newTBQueueIO 10
    b <- Strict.newTBQueueIO 10
    let bearerA = adapt $ queueChannelAsBearer (Mux.SDUSize 1260) (QueueChannel a b)
        bearerB = queueChannelAsBearer (Mux.SDUSize 1260) (QueueChannel b a)
        clientInfo = Mux.MiniProtocolInfo (Mux.MiniProtocolNum 2) Mux.InitiatorDirectionOnly
                       (Mux.MiniProtocolLimits 1048576) Nothing
        serverInfo = Mux.MiniProtocolInfo (Mux.MiniProtocolNum 2) Mux.ResponderDirectionOnly
                       (Mux.MiniProtocolLimits 1048576) Nothing
    client <- Mux.new (Mux.TracersI nullTracer tracer nullTracer) [clientInfo]
    server <- Mux.new Mux.nullTracers [serverInfo]
    withAsync (Mux.run client bearerA) $ \ca ->
      withAsync (Mux.run server bearerB) $ \sa -> do
        link ca
        link sa
        action client server

awaitResult :: Show e => STM IO (Either e a) -> IO a
awaitResult done = atomically done >>= either (fail . show) pure

receiveBytes :: Int64 -> Channel IO BL.ByteString -> IO ()
receiveBytes n channel
    | n <= 0 = pure ()
    | otherwise = recv channel >>= \case
        Nothing -> fail "unexpected EOF"
        Just bytes -> receiveBytes (n - BL.length bytes) channel

annotatedReadTimes :: IO Property
annotatedReadTimes = do
    installed <- newTVarIO quietScope
    events <- newTVarIO []
    terminals <- newTVarIO []
    readCount <- newTVarIO (0 :: Int)
    buffered <- newTVarIO []
    let first = BL.pack [0x82, 0x01, 0x81, 0x18, 0x01]
        second = encodeReply [7, 8]
        install name = do
          when (name == "B") $ threadDelay 0.02
          atomically $ writeTVar installed quietScope
            { receiveProgress = \event -> atomically $ modifyTVar events (++
                [(name, progressEnd event, progressValue event, progressObservedAt event)])
            , receiveComplete = \receipt remaining -> do
                decodedAt <- getMonotonicTime
                atomically $ do
                  modifyTVar terminals (++ [(name, receiptObservedAt receipt, decodedAt)])
                  modifyTVar buffered (++ [(name, remaining)])
            , receiveAbort = const $ fail "valid mux fixture unexpectedly aborted"
            }
        tracer = mkTracer $ \case
          TraceRecvMsg _ -> threadDelay 0.01
          _ -> pure ()
        callback = MiniProtocolCb $ \() channel -> withTimeoutSerial $ \timeoutFn -> do
          let counted = channel {recv = do
                atomically $ modifyTVar readCount (+ 1)
                recv channel}
              driver = fixtureDriver timeoutFn (atomically $ readTVar installed) tracer
                (const $ pure ()) counted
          (replies, state) <- runPipelinedPeerWithDriver driver $
            PeerPipelined $ twoRequests install
          pure (replies, progressTrailingBytes state)
    replies <- withMuxPair nullTracer id $ \client server -> do
      sr <- Mux.runMiniProtocol server (Mux.MiniProtocolNum 2) Mux.ResponderDirectionOnly
        Mux.StartEagerly $ \channel -> do
          receiveBytes 4 channel
          send channel (first <> second)
          receiveBytes 2 channel
          pure ((), Nothing)
      cr <- Mux.runMiniProtocol client (Mux.MiniProtocolNum 2)
        Mux.InitiatorDirectionOnly Mux.StartEagerly $ runMiniProtocolCb callback ()
      result <- awaitResult cr
      awaitResult sr
      pure result
    observed <- atomically $ readTVar events
    completedAt <- atomically $ readTVar terminals
    totalReads <- atomically $ readTVar readCount
    remaining <- atomically $ readTVar buffered
    pure $ counterexample (show (observed, completedAt, totalReads, replies)) $
      case (observed, completedAt) of
        ([("A", 5, 1, a), ("B", 4, 7, b), ("B", 5, 8, c)], [("A", ta, da), ("B", tb, db)]) -> conjoin
          [ a === b, b === c, a === ta, ta === tb
          , totalReads === 1
          , remaining === [("A", True), ("B", False)]
          , property (a <= da && da < db)
          , replies === [Batch [1] first, Batch [7, 8] second]
          ]
        _ -> property False

blockedSend :: IO Property
blockedSend = do
    gate <- newEmptyTMVarIO
    attemptedSecond <- newEmptyTMVarIO
    returnedFirst <- newEmptyTMVarIO
    firstTime <- newEmptyTMVarIO
    secondTime <- newEmptyTMVarIO
    calls <- newTVarIO (0 :: Int)
    -- Twice muxChannel's 0x3ffff-byte soft egress limit, so the
    -- second send stays blocked while the bearer write gate is closed.
    let values = replicate (2 * 0x3ffff) 1
        payload = encodeReply values
        final = encodeReply [2]
        tracer = mkTracer $ \case
          Mux.TraceChannelSendStart _ _ -> atomically $ do
            n <- readTVar calls
            writeTVar calls (n + 1)
            if n == 1 then putTMVar attemptedSecond () else pure ()
          _ -> pure ()
        sent ts = atomically $ do
          first <- isEmptyTMVar firstTime
          if first then putTMVar firstTime ts else putTMVar secondTime ts
        adapt bearer = bearer
          { write = \t tf sdu -> atomically (readTMVar gate) >> write bearer t tf sdu
          , writeMany = \t tf sdus -> atomically (readTMVar gate) >> writeMany bearer t tf sdus
          }
    (before, releasedAt, returnedAt) <- withMuxPair tracer adapt $ \client server -> do
      sr <- Mux.runMiniProtocol server (Mux.MiniProtocolNum 2) Mux.ResponderDirectionOnly
        Mux.StartEagerly $ \channel -> do
          receiveBytes (BL.length payload + BL.length final) channel
          pure ((), Nothing)
      cr <- Mux.runMiniProtocol client (Mux.MiniProtocolNum 2)
        Mux.InitiatorDirectionOnly Mux.StartEagerly $ \raw -> withTimeoutSerial $ \timeoutFn -> do
          let driver :: Driver BatchProtocol AsServer ProgressState IO
              driver = fixtureDriver timeoutFn (pure quietScope) nullTracer sent raw
          sendMessage driver ReflServerAgency (MsgResp (Batch values BL.empty))
          atomically $ putTMVar returnedFirst ()
          sendMessage driver ReflServerAgency (MsgResp (Batch [2] BL.empty))
          ts <- getMonotonicTime
          pure (ts, Nothing)
      atomically $ readTMVar returnedFirst >> readTMVar attemptedSecond
      before <- atomically $ tryReadTMVar secondTime
      releasedAt <- getMonotonicTime
      atomically $ putTMVar gate ()
      returnedAt <- awaitResult cr
      awaitResult sr
      pure (before, releasedAt, returnedAt)
    a <- atomically $ readTMVar firstTime
    b <- atomically $ readTMVar secondTime
    pure $ counterexample (show (before, a, b, releasedAt, returnedAt)) $ conjoin
      [ before === Nothing
      , property (a < releasedAt && releasedAt <= b && b <= returnedAt)
      ]

malformedTail :: IO Property
malformedTail = do
    progress <- newTVarIO []
    terminals <- newTVarIO []
    aborted <- newTVarIO []
    installed <- newTVarIO quietScope
    input <- newTVarIO $ Just $ BL.pack [0x82, 0x01, 0x82, 0x01, 0xff]
    let scope name = quietScope
          { receiveProgress = \event -> atomically $ do
              modifyTVar progress (++ [(name, progressValue event)])
              writeTVar installed (scope "replacement")
          , receiveComplete = \_ _buffered -> atomically $ modifyTVar terminals (++ [name])
          , receiveAbort = \_ -> atomically $ modifyTVar aborted (++ [name])
          }
        channel = Channel
          { send = const $ pure ()
          , recv = atomically $ do
              bytes <- readTVar input
              writeTVar input Nothing
              pure bytes
          }
    result <- try $ withTimeoutSerial $ \timeoutFn -> do
      let driver = fixtureDriver timeoutFn (atomically $ readTVar installed)
            nullTracer (const $ pure ()) channel
      void $ runPipelinedPeerWithDriver driver $ PeerPipelined $
        twoRequests $ \name -> atomically $ writeTVar installed (scope name)
    p <- atomically $ readTVar progress
    t <- atomically $ readTVar terminals
    a <- atomically $ readTVar aborted
    pure $ counterexample (show (result :: Either DecoderFailure (), p, t, a)) $ conjoin
      [ p === [("A", 1)]
      , t === []
      , a === ["A"]
      , property $ case result of Left _ -> True; Right _ -> False
      ]

receiveOne :: (ProgressEmitter IO Word -> AnnotatedCodec BatchProtocol CBOR.DeserialiseFailure IO BL.ByteString)
           -> ReceiveScope IO Word -> Word -> Int -> Maybe DiffTime -> Channel IO BL.ByteString -> IO ()
receiveOne codec scope messageLimit inputLimit fatal channel = withTimeoutSerial $ \timeoutFn -> do
    let driver :: Driver BatchProtocol AsClient ProgressState IO
        driver = progressDriverWithLimits nullTracer timeoutFn codec
          (ProtocolSizeLimits (const messageLimit) (fromIntegral . BL.length))
          (ProtocolTimeLimits $ const fatal)
          (fromIntegral inputLimit) (const $ pure scope) (\_ _ -> pure ()) channel
    void $ recvMessage driver @StBusy ReflServerAgency (initialDState driver)

fragmentation :: [Int] -> IO Property
fragmentation cuts = do
    let wire = encodeReply [24, 25] <> encodeReply [1]
        split [] bytes = [bytes]
        split (n : ns) bytes = let (a, b) = BL.splitAt (fromIntegral n) bytes
                              in a : split ns b
    channel <- fixedInputChannel (BL.empty : split cuts wire)
    installed <- newTVarIO (\_ _ _ -> pure ())
    events <- newTVarIO []
    let install name = atomically $ writeTVar installed $ \_ end value ->
          atomically $ modifyTVar events (++ [(name, end, value)])
    replies <- runFixture (atomically $ readTVar installed) install channel
    observed <- atomically $ readTVar events
    pure $ counterexample (show (cuts, replies, observed)) $ conjoin
      [ replies === [Batch [24, 25] (encodeReply [24, 25]), Batch [1] (encodeReply [1])]
      , observed === [("A", 5, 24), ("A", 7, 25), ("B", 4, 1)] ]

-- One physical read also contains the following reply. Limits must apply to
-- this message's consumption rather than indiscriminately charging that tail.
checkReceiveFailure
    :: (ProgressEmitter IO Word -> AnnotatedCodec BatchProtocol CBOR.DeserialiseFailure IO BL.ByteString)
    -> ProgressLimits -> Word -> Int -> ProgressFailure -> [Word] -> IO Property
checkReceiveFailure codec limits protocolLimit inputLimit expected accepted = do
    progress <- newTVarIO []
    aborted <- newTVarIO (0 :: Int)
    completed <- newTVarIO (0 :: Int)
    channel <- fixedInputChannel [encodeReply [24, 25] <> encodeReply [3]]
    let scope = quietScope
          { receiveLimits = limits
          , receiveProgress = \event -> atomically $ modifyTVar progress (++ [progressValue event])
          , receiveAbort = \_ -> atomically $ modifyTVar aborted (+ 1)
          , receiveComplete = \_ _buffered -> atomically $ modifyTVar completed (+ 1) }
    result <- try $ receiveOne codec scope protocolLimit inputLimit (Just 1) channel
    values <- atomically $ readTVar progress
    aborts <- atomically $ readTVar aborted
    terminals <- atomically $ readTVar completed
    pure $ counterexample (show (result :: Either SomeException (), values, aborts, terminals)) $ conjoin
      [ values === accepted, aborts === 1, terminals === 0
      , case result of Left err -> fromException err === Just expected; Right _ -> property False ]

progressLimits :: IO Property
progressLimits = conjoin <$> sequence
    [ checkReceiveFailure productionCodec (ProgressLimits 4 512 8) 1024 4096
        (ProgressMessageLimit 5 4) []
    , checkReceiveFailure productionCodec (ProgressLimits 1024 512 8) 6 4096
        (ProgressMessageLimit 7 6) [24]
    , checkReceiveFailure productionCodec (ProgressLimits 1024 1 8) 1024 4096
        (ProgressElementLimit 2 1) []
    , checkReceiveFailure productionCodec (ProgressLimits 1024 512 1) 1024 4096
        (ProgressCountLimit 1) [24]
    , checkReceiveFailure productionCodec (ProgressLimits 1024 512 8) 1024 2
        (ProgressInputLimit 11 2) [] ]

invalidRanges :: IO Property
invalidRanges = conjoin <$> sequence
    [ checkReceiveFailure (\emit -> productionCodec (\(WithByteSpan (value, _, _)) -> emit (WithByteSpan (value, 3, 99))))
        (ProgressLimits 1024 512 8) 1024 4096 (ProgressBeyondInput 99 11) []
    , checkReceiveFailure (\emit -> productionCodec (\spanValue@(WithByteSpan (value, _, _)) ->
          emit (if value == 25 then WithByteSpan (value, 0, 1) else spanValue)))
        (ProgressLimits 1024 512 8) 1024 4096 (InvalidProgressRange 0 1) [24]
    , checkReceiveFailure (\emit -> productionCodec (\(WithByteSpan (value, _, _)) ->
          emit (WithByteSpan (value, -1, 1))))
        (ProgressLimits 1024 512 8) 1024 4096 (InvalidProgressRange (-1) 1) []
    , checkReceiveFailure (\emit -> productionCodec (\(WithByteSpan (value, _, _)) ->
          emit (WithByteSpan (value, 3, 4294967297))))
        (ProgressLimits maxBound maxBound maxBound) maxBound 4096
        (ProgressMessageLimit 4294967297 4294967295) [] ]

callbackFailures :: IO Property
callbackFailures = conjoin <$> forM ["start", "input-start", "input", "progress", "terminal"] (\phase -> do
    values <- newTVarIO []
    aborted <- newTVarIO (0 :: Int)
    let reject at = when (phase == at) (throwIO $ userError "consumer rejected input")
        scope = quietScope
          { receiveStarted = reject "start"
          , receiveInputStarting = reject "input-start"
          , receiveInput = const $ reject "input"
          , receiveProgress = \event -> do
              when (progressValue event == 2) (reject "progress")
              atomically $ modifyTVar values (++ [progressValue event])
          , receiveComplete = \_ _ -> reject "terminal"
          , receiveAbort = \_ -> atomically $ modifyTVar aborted (+ 1) }
    channel <- fixedInputChannel [encodeReply [1, 2]]
    result <- try $ receiveOne productionCodec scope 1024 4096 (Just 1) channel
    accepted <- atomically $ readTVar values
    aborts <- atomically $ readTVar aborted
    pure $ counterexample (show (phase, result :: Either SomeException (), accepted, aborts)) $ conjoin
      [ aborts === 1
      , accepted === case phase of "progress" -> [1]; "terminal" -> [1, 2]; _ -> []
      , property $ case result of Left _ -> True; Right _ -> False ])

asyncBoundaries :: IO Property
asyncBoundaries = conjoin <$> forM ["start", "input-start", "input", "await", "progress", "terminal"] (\phase -> do
    entered <- newEmptyTMVarIO
    blocked <- newEmptyTMVarIO
    values <- newTVarIO []
    aborted <- newTVarIO (0 :: Int)
    completed <- newTVarIO (0 :: Int)
    let pause at = when (phase == at) $ do
          atomically $ putTMVar entered ()
          atomically $ readTMVar blocked
        scope = quietScope
          { receiveStarted = pause "start"
          , receiveInputStarting = pause "input-start"
          , receiveInput = const $ pause "input"
          , receiveAwaitInput = pause "await"
          , receiveProgress = \event -> do
              atomically $ modifyTVar values (++ [progressValue event])
              pause "progress"
          , receiveComplete = \_ _buffered -> pause "terminal" >> atomically (modifyTVar completed (+ 1))
          , receiveAbort = \_ -> atomically $ modifyTVar aborted (+ 1) }
    channel <- fixedInputChannel [encodeReply [1, 2]]
    withAsync (receiveOne productionCodec scope 1024 4096 (Just 1) channel) $ \receiver -> do
      atomically $ readTMVar entered
      cancel receiver
    accepted <- atomically $ readTVar values
    aborts <- atomically $ readTVar aborted
    terminals <- atomically $ readTVar completed
    pure $ counterexample (show (phase, accepted, aborts, terminals)) $ conjoin
      [ aborts === 1, terminals === 0
      , accepted === case phase of "progress" -> [1]; "terminal" -> [1, 2]; _ -> [] ])

fatalTimeout :: IO Property
fatalTimeout = conjoin <$> forM [False, True] (\emptyFlood -> do
    aborted <- newTVarIO (0 :: Int)
    blocked <- newEmptyTMVarIO
    let scope = quietScope { receiveAbort = \_ -> atomically $ modifyTVar aborted (+ 1) }
        channel = Channel { send = const $ pure ()
                          , recv = if emptyFlood then pure (Just BL.empty) else atomically $ readTMVar blocked }
    result <- try $ receiveOne productionCodec scope 1024 4096 (Just 0.02) channel
    aborts <- atomically $ readTVar aborted
    pure $ counterexample (show (result :: Either SomeException (), aborts)) $ conjoin
      [ aborts === 1, property $ case result of
          Left err -> case fromException err of Just ExceededTimeLimit{} -> True; _ -> False
          Right _ -> False ])

lateEmitter :: IO Property
lateEmitter = do
    saved <- newTVarIO Nothing
    values <- newTVarIO []
    let codec emit = progressCodec $ do
          atomically $ writeTVar saved (Just emit)
          pure $ \start end value -> emit (WithByteSpan (value, fromIntegral start, fromIntegral end))
        scope = quietScope { receiveProgress = \event -> atomically $ modifyTVar values (++ [progressValue event]) }
    channel <- fixedInputChannel [encodeReply [1]]
    receiveOne codec scope 1024 4096 (Just 1) channel
    Just emit <- atomically $ readTVar saved
    result <- try $ emit (WithByteSpan (2, 4, 5))
    accepted <- atomically $ readTVar values
    pure $ counterexample (show (result :: Either ProgressFailure (), accepted)) $
      conjoin [result === Left ProgressScopeClosed, accepted === [1]]

cancelledSend :: IO Property
cancelledSend = do
    entered <- newEmptyTMVarIO
    blocked <- newEmptyTMVarIO
    observations <- newTVarIO []
    let channel = Channel { send = \_ -> atomically (putTMVar entered ()) >> atomically (readTMVar blocked)
                          , recv = pure Nothing }
    withTimeoutSerial $ \timeoutFn -> do
      let driver :: Driver BatchProtocol AsClient ProgressState IO
          driver = fixtureDriver timeoutFn (pure quietScope) nullTracer
            (\at -> atomically $ modifyTVar observations (++ [at])) channel
      withAsync (sendMessage driver ReflClientAgency (MsgReq ())) $ \sender -> do
        atomically $ readTMVar entered
        cancel sender
    observed <- atomically $ readTVar observations
    pure $ observed === []

noElementProgress :: IO Property
noElementProgress = do
    receipts <- newTVarIO []
    let codec _ = progressCodec (pure $ \_ _ _ -> pure ())
        scope = quietScope
          { receiveLimits = ProgressLimits 1024 0 0
          , receiveProgress = const $ fail "no-element context received progress"
          , receiveComplete = \receipt _buffered -> atomically $ modifyTVar receipts (++ [receiptBytes receipt]) }
    channel <- fixedInputChannel [encodeReply [1, 2] <> encodeReply [3]]
    receiveOne codec scope 1024 4096 (Just 1) channel
    observed <- atomically $ readTVar receipts
    pure $ observed === [5]

inputBoundaries :: IO Property
inputBoundaries = do
    events <- newTVarIO []
    inputTimes <- newTVarIO []
    terminalTimes <- newTVarIO []
    startTimes <- newTVarIO []
    protected <- newTVarIO False
    let note event = atomically $ modifyTVar events (++ [event])
        scope = quietScope
          { receiveStarted = note "start" >> atomically (writeTVar protected True)
          , receiveAwaitInput = note "await" >> atomically (writeTVar protected False)
          , receiveInputStarting = do
              note "publish"
              now <- getMonotonicTime
              atomically $ writeTVar protected True >> modifyTVar startTimes (++ [now])
          , receiveInput = \observation -> do
              atomically (readTVar protected) >>= \active -> unless active (fail "input had no publication protection")
              note ("input" ++ show (inputBytes observation))
              atomically $ modifyTVar inputTimes (++ [inputObservedAt observation])
          , receiveProgress = \event -> note ("tx" ++ show (progressValue event))
          , receiveComplete = \receipt _buffered -> do
              note "done"
              atomically $ modifyTVar terminalTimes (++ [receiptObservedAt receipt]) }
    raw <- fixedInputChannel [BL.empty, BL.pack [0x82, 0x01, 0x82, 0x18], BL.empty, BL.pack [0x18, 0x18, 0x19]]
    let channel = raw { recv = do
          active <- atomically $ readTVar protected
          when active (fail "blocking read retained publication protection")
          recv raw }
    receiveOne productionCodec scope 1024 4096 (Just 1) channel
    observed <- atomically $ readTVar events
    inputs <- atomically $ readTVar inputTimes
    terminals <- atomically $ readTVar terminalTimes
    starts <- atomically $ readTVar startTimes
    pure $ counterexample (show (observed, inputs, terminals, starts)) $ conjoin
      [ observed === ["start", "await", "publish", "input4", "await", "publish", "input3", "tx24", "tx25", "done"]
      , property (length starts == 2 && and (zipWith (<=) starts inputs))
      , property $ case (inputs, terminals) of
          ([first, lastInput], [terminal]) -> first <= lastInput && lastInput == terminal
          _ -> False ]

emptyPublication :: IO Property
emptyPublication = do
    started <- newTVarIO (0 :: Int)
    aborted <- newTVarIO (0 :: Int)
    let scope = quietScope
          { receiveInputStarting = atomically $ modifyTVar started (+ 1)
          , receiveAbort = \_ -> atomically $ modifyTVar aborted (+ 1) }
    channel <- fixedInputChannel [BL.empty, BL.empty]
    result <- try $ receiveOne productionCodec scope 1024 4096 (Just 1) channel
    starts <- atomically $ readTVar started
    aborts <- atomically $ readTVar aborted
    pure $ counterexample (show (result :: Either SomeException (), starts, aborts)) $ conjoin
      [ starts === 0, aborts === 1
      , property $ case result of Left _ -> True; Right _ -> False ]

byteSpanAnnotations :: Property
byteSpanAnnotations =
    let bytes = BL.pack [0x82, 0x01, 0x81, 0x18, 0x01]
        annotated = WithByteSpan (id, 3, 5)
        Utils.WithBytes original decoded = Utils.runWithByteSpan bytes annotated
    in conjoin [original === BL.pack [0x18, 0x01], decoded === bytes]

fixedWidthBounds :: IO Property
fixedWidthBounds = do
    values <- newTVarIO []
    receipts <- newTVarIO []
    let scope = quietScope
          { receiveLimits = ProgressLimits (maxBound - 1) maxBound maxBound
          , receiveProgress = \event -> atomically $ modifyTVar values (++ [progressValue event])
          , receiveComplete = \receipt _buffered -> atomically $ modifyTVar receipts (++ [receiptBytes receipt]) }
    -- The sum of the message and chunk caps exceeds Word32. The actual read is
    -- tiny; wrapping that sum before checking would incorrectly reject it.
    channel <- fixedInputChannel [encodeReply [24, 25] <> encodeReply [3]]
    receiveOne productionCodec scope maxBound 11 (Just 1) channel
    accepted <- atomically $ readTVar values
    terminal <- atomically $ readTVar receipts
    pure $ conjoin [accepted === [24, 25], terminal === [7]]
