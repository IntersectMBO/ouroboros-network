{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.Protocol.LocalMsgNotification where

import Codec.Serialise (DeserialiseFailure)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim
import Control.Monad.ST (runST)
import Control.Tracer (Tracer, contramap, nullTracer)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Word
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck

import DMQ.Protocol.LocalMsgNotification.Client
import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Examples
import DMQ.Protocol.LocalMsgNotification.Server
import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.SigSubmission.Type hiding (SingTxSubmission (..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties hiding (prop_codec)
import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple
import Test.DMQ.Protocol.SigSubmission ()
import Test.Ouroboros.Network.Protocol.Utils
import Test.Ouroboros.Network.Utils

testTracer :: ({-Show a,-}Applicative m) => Tracer m a
testTracer = nullTracer -- debugTracer

tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "LocalMsgNotification"
      [ testProperty "codec" prop_codec
      , testProperty "codec 2-splits"   prop_codec_splits2
      , testProperty "codec 3-splits" $ withMaxSize 10
                                          prop_codec_splits3
      , testProperty "codec cbor"       prop_codec_cbor
      , testProperty "codec valid cbor" prop_codec_valid_cbor
      , testProperty "connect" prop_connect
      , testProperty "direct"  prop_direct
      , testProperty "channel ST" prop_channel_ST
      , testProperty "channel IO" prop_channel_IO
      ]
    ]

-- | Check the codec round trip property.
--
prop_codec :: AnyMessage (LocalMsgNotification Sig) -> Property
prop_codec msg = runST (prop_anncodecM codec msg)

prop_codec_splits2 :: AnyMessage (LocalMsgNotification Sig) -> Property
prop_codec_splits2 msg =
  runST (prop_anncodec_splitsM splits2 codec msg)

prop_codec_splits3 :: AnyMessage (LocalMsgNotification Sig) -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_anncodec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessage (LocalMsgNotification Sig)
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)

prop_codec_valid_cbor
  :: AnyMessage (LocalMsgNotification Sig)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec


-- | Run a simple tx-submission client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: Positive Word16 -> DistinctNEList Sig -> Bool
prop_connect (Positive maxMsgs) (DistinctNEList msgs) =
  case runSimOrThrow
         (connect
           (localMsgNotificationServerPeer $
            testServer testTracer maxMsgs msgs)
           (localMsgNotificationClientPeer $
            testClient testTracer maxMsgs)) of

    ((), msgs', TerminalStates SingDone SingDone) ->
      msgs' == NE.toList msgs

--
-- Properties using a channel
--

-- | Run a local tx-submission client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> Positive Word16
             -> DistinctNEList Sig
             -> m Property
prop_channel createChannels
             (Positive maxMsgs) (DistinctNEList msgs) =
  (\((), msgs') -> msgs' === NE.toList msgs) <$>

  runAnnotatedConnectedPeers
    createChannels
    testTracer
    codec
    (localMsgNotificationServerPeer $
       testServer (("server",) `contramap` testTracer) maxMsgs msgs)
    (localMsgNotificationClientPeer $
       testClient (("client",) `contramap` testTracer) maxMsgs)

-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: Positive Word16 -> DistinctNEList Sig
                -> Property
prop_channel_ST maxMsgs msgs =
  runSimOrThrow
    (prop_channel createConnectedChannels maxMsgs msgs)

-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: Positive Word16 -> DistinctNEList Sig
                -> Property
prop_channel_IO maxMsgs msgs =
    ioProperty $
      prop_channel createConnectedBufferedChannelsUnbounded maxMsgs msgs

--
-- Properties going directly, not via Peer.
--

-- | Run a simple tx-submission client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: Positive Word16 -> DistinctNEList Sig -> Bool
prop_direct (Positive maxMsgs) (DistinctNEList sigs) =
    runSimOrThrow
      (direct
        (testServer testTracer maxMsgs sigs)
        (testClient testTracer maxMsgs))
  ==
    (NE.toList sigs, ())


direct :: forall m msg a b. (Monad m)
       => LocalMsgNotificationServer m msg a
       -> LocalMsgNotificationClient m msg b
       -> m (b, a)
direct (LocalMsgNotificationServer mserver0) (LocalMsgNotificationClient mclient0) = do
  server0 <- mserver0
  client0 <- mclient0
  go server0 client0
  where
    go :: ServerIdle m msg a
       -> LocalMsgNotificationClientStIdle m msg b
       -> m (b, a)
    go ServerIdle { msgRequestHandler, msgDoneHandler } client = do
      case client of
        SendMsgDone mb -> (,) <$> mb <*> msgDoneHandler
        SendMsgRequestBlocking k -> do
          response <- msgRequestHandler SingBlocking
          case response of
            ServerReply (BlockingReply msgs) hasMore server' -> do
              client' <- k msgs hasMore
              go server' client'
        SendMsgRequestNonBlocking k -> do
          response <- msgRequestHandler SingNonBlocking
          case response of
            ServerReply (NonBlockingReply msgs) hasMore server' -> do
              client' <- k msgs hasMore
              go server' client'

--
-- Common utilities and types used in the tests in this module.
--

type LocalMsgNotificationCodec m msg =
    AnnotatedCodec (LocalMsgNotification msg)
                   DeserialiseFailure m
                   ByteString

codec :: MonadST m
      => LocalMsgNotificationCodec m Sig
codec = codecLocalMsgNotification


instance Arbitrary HasMore where
  arbitrary = elements [HasMore, DoesNotHaveMore]

instance Arbitrary msg => Arbitrary (AnyMessage (LocalMsgNotification msg)) where
  arbitrary = oneof
    [ pure . AnyMessage . MsgRequest $ SingBlocking
    , pure . AnyMessage . MsgRequest $ SingNonBlocking
    , AnyMessage <$>
        (MsgReply . BlockingReply . NE.fromList . QC.getNonEmpty <$> arbitrary <*> arbitrary)
    , AnyMessage <$>
        (MsgReply . NonBlockingReply <$> arbitrary <*> arbitrary)
    , pure $ AnyMessage MsgClientDone
    ]


instance (Eq msg) => Eq (AnyMessage (LocalMsgNotification msg)) where
  (==) (AnyMessage (MsgRequest SingBlocking))
       (AnyMessage (MsgRequest SingBlocking)) = True

  (==) (AnyMessage (MsgRequest SingNonBlocking))
       (AnyMessage (MsgRequest SingNonBlocking)) = True

  (==) (AnyMessage (MsgReply (BlockingReply msgs) hasMore))
       (AnyMessage (MsgReply (BlockingReply msgs') hasMore')) =
    (msgs, hasMore) == (msgs', hasMore')

  (==) (AnyMessage (MsgReply (NonBlockingReply msgs) hasMore))
       (AnyMessage (MsgReply (NonBlockingReply msgs') hasMore')) =
    (msgs, hasMore) == (msgs', hasMore')

  (==) (AnyMessage MsgClientDone)
       (AnyMessage MsgClientDone) = True

  _ == _ = False


labelMsg :: AnyMessage (LocalMsgNotification Sig) -> Property -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgRequest {}     -> "MsgRequest"
           MsgReply as _more -> "MsgReply " ++ renderRanges 3 (length as)
           MsgClientDone     -> "MsgClientDone"
        )


type TestServer m msg = LocalMsgNotificationServer m msg ()
type TestClient m msg = LocalMsgNotificationClient m msg [msg]

testServer :: Monad m
           => Tracer m (TraceEventServer msg)
           -> Word16
           -> NonEmpty msg
           -> TestServer m msg
testServer tracer maxSigs sigs =
  msgNotificationServer
    tracer maxSigs sigs


testClient :: Monad m
           => Tracer m (TraceEventClient msg)
           -> Word16
           -> TestClient m msg
testClient tracer maxMsgs =
  msgNotificationClient
    tracer maxMsgs
