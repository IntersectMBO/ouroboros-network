{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.TxSubmission2.Test (tests) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.List.NonEmpty as NonEmpty

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (Tracer (..), nullTracer)

import           Codec.Serialise (DeserialiseFailure)
import qualified Codec.Serialise as Serialise (decode, encode)

import           Network.TypedProtocol.Codec hiding (prop_codec)
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeersPipelined)
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello)
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TxV1
import qualified Ouroboros.Network.Protocol.TxSubmission.Codec as TxV1
import           Ouroboros.Network.Protocol.TxSubmission.Test (TxSubmissionTestParams (..), DistinctList (..), TxId (..), Tx (..), txId)
import qualified Ouroboros.Network.Protocol.TxSubmission2.Codec as TxV2
import           Ouroboros.Network.Protocol.TxSubmission2.Direct
import           Ouroboros.Network.Protocol.TxSubmission2.Examples
import           Ouroboros.Network.Protocol.TxSubmission2.Client
import           Ouroboros.Network.Protocol.TxSubmission2.Server
import           Ouroboros.Network.Protocol.TxSubmission2.Type
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TxV2
import qualified Ouroboros.Network.Protocol.TxSubmission2.Hello.Codec as Hello

import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     prop_codec_valid_cbor_encoding, splits2, splits3)

import           Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


--
-- Test cases
--


tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol"
    [ testGroup "TxSubmission2"
        [ testProperty "direct"              prop_direct
        , testProperty "connect 1"           prop_connect1
        , testProperty "connect 2"           prop_connect2
        , testProperty "codec"               prop_codec
        , testProperty "codec id"            prop_codec_id
        , testProperty "codec 2-splits"      prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                             prop_codec_splits3
        , testProperty "codec cbor"          prop_codec_cbor
        , testProperty "codec valid cbor"    prop_codec_valid_cbor
        , testProperty "encodings agree"     prop_encodings_agree
        , testProperty "v2 encodings agree"  prop_v2_encodings_argree
        , testProperty "channel ST"          prop_channel_ST
        , testProperty "channel IO"          prop_channel_IO
        , testProperty "pipe IO"             prop_pipe_IO
        ]
    ]

--
-- Common types & clients and servers used in the tests in this module.
--

type TestServer m = TxSubmissionServerPipelined TxId Tx m [Tx]
type TestClient m = TxSubmissionClient          TxId Tx m ()

testServer :: Monad m
           => Tracer m (TraceEventServer TxId Tx)
           -> TxSubmissionTestParams
           -> TestServer m
testServer tracer
           TxSubmissionTestParams {
             testMaxUnacked        = Positive (Small maxUnacked),
             testMaxTxIdsToRequest = Positive (Small maxTxIdsToRequest),
             testMaxTxToRequest    = Positive (Small maxTxToRequest)
           } =
    txSubmissionServer
      tracer txId
      maxUnacked maxTxIdsToRequest maxTxToRequest

testClient :: Monad m
           => Tracer m (TraceEventClient TxId Tx)
           -> TxSubmissionTestParams
           -> TestClient m
testClient tracer            TxSubmissionTestParams {
             testMaxUnacked   = Positive (Small maxUnacked),
             testTransactions = DistinctList txs
           } =
    txSubmissionClient
      tracer txId txSize
      maxUnacked
      txs
  where
    txSize _ = 500


--
-- Properties going directly, not via Peer.
--

-- | Run a simple tx-submission client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: TxSubmissionTestParams -> Bool
prop_direct params@TxSubmissionTestParams{testTransactions} =
    runSimOrThrow
      (directPipelined
        (testServer nullTracer params)
        (testClient nullTracer params))
  ==
    (fromDistinctList testTransactions, ())


-- | Run a simple tx-submission client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
-- This test converts the pipelined server peer to a non-pipelined peer
-- before connecting it with the client.
--
prop_connect1 :: TxSubmissionTestParams -> Bool
prop_connect1 params@TxSubmissionTestParams{testTransactions} =
    case runSimOrThrow
           (connect
             (forgetPipelined $
              txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates TokDone TokDone) ->
        txs' == fromDistinctList testTransactions



-- | Run a pipelined tx-submission client against a server, going via the
-- 'Peer' representation, but without going via a channel.
--
-- This test uses the pipelined server, connected to the non-pipelined client.
--
prop_connect2 :: TxSubmissionTestParams -> [Bool] -> Bool
prop_connect2 params@TxSubmissionTestParams{testTransactions}
                      choices =
    case runSimOrThrow
           (connectPipelined choices
             (txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates TokDone TokDone) ->
        txs' == fromDistinctList testTransactions

--
-- Properties using a channel
--

-- | Run a simple tx-submission client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> TxSubmissionTestParams
             -> m Bool
prop_channel createChannels params@TxSubmissionTestParams{testTransactions} =

    (\(txs', ()) -> txs' == fromDistinctList testTransactions) <$>

    runConnectedPeersPipelined
      createChannels
      nullTracer
      codec_v2
      (txSubmissionServerPeerPipelined $
       testServer nullTracer params)
      (txSubmissionClientPeer $
       testClient nullTracer params)


-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: TxSubmissionTestParams
                -> Bool
prop_channel_ST params =
    runSimOrThrow
      (prop_channel createConnectedChannels params)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: TxSubmissionTestParams -> Property
prop_channel_IO params =
    ioProperty (prop_channel createConnectedChannels params)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: TxSubmissionTestParams -> Property
prop_pipe_IO params =
    ioProperty (prop_channel createPipeConnectedChannels params)


instance Arbitrary (AnyMessageAndAgency ps)
      => Arbitrary (AnyMessageAndAgency (Hello ps stIdle)) where
  arbitrary =
      frequency [ (5, f <$> arbitrary)
                , (1, pure (AnyMessageAndAgency (ClientAgency Hello.TokHello) Hello.MsgHello))
                ]
    where
      f :: AnyMessageAndAgency ps -> AnyMessageAndAgency (Hello ps stIdle)
      f (AnyMessageAndAgency (ClientAgency tok) msg) =
          AnyMessageAndAgency
            (ClientAgency (Hello.TokClientTalk tok))
            (Hello.MsgTalk msg)
      f (AnyMessageAndAgency (ServerAgency tok) msg) =
          AnyMessageAndAgency
            (ServerAgency (Hello.TokServerTalk tok))
            (Hello.MsgTalk msg)


instance Arbitrary (AnyMessageAndAgency (TxSubmission2 TxId Tx)) where
  arbitrary = oneof
    [ pure $ AnyMessageAndAgency (ClientAgency TokInit) MsgInit
    , AnyMessageAndAgency (ServerAgency TokIdle) <$>
        (MsgRequestTxIds TokBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokIdle) <$>
        (MsgRequestTxIds TokNonBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , AnyMessageAndAgency (ClientAgency (TokTxIds TokBlocking)) <$>
        MsgReplyTxIds <$> (BlockingReply . NonEmpty.fromList
                                         . QC.getNonEmpty
                                       <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency (TokTxIds TokNonBlocking)) <$>
        MsgReplyTxIds <$> (NonBlockingReply <$> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokIdle) <$>
        MsgRequestTxs <$> arbitrary

    , AnyMessageAndAgency (ClientAgency TokTxs) <$>
        MsgReplyTxs <$> arbitrary

    , AnyMessageAndAgency (ClientAgency (TokTxIds TokBlocking)) <$>
        pure MsgDone
    ]

instance (Eq txid, Eq tx) => Eq (AnyMessage (TxSubmission2 txid tx)) where

  (==) (AnyMessage MsgInit)
       (AnyMessage MsgInit) = True

  (==) (AnyMessage (MsgRequestTxIds TokBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestTxIds TokBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgRequestTxIds TokNonBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestTxIds TokNonBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgReplyTxIds (BlockingReply txids)))
       (AnyMessage (MsgReplyTxIds (BlockingReply txids'))) =
    txids == txids'

  (==) (AnyMessage (MsgReplyTxIds (NonBlockingReply txids)))
       (AnyMessage (MsgReplyTxIds (NonBlockingReply txids'))) =
    txids == txids'

  (==) (AnyMessage (MsgRequestTxs txids))
       (AnyMessage (MsgRequestTxs txids')) = txids == txids'

  (==) (AnyMessage (MsgReplyTxs txs))
       (AnyMessage (MsgReplyTxs txs')) = txs == txs'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


instance Eq (AnyMessageAndAgency        ps)
      => Eq (AnyMessageAndAgency (Hello ps stIdle)) where
  (==) (AnyMessageAndAgency (ClientAgency Hello.TokHello) Hello.MsgHello)
       (AnyMessageAndAgency (ClientAgency Hello.TokHello) Hello.MsgHello)
     = True
  (==) (AnyMessageAndAgency (ClientAgency (Hello.TokClientTalk tok))  (Hello.MsgTalk msg))
       (AnyMessageAndAgency (ClientAgency (Hello.TokClientTalk tok')) (Hello.MsgTalk msg'))
     = AnyMessageAndAgency (ClientAgency tok) msg == AnyMessageAndAgency (ClientAgency tok') msg'
  (==) (AnyMessageAndAgency (ServerAgency (Hello.TokServerTalk tok))  (Hello.MsgTalk msg))
       (AnyMessageAndAgency (ServerAgency (Hello.TokServerTalk tok')) (Hello.MsgTalk msg'))
     = AnyMessageAndAgency (ServerAgency tok) msg == AnyMessageAndAgency (ServerAgency tok') msg'
  (==) _ _ = False


instance Eq (AnyMessage        ps)
      => Eq (AnyMessage (Hello ps stIdle)) where
  (==) (AnyMessage Hello.MsgHello)
       (AnyMessage Hello.MsgHello) =
    True
  (==) (AnyMessage (Hello.MsgTalk msg))
       (AnyMessage (Hello.MsgTalk msg')) =
    AnyMessage msg == AnyMessage msg'
  (==) _ _ = False


codec_v1 :: MonadST m
          => Codec (TxSubmission TxId Tx)
                   DeserialiseFailure
                   m ByteString
codec_v1 = TxV1.codecTxSubmission
           Serialise.encode Serialise.decode
           Serialise.encode Serialise.decode


codec_v2 :: MonadST m
         => Codec (TxSubmission2 TxId Tx)
                   DeserialiseFailure
                   m ByteString
codec_v2 = TxV2.codecTxSubmission2
           Serialise.encode Serialise.decode
           Serialise.encode Serialise.decode


codec_hello :: MonadST m
            => Codec (Hello (TxSubmission TxId Tx) TxV1.StIdle)
                      DeserialiseFailure
                      m ByteString
codec_hello = Hello.codecTxSubmission2
                Serialise.encode Serialise.decode
                Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec msg =
  runST (prop_codecM codec_v2 msg)

-- | Check the codec round trip property for the id condec.
--
prop_codec_id :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_id msg =
  runST (prop_codecM TxV2.codecTxSubmission2Id msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec_v2 msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 codec_v2 msg)

prop_codec_cbor
  :: AnyMessageAndAgency (TxSubmission2 TxId Tx)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec_v2 msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageAndAgency (TxSubmission2 TxId Tx)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec_v2

-- | 'codecTxSubmission' and 'codecTxSubmission2' agree on the encoding.  This
-- and 'prop_codec' ensures the 'codecTxSubmission2' is backward compatible with
-- 'codecTxSubmission'.
--
prop_encodings_agree :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_encodings_agree (AnyMessageAndAgency stok@ClientAgency {} msg) =
     encode (codec_v1 @IO)  stok msg
  == encode (codec_v2 @IO)
            (tokV1ToV2 stok)
            (msgV1ToV2 msg)
prop_encodings_agree (AnyMessageAndAgency stok@ServerAgency {} msg) =
     encode (codec_v1 @IO)  stok msg
  == encode (codec_v2 @IO)
            (tokV1ToV2 stok)
            (msgV1ToV2 msg)


type        V1ToV2 :: TxSubmission txid tx -> TxSubmission2 txid tx
type family V1ToV2 st where
  V1ToV2 TxV1.StIdle                       = TxV2.StIdle
  V1ToV2 (TxV1.StTxIds TxV1.StBlocking)    = TxV2.StTxIds TxV2.StBlocking
  V1ToV2 (TxV1.StTxIds TxV1.StNonBlocking) = TxV2.StTxIds TxV2.StNonBlocking
  V1ToV2 TxV1.StTxs                        = TxV2.StTxs
  V1ToV2 TxV1.StDone                       = TxV2.StDone

msgV1ToV2 :: Message (TxSubmission txid tx) from to
          -> Message (TxSubmission2 txid tx) (V1ToV2 from) (V1ToV2 to)
msgV1ToV2 (TxV1.MsgRequestTxIds TxV1.TokBlocking ackNo reqNo) =
           TxV2.MsgRequestTxIds TxV2.TokBlocking ackNo reqNo
msgV1ToV2 (TxV1.MsgRequestTxIds TxV1.TokNonBlocking ackNo reqNo) =
           TxV2.MsgRequestTxIds TxV2.TokNonBlocking ackNo reqNo
msgV1ToV2 (TxV1.MsgReplyTxIds (TxV1.BlockingReply txids)) =
           TxV2.MsgReplyTxIds (TxV2.BlockingReply txids)
msgV1ToV2 (TxV1.MsgReplyTxIds (TxV1.NonBlockingReply txids)) =
           TxV2.MsgReplyTxIds (TxV2.NonBlockingReply txids)
msgV1ToV2 (TxV1.MsgRequestTxs txs) =
           TxV2.MsgRequestTxs txs
msgV1ToV2 (TxV1.MsgReplyTxs txs) =
           TxV2.MsgReplyTxs txs
msgV1ToV2  TxV1.MsgDone =
           TxV2.MsgDone
    

tokV1ToV2 :: PeerHasAgency pr (st :: TxSubmission  txid tx)
          -> PeerHasAgency pr (V1ToV2 st)
tokV1ToV2 (ClientAgency (TxV1.TokTxIds TxV1.TokBlocking)) =
           ClientAgency (TxV2.TokTxIds TxV2.TokBlocking)
tokV1ToV2 (ClientAgency (TxV1.TokTxIds TxV1.TokNonBlocking)) =
           ClientAgency (TxV2.TokTxIds TxV2.TokNonBlocking) 
tokV1ToV2 (ClientAgency  TxV1.TokTxs) =
           ClientAgency  TxV2.TokTxs
tokV1ToV2 (ServerAgency  TxV1.TokIdle) =
           ServerAgency  TxV2.TokIdle

type        HelloToV2 :: Hello (TxSubmission txid tx) TxV1.StIdle
                      -> TxSubmission2 txid tx
type family HelloToV2 st where
  HelloToV2  Hello.StHello    = StInit
  HelloToV2 (Hello.StTalk st) = V1ToV2 st


prop_v2_encodings_argree
    :: AnyMessageAndAgency (Hello (TxSubmission TxId Tx) TxV1.StIdle)
    -> Bool
prop_v2_encodings_argree (AnyMessageAndAgency stok msg) =
       encode (codec_hello @IO) stok msg
    == encode (codec_v2 @IO) (tokHelloToV2 stok) (msgHelloToV2 msg)


msgHelloToV2 :: Message (Hello (TxSubmission TxId Tx) TxV1.StIdle) from to
             -> Message (TxSubmission2 TxId Tx) (HelloToV2 from) (HelloToV2 to)
msgHelloToV2  Hello.MsgHello     = MsgInit 
msgHelloToV2 (Hello.MsgTalk msg) = msgV1ToV2 msg


tokHelloToV2 :: PeerHasAgency pr (st :: Hello (TxSubmission txid tx) TxV1.StIdle)
             -> PeerHasAgency pr (HelloToV2 st)
tokHelloToV2 (ClientAgency Hello.TokHello) = ClientAgency TokInit
tokHelloToV2 (ClientAgency (Hello.TokClientTalk tok)) = tokV1ToV2 (ClientAgency tok)
tokHelloToV2 (ServerAgency (Hello.TokServerTalk tok)) = tokV1ToV2 (ServerAgency tok)
