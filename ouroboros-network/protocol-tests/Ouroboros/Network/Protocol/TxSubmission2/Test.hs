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
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.TxSubmission2.Test (tests) where

import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)

import           Codec.Serialise (DeserialiseFailure)
import qualified Codec.Serialise as Serialise (decode, encode)

import           Network.TypedProtocol.Codec hiding (prop_codec)
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple (runConnectedPeersPipelined)
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello)
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Test hiding (tests)
import           Ouroboros.Network.Protocol.TxSubmission2.Codec
import           Ouroboros.Network.Protocol.TxSubmission2.Type

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
        [ testProperty "connect 1" prop_connect1
        , testProperty "connect 2"           prop_connect2
        , testProperty "codec"               prop_codec
        , testProperty "codec id"            prop_codec_id
        , testProperty "codec 2-splits"      prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                             prop_codec_splits3
        , testProperty "codec cbor"          prop_codec_cbor
        , testProperty "codec valid cbor"    prop_codec_valid_cbor
        , testProperty "encodings agree"     prop_encodings_agree
        , testProperty "channel ST"          prop_channel_ST
        , testProperty "channel IO"          prop_channel_IO
        , testProperty "pipe IO"             prop_pipe_IO
        ]
    ]


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
              wrapServerPeerPipelined $
              txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (wrapClientPeer $
              txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates (Hello.TokDone TokDone) (Hello.TokDone TokDone)) ->
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
             (wrapServerPeerPipelined $
              txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (wrapClientPeer $
              txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates (Hello.TokDone TokDone) (Hello.TokDone TokDone)) ->
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
      codec2
      (wrapServerPeerPipelined $
       txSubmissionServerPeerPipelined $
       testServer nullTracer params)
      (wrapClientPeer $
       txSubmissionClientPeer $
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



codec2 :: MonadST m
        => Codec (TxSubmission2 TxId Tx)
                 DeserialiseFailure
                 m ByteString
codec2 = codecTxSubmission2
           Serialise.encode Serialise.decode
           Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec msg =
  runST (prop_codecM codec2 msg)

-- | Check the codec round trip property for the id condec.
--
prop_codec_id :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_id msg =
  runST (prop_codecM codecTxSubmission2Id msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec2 msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (TxSubmission2 TxId Tx) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 codec2 msg)

prop_codec_cbor
  :: AnyMessageAndAgency (TxSubmission2 TxId Tx)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec2 msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageAndAgency (TxSubmission2 TxId Tx)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec2

-- | 'codecTxSubmission' and 'codecTxSubmission2' agree on the encoding.  This
-- and 'prop_codec' ensures the 'codecTxSubmission2' is backward compatible with
-- 'codecTxSubmission'.
--
prop_encodings_agree :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_encodings_agree (AnyMessageAndAgency stok@(ClientAgency tok) msg) =
     encode (codec @IO)  stok msg
  == encode (codec2 @IO)
            (ClientAgency (Hello.TokClientTalk tok))
            (Hello.MsgTalk msg)
prop_encodings_agree (AnyMessageAndAgency stok@(ServerAgency tok) msg) =
     encode (codec @IO)  stok msg
  == encode (codec2 @IO)
            (ServerAgency (Hello.TokServerTalk tok))
            (Hello.MsgTalk msg)
