{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans #-}
-- usage of `MsgKThxBye` is safe in this module.
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

module Ouroboros.Network.Protocol.TxSubmission.Test (
    tests
   ,TxId (..)
   ,Tx (..)
   ,TxSubmissionTestParams (..)
   ,testClient
   ,testServer
   ,codec
   ,DistinctList (..)
   ) where

import           Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word (Word16)
import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.ST (runST)
import           Control.Monad.IOSim
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Tracer (Tracer(..), nullTracer)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise (encode, decode)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Codec hiding (prop_codec)
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Driver.Simple
                   (runConnectedPeersPipelined)
import           Ouroboros.Network.Util.ShowProxy

import           Ouroboros.Network.Protocol.TxSubmission.Client
import           Ouroboros.Network.Protocol.TxSubmission.Codec
import           Ouroboros.Network.Protocol.TxSubmission.Direct
import           Ouroboros.Network.Protocol.TxSubmission.Examples
import           Ouroboros.Network.Protocol.TxSubmission.Server
import           Ouroboros.Network.Protocol.TxSubmission.Type

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
  [ testGroup "TxSubmission"
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
      , testProperty "channel ST"          prop_channel_ST
      , testProperty "channel IO"          prop_channel_IO
      , testProperty "pipe IO"             prop_pipe_IO
      ]
  ]


--
-- Common types & clients and servers used in the tests in this module.
--

newtype Tx = Tx TxId
  deriving (Eq, Show, Arbitrary, Serialise)

instance ShowProxy Tx where
    showProxy _ = "Tx"

txId :: Tx -> TxId
txId (Tx txid) = txid

newtype TxId = TxId Int
  deriving (Eq, Ord, Show, Arbitrary, Serialise)

instance ShowProxy TxId where
    showProxy _ = "TxId"

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


--
-- Properties going via Peer, but without using a channel
--

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
      codec
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


--
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency (TxSubmission TxId Tx)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ServerAgency TokIdle) <$>
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

    , AnyMessageAndAgency (ServerAgency TokIdle) <$>
        pure MsgKThxBye

    , AnyMessageAndAgency (ClientAgency TokTxs) <$>
        MsgReplyTxs <$> arbitrary

    , AnyMessageAndAgency (ClientAgency (TokTxIds TokBlocking)) <$>
        pure MsgDone
    ]

instance (Eq txid, Eq tx) => Eq (AnyMessage (TxSubmission txid tx)) where

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

  (==) (AnyMessage MsgKThxBye)
       (AnyMessage MsgKThxBye) = True

  _ == _ = False


codec :: MonadST m
       => Codec (TxSubmission TxId Tx)
                DeserialiseFailure
                m ByteString
codec = codecTxSubmission
          Serialise.encode Serialise.decode
          Serialise.encode Serialise.decode

-- | Check the codec round trip property.
--
prop_codec :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_codec msg =
  runST (prop_codecM codec msg)

-- | Check the codec round trip property for the id condec.
--
prop_codec_id :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_codec_id msg =
  runST (prop_codecM codecTxSubmissionId msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (TxSubmission TxId Tx) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessageAndAgency (TxSubmission TxId Tx)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessageAndAgency (TxSubmission TxId Tx)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec

--
-- Local generators
--

data TxSubmissionTestParams =
     TxSubmissionTestParams {
       testMaxUnacked        :: Positive (Small Word16),
       testMaxTxIdsToRequest :: Positive (Small Word16),
       testMaxTxToRequest    :: Positive (Small Word16),
       testTransactions      :: DistinctList Tx
     }
  deriving Show

instance Arbitrary TxSubmissionTestParams where
  arbitrary =
    TxSubmissionTestParams <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

  shrink (TxSubmissionTestParams a b c d) =
    [ TxSubmissionTestParams a' b' c' d'
    | (a', b', c', d') <- shrink (a, b, c, d) ]


newtype DistinctList a = DistinctList { fromDistinctList :: [a] }
  deriving Show

instance (Eq a, Arbitrary a) => Arbitrary (DistinctList a) where
  arbitrary = DistinctList . nub <$> arbitrary

  shrink (DistinctList xs) =
    [ DistinctList (nub xs') | xs' <- shrink xs ]

