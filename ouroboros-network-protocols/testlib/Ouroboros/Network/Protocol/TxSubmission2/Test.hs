{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
module Ouroboros.Network.Protocol.TxSubmission2.Test
  ( tests
  , Tx (..)
  , TxId (..)
  ) where

import Data.Bifunctor (second)
import Data.ByteString.Lazy (ByteString)
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word16)

import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadCatch)
import Control.Monad.IOSim
import Control.Monad.ST (runST)
import Control.Tracer (Tracer (..), contramap, nullTracer)

import Codec.Serialise (DeserialiseFailure, Serialise)
import Codec.Serialise qualified as Serialise (decode, encode)

import Network.TypedProtocol.Codec hiding (prop_codec)
import Network.TypedProtocol.Proofs

import Ouroboros.Network.Channel
import Ouroboros.Network.Driver.Simple (runConnectedPeersPipelined)
import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Protocol.TxSubmission2.Client
import Ouroboros.Network.Protocol.TxSubmission2.Codec
import Ouroboros.Network.Protocol.TxSubmission2.Direct
import Ouroboros.Network.Protocol.TxSubmission2.Examples
import Ouroboros.Network.Protocol.TxSubmission2.Server
import Ouroboros.Network.Protocol.TxSubmission2.Type

import Test.Data.CDDL (Any (..))
import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)
import Test.Ouroboros.Network.Utils (renderRanges)

import Control.DeepSeq
import GHC.Generics
import Test.QuickCheck as QC
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


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
        , testProperty "codec 3-splits"    $ withMaxSize 10
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
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy Tx where
    showProxy _ = "Tx"

txId :: Tx -> TxId
txId (Tx txid) = txid

-- | We use any `CBOR.Term`.  This allows us to use `any` in cddl specs.
--
newtype TxId = TxId Any
  deriving (Eq, Ord, Show, Arbitrary, Serialise, Generic, NFData)

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
             (forgetPipelined [] $
              txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates SingDone SingDone) ->
        txs' == fromDistinctList testTransactions



-- | Run a pipelined tx-submission client against a server, going via the
-- 'Peer' representation, but without going via a channel.
--
-- This test uses the pipelined server, connected to the non-pipelined client.
--
prop_connect2 :: TxSubmissionTestParams -> NonEmptyList Bool -> Bool
prop_connect2 params@TxSubmissionTestParams{testTransactions}
                     (NonEmpty choices) =
    case runSimOrThrow
           (connectPipelined choices
             (txSubmissionServerPeerPipelined $
              testServer nullTracer params)
             (txSubmissionClientPeer $
              testClient nullTracer params)) of

      (txs', (), TerminalStates SingDone SingDone) ->
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
       testServer (("server",) `contramap` nullTracer) params)
      (txSubmissionClientPeer $
       testClient (("client",) `contramap` nullTracer) params)


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
    ioProperty (prop_channel createConnectedBufferedChannelsUnbounded params)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: TxSubmissionTestParams -> Property
prop_pipe_IO params =
    ioProperty (prop_channel createPipeConnectedChannels params)


deriving newtype instance Arbitrary NumTxIdsToAck
deriving newtype instance Arbitrary NumTxIdsToReq


instance Arbitrary (AnyMessage (TxSubmission2 TxId Tx)) where
  arbitrary = oneof
    [ pure $ AnyMessage MsgInit
    , AnyMessage  <$>
        (MsgRequestTxIds SingBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , AnyMessage <$>
        (MsgRequestTxIds SingNonBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , AnyMessage <$>
        MsgReplyTxIds <$> (BlockingReply . NonEmpty.fromList
                                         . map (second SizeInBytes)
                                         . QC.getNonEmpty
                                       <$> arbitrary)

    , AnyMessage <$> MsgReplyTxIds <$> (NonBlockingReply . map (second SizeInBytes) <$> arbitrary)

    , AnyMessage <$> MsgRequestTxs <$> arbitrary

    , AnyMessage <$> MsgReplyTxs <$> arbitrary

    , AnyMessage <$> pure MsgDone
    ]

instance (Eq txid, Eq tx) => Eq (AnyMessage (TxSubmission2 txid tx)) where

  (==) (AnyMessage MsgInit)
       (AnyMessage MsgInit) = True

  (==) (AnyMessage (MsgRequestTxIds SingBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestTxIds SingBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgRequestTxIds SingNonBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestTxIds SingNonBlocking ackNo' reqNo')) =
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


codec_v2 :: MonadST m
         => Codec (TxSubmission2 TxId Tx)
                   DeserialiseFailure
                   m ByteString
codec_v2 = codecTxSubmission2
           Serialise.encode Serialise.decode
           Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec :: AnyMessage (TxSubmission2 TxId Tx) -> Bool
prop_codec msg =
  runST (prop_codecM codec_v2 msg)

-- | Check the codec round trip property for the id condec.
--
prop_codec_id :: AnyMessage (TxSubmission2 TxId Tx) -> Bool
prop_codec_id msg =
  runST (prop_codecM codecTxSubmission2Id msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessage (TxSubmission2 TxId Tx) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec_v2 msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessage (TxSubmission2 TxId Tx) -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codec_v2 msg)

prop_codec_cbor
  :: AnyMessage (TxSubmission2 TxId Tx)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec_v2 msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessage (TxSubmission2 TxId Tx)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec_v2

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


labelMsg :: AnyMessage (TxSubmission2 txid tx) -> Bool -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgInit            -> "MsgInit"
           MsgRequestTxIds {} -> "MsgRequestTxIds"
           MsgReplyTxIds as   -> "MsgReplyTxIds " ++ renderRanges 3 (length as)
           MsgRequestTxs as   -> "MsgRequestTxs " ++ renderRanges 3 (length as)
           MsgReplyTxs as     -> "MsgReplyTxs " ++ renderRanges 3 (length as)
           MsgDone            -> "MsgDone"
        )
