{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE DataKinds                  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Network.Protocol.LocalTxSubmission.Test (
    tests
  ) where

import           Data.ByteString.Lazy (ByteString)

import           Control.Monad.ST (runST)
import           Control.Monad.IOSim
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Tracer (nullTracer)

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise (encode, decode)

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Channel
import           Ouroboros.Network.Codec hiding (prop_codec)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Client
import           Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import           Ouroboros.Network.Protocol.LocalTxSubmission.Direct
import           Ouroboros.Network.Protocol.LocalTxSubmission.Examples
import           Ouroboros.Network.Protocol.LocalTxSubmission.Server
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type

import           Test.Ouroboros.Network.Testing.Utils (splits2, splits3)

import           Text.Show.Functions ()
import           Test.QuickCheck as QC
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)


--
-- Test cases
--

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.LocalTxSubmission"
  [ testProperty "direct"              prop_direct
  , testProperty "connect"             prop_connect
  , testProperty "codec"               prop_codec
  , testProperty "codec 2-splits"      prop_codec_splits2
  , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                       prop_codec_splits3
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  ]


--
-- Common types & clients and servers used in the tests in this module.
--

newtype Tx = Tx Int
  deriving (Eq, Show, Arbitrary, CoArbitrary, Serialise)

newtype Reject = Reject Int
  deriving (Eq, Show, Arbitrary, Serialise)


--
-- Properties going directly, not via Peer.
--

-- | Run a simple tx-submission client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: (Tx -> Maybe Reject) -> [Tx] -> Bool
prop_direct p txs =
    runSimOrThrow
      (direct
        (localTxSubmissionClient txs)
        (localTxSubmissionServer p))
  ==
    (txs', txs')
  where
    txs' = [ (tx, p tx) | tx <- txs ]


--
-- Properties going via Peer, but without using a channel
--

-- | Run a simple tx-submission client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
-- This test converts the pipelined server peer to a non-pipelined peer
-- before connecting it with the client.
--
prop_connect :: (Tx -> Maybe Reject) -> [Tx] -> Bool
prop_connect p txs =
    case runSimOrThrow
           (connect
             (localTxSubmissionClientPeer $ pure $
              localTxSubmissionClient txs)
             (localTxSubmissionServerPeer $ pure $
              localTxSubmissionServer p)) of

      (a, b, TerminalStates TokDone TokDone) ->
        (a, b) == (txs', txs')
  where
    txs' = [ (tx, p tx) | tx <- txs ]


--
-- Properties using a channel
--

-- | Run a local tx-submission client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> (Tx -> Maybe Reject) -> [Tx]
             -> m Bool
prop_channel createChannels p txs =

    ((txs', txs') ==) <$>

    runConnectedPeers
      createChannels
      nullTracer
      codec
      (localTxSubmissionClientPeer $ pure $
       localTxSubmissionClient txs)
      (localTxSubmissionServerPeer $ pure $
       localTxSubmissionServer p)
  where
    txs' = [ (tx, p tx) | tx <- txs ]


-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: (Tx -> Maybe Reject) -> [Tx] -> Bool
prop_channel_ST p txs =
    runSimOrThrow
      (prop_channel createConnectedChannels p txs)


-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: (Tx -> Maybe Reject) -> [Tx] -> Property
prop_channel_IO p txs =
    ioProperty (prop_channel createConnectedChannels p txs)


-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: (Tx -> Maybe Reject) -> [Tx] -> Property
prop_pipe_IO p txs =
    ioProperty (prop_channel createPipeConnectedChannels p txs)


--
-- Codec properties
--

instance Arbitrary (AnyMessageAndAgency (LocalTxSubmission Tx Reject)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) <$>
        (MsgSubmitTx <$> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokBusy) <$>
        pure MsgAcceptTx

    , AnyMessageAndAgency (ServerAgency TokBusy) <$>
        (MsgRejectTx <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokIdle) <$>
        pure MsgDone
    ]

instance (Show tx, Show reject) =>
          Show (AnyMessageAndAgency (LocalTxSubmission tx reject)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance (Eq tx, Eq reject) =>
          Eq (AnyMessage (LocalTxSubmission tx reject)) where

  (==) (AnyMessage (MsgSubmitTx tx))
       (AnyMessage (MsgSubmitTx tx')) = tx == tx'

  (==) (AnyMessage MsgAcceptTx)
       (AnyMessage MsgAcceptTx) = True

  (==) (AnyMessage (MsgRejectTx rej))
       (AnyMessage (MsgRejectTx rej')) = rej == rej'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
       => Codec (LocalTxSubmission Tx Reject)
                DeserialiseFailure
                m ByteString
codec = codecLocalTxSubmission
          Serialise.encode Serialise.decode
          Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec :: AnyMessageAndAgency (LocalTxSubmission Tx Reject) -> Bool
prop_codec msg =
  runST (prop_codecM codec msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (LocalTxSubmission Tx Reject) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (LocalTxSubmission Tx Reject) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 codec msg)

