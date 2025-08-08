{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.Protocol.LocalMsgNotification where

import Codec.Serialise (DeserialiseFailure)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.ST (runST)
import Data.ByteString.Lazy (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Test.QuickCheck qualified as QC
import Test.Tasty
import Test.Tasty.QuickCheck

import DMQ.Protocol.LocalMsgNotification.Codec
import DMQ.Protocol.LocalMsgNotification.Type
import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type hiding (SingBlockingStyle (..), BlockingReplyList (..))
import Network.TypedProtocol.Codec hiding (prop_codec)
import Test.DMQ.Protocol.SigSubmission ()
import Test.Ouroboros.Network.Protocol.Utils
import Test.Ouroboros.Network.Utils


tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "LocalMsgNotification"
      [ testProperty "codec" prop_codec
      , testProperty "codec 2-splits" prop_codec_splits2
      , testProperty "codec 3-splits" $ withMaxSize 10
                                          prop_codec_splits3
      , testProperty "codec cbor"       prop_codec_cbor
      , testProperty "codec valid cbor" prop_codec_valid_cbor
      ]
    ]

-- | Check the codec round trip property.
--
prop_codec :: AnyMessage (LocalMsgNotification Sig) -> Bool
prop_codec msg = runST (prop_codecM codec msg)

prop_codec_splits2 :: AnyMessage (LocalMsgNotification Sig) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

prop_codec_splits3 :: AnyMessage (LocalMsgNotification Sig) -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codecSigSubmission msg)

prop_codec_valid_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codecSigSubmission


--
-- Common utilities and types used in the tests in this module.
--

codec :: MonadST m
      => Codec (LocalMsgNotification Sig)
               DeserialiseFailure m
               ByteString
codec = codecLocalMsgNotification encodeSig decodeSig

instance Arbitrary HasMore where
  arbitrary = elements [HasMore, DoesNotHaveMore]

instance Arbitrary msg => Arbitrary (AnyMessage (LocalMsgNotification msg)) where
  arbitrary = oneof
    [ pure . AnyMessage . MsgRequest $ SingBlocking
    , pure . AnyMessage . MsgRequest $ SingNonBlocking
    , AnyMessage <$>
        ((MsgReply . BlockingReply . NonEmpty.fromList . QC.getNonEmpty <$> arbitrary) <*> arbitrary)
    , AnyMessage <$>
        ((MsgReply . NonBlockingReply <$> arbitrary) <*> arbitrary)
    , pure $ AnyMessage MsgServerDone
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

  (==) (AnyMessage MsgServerDone)
       (AnyMessage MsgServerDone) = True

  (==) (AnyMessage MsgClientDone)
       (AnyMessage MsgClientDone) = True

  _ == _ = False


labelMsg :: AnyMessage (LocalMsgNotification Sig) -> Bool -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgRequest {}      -> "MsgRequest"
           MsgReply as _more  -> "MsgReply " ++ renderRanges 3 (length as)
           MsgServerDone      -> "MsgServerDone"
           MsgClientDone      -> "MsgClientDone"
        )

--
-- Properties using a channel
--

-- | Run a local tx-submission client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> (Sig -> SubmitResult Reject) -> [Tx]
             -> m Bool
prop_channel createChannels p txs =

    ((txs', txs') ==) <$>

    runConnectedPeers
      createChannels
      nullTracer
      codec
      (localTxSubmissionClientPeer $
       localTxSubmissionClient txs)
      (localTxSubmissionServerPeer $ pure $
       localTxSubmissionServer p)
  where
    txs' = [ (tx, p tx) | tx <- txs ]
