{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric              #-}
module Ouroboros.Network.Protocol.ObjectDiffusion.Test (tests) where

import Data.ByteString.Lazy (ByteString)
import Data.List (nub)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word16)

import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.ST (runST)

import Codec.Serialise (DeserialiseFailure, Serialise)
import Codec.Serialise qualified as Serialise (decode, encode)

import Network.TypedProtocol.Codec hiding (prop_codec)

import Ouroboros.Network.Util.ShowProxy

import Ouroboros.Network.Protocol.ObjectDiffusion.Codec
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

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
    [ testGroup "ObjectDiffusion"
        [ testProperty "codec"               prop_codec
        , testProperty "codec id"            prop_codec_id
        , testProperty "codec 2-splits"      prop_codec_splits2
        , testProperty "codec 3-splits"    $ withMaxSize 10
                                             prop_codec_splits3
        , testProperty "codec cbor"          prop_codec_cbor
        , testProperty "codec valid cbor"    prop_codec_valid_cbor
        ]
    ]

--
-- Common types & clients and servers used in the tests in this module.
--

newtype Object = Object ObjectId
  deriving (Eq, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy Object where
    showProxy _ = "Object"

-- | We use any `CBOR.Term`.  This allows us to use `any` in cddl specs.
--
newtype ObjectId = ObjectId Any
  deriving (Eq, Ord, Show, Arbitrary, Serialise, Generic, NFData)

instance ShowProxy ObjectId where
    showProxy _ = "ObjectId"

deriving newtype instance Arbitrary NumObjectIdsAck
deriving newtype instance Arbitrary NumObjectIdsReq

instance Arbitrary (AnyMessage (ObjectDiffusion ObjectId Object)) where
  arbitrary = oneof
    [ pure $ AnyMessage MsgInit
    , AnyMessage
        <$> ( MsgRequestObjectIds SingBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> ( MsgRequestObjectIds SingNonBlocking
            <$> arbitrary
            <*> arbitrary
            )

    , AnyMessage
        <$> MsgReplyObjectIds
        <$> ( BlockingReply
            . NonEmpty.fromList
            . QC.getNonEmpty
            )
        <$> arbitrary

    , AnyMessage
        <$> MsgReplyObjectIds
        <$> NonBlockingReply
        <$> arbitrary

    , AnyMessage
        <$> MsgRequestObjects
        <$> arbitrary

    , AnyMessage
        <$> MsgReplyObjects
        <$> arbitrary

    , AnyMessage
        <$> pure MsgDone
    ]

instance (Eq objectId
         , Eq object
         )
      => Eq (AnyMessage (ObjectDiffusion objectId object)) where

  (==) (AnyMessage MsgInit)
       (AnyMessage MsgInit) = True

  (==) (AnyMessage (MsgRequestObjectIds SingBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestObjectIds SingBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgRequestObjectIds SingNonBlocking ackNo  reqNo))
       (AnyMessage (MsgRequestObjectIds SingNonBlocking ackNo' reqNo')) =
    (ackNo, reqNo) == (ackNo', reqNo')

  (==) (AnyMessage (MsgReplyObjectIds (BlockingReply objectIds)))
       (AnyMessage (MsgReplyObjectIds (BlockingReply objectIds'))) =
    objectIds == objectIds'

  (==) (AnyMessage (MsgReplyObjectIds (NonBlockingReply objectIds)))
       (AnyMessage (MsgReplyObjectIds (NonBlockingReply objectIds'))) =
    objectIds == objectIds'

  (==) (AnyMessage (MsgRequestObjects objectIds))
       (AnyMessage (MsgRequestObjects objectIds')) = objectIds == objectIds'

  (==) (AnyMessage (MsgReplyObjects txs))
       (AnyMessage (MsgReplyObjects txs')) = txs == txs'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
      => Codec
           (ObjectDiffusion ObjectId Object)
           DeserialiseFailure
           m ByteString
codec = codecObjectDiffusion
          Serialise.encode Serialise.decode
          Serialise.encode Serialise.decode


-- | Check the codec round trip property.
--
prop_codec
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Bool
prop_codec msg =
  runST (prop_codecM codec msg)

-- | Check the codec round trip property for the id condec.
--
prop_codec_id
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Bool
prop_codec_id msg =
  runST (prop_codecM codecObjectDiffusionId msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)

-- | Check that the encoder produces a valid CBOR.
--
prop_codec_valid_cbor
  :: AnyMessage (ObjectDiffusion ObjectId Object)
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codec

--
-- Local generators
--

data ObjectSubmissionTestParams =
     ObjectSubmissionTestParams {
       testMaxUnacked        :: Positive (Small Word16),
       testMaxObjectIdsToRequest :: Positive (Small Word16),
       testMaxObjectToRequest    :: Positive (Small Word16),
       testTransactions      :: DistinctList Object
     }
  deriving Show

instance Arbitrary ObjectSubmissionTestParams where
  arbitrary =
    ObjectSubmissionTestParams <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

  shrink (ObjectSubmissionTestParams a b c d) =
    [ ObjectSubmissionTestParams a' b' c' d'
    | (a', b', c', d') <- shrink (a, b, c, d) ]


newtype DistinctList a = DistinctList { fromDistinctList :: [a] }
  deriving Show

instance (Eq a, Arbitrary a) => Arbitrary (DistinctList a) where
  arbitrary = DistinctList . nub <$> arbitrary

  shrink (DistinctList xs) =
    [ DistinctList (nub xs') | xs' <- shrink xs ]


labelMsg :: AnyMessage (ObjectDiffusion objectId object) -> Bool -> Property
labelMsg (AnyMessage msg) =
  label (case msg of
           MsgInit                -> "MsgInit"
           MsgRequestObjectIds {} -> "MsgRequestObjectIds"
           MsgReplyObjectIds as   -> "MsgReplyObjectIds " ++ renderRanges 3 (length as)
           MsgRequestObjects as   -> "MsgRequestObjects " ++ renderRanges 3 (length as)
           MsgReplyObjects as     -> "MsgReplyObjects " ++ renderRanges 3 (length as)
           MsgDone                -> "MsgDone"
        )
