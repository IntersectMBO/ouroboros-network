{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.Protocol.SigSubmission where

import Control.Monad.ST (runST)
import Data.Bifunctor (second)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word32)

import Network.TypedProtocol.Codec hiding (prop_codec)

import Ouroboros.Network.SizeInBytes

import DMQ.Protocol.SigSubmission.Type
import DMQ.Protocol.SigSubmission.Codec

import Ouroboros.Network.Protocol.TxSubmission2.Test (labelMsg)

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck as QC
import Test.QuickCheck.Instances.ByteString ()


tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "SigSubmission"
      [ testProperty "codec"               prop_codec
      , testProperty "codec id"            prop_codec_id
      , testProperty "codec 2-splits"      prop_codec_splits2
      , testProperty "codec 3-splits"    $ withMaxSize 10
                                           prop_codec_splits3
      , testProperty "codec cbor"          prop_codec_cbor
      , testProperty "codec valid cbor"    prop_codec_valid_cbor
      ]
    ]

instance Arbitrary SigHash where
  arbitrary = SigHash <$> arbitrary
  shrink = map SigHash . shrink . getSigHash

instance Arbitrary SigId where
  arbitrary = SigId <$> arbitrary
  shrink = map SigId . shrink . getSigId

instance Arbitrary SigBody where
  arbitrary = SigBody <$> arbitrary
  shrink = map SigBody . shrink . getSigBody

instance Arbitrary SigTTL where
  -- generate only whole seconds (this is what we receive on the wire)
  arbitrary = SigTTL . realToFrac @Word32 <$> arbitrary
  -- shrink via Word32 (e.g. in seconds)
  shrink (SigTTL posix) = SigTTL . realToFrac
                      <$> shrink (floor @_ @Word32 posix)

instance Arbitrary SigKesSignature where
  arbitrary = SigKesSignature <$> arbitrary
  shrink = map SigKesSignature . shrink . getSigKesSignature

instance Arbitrary SigOpCertificate where
  arbitrary = SigOpCertificate <$> arbitrary
  shrink = map SigOpCertificate . shrink . getSigOpCertificate

instance Arbitrary Sig where
  arbitrary = Sig <$> arbitrary
                  <*> arbitrary
                  <*> (getSigTTL <$> arbitrary)
                  <*> arbitrary
                  <*> arbitrary
  shrink sig@Sig { sigId, sigBody, sigTTL, sigOpCertificate, sigKesSignature } =
    [ sig { sigId = sigId' }
    | sigId' <- shrink sigId
    ]
    ++
    [ sig { sigBody = sigBody' }
    | sigBody' <- shrink sigBody
    ]
    ++
    [ sig { sigTTL = sigTTL' }
    | SigTTL sigTTL' <- shrink (SigTTL sigTTL)
    ]
    ++
    [ sig { sigOpCertificate = sigOpCertificate' }
    | sigOpCertificate' <- shrink sigOpCertificate
    ]
    ++
    [ sig { sigKesSignature = sigKesSignature' }
    | sigKesSignature' <- shrink sigKesSignature
    ]


instance Arbitrary (AnyMessage SigSubmission) where
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

prop_codec :: AnyMessage SigSubmission -> Bool
prop_codec msg = 
  runST (prop_codecM codecSigSubmission msg)

prop_codec_id :: AnyMessage SigSubmission -> Bool
prop_codec_id msg =
  runST (prop_codecM codecSigSubmissionId msg)

prop_codec_splits2 :: AnyMessage SigSubmission -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codecSigSubmission msg)

prop_codec_splits3 :: AnyMessage SigSubmission -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_codec_splitsM splits3 codecSigSubmission msg)


prop_codec_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codecSigSubmission msg)

prop_codec_valid_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codecSigSubmission
