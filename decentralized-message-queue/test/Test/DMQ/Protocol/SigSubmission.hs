{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.Protocol.SigSubmission where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad.ST (runST)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Word (Word32)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties hiding (prop_codec)

import Cardano.Binary (ToCBOR (..))

import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type

import Ouroboros.Network.Protocol.TxSubmission2.Test (labelMsg)

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck


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

instance Arbitrary POSIXTime where
  -- generate only whole seconds (this is what we receive on the wire)
  arbitrary = realToFrac @Word32 <$> arbitrary
  -- shrink via Word32 (e.g. in seconds)
  shrink posix = realToFrac <$> shrink (floor @_ @Word32 posix)

instance Arbitrary SigKESSignature where
  arbitrary = SigKESSignature <$> arbitrary
  shrink = map SigKESSignature . shrink . getSigKESSignature

instance Arbitrary SigOpCertificate where
  arbitrary = SigOpCertificate <$> arbitrary
  shrink = map SigOpCertificate . shrink . getSigOpCertificate

instance Arbitrary SigColdKey where
  arbitrary = SigColdKey <$> arbitrary
  shrink = map SigColdKey . shrink . getSigColdKey

instance Arbitrary SigRaw where
  arbitrary = SigRaw <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
  shrink sig@SigRaw { sigRawId,
                      sigRawBody,
                      sigRawKESSignature,
                      sigRawKESPeriod,
                      sigRawOpCertificate,
                      sigRawColdKey,
                      sigRawExpiresAt
                    } =
    [ sig { sigRawId = sigRawId' }
    | sigRawId' <- shrink sigRawId
    ]
    ++
    [ sig { sigRawBody = sigRawBody' }
    | sigRawBody' <- shrink sigRawBody
    ]
    ++
    [ sig { sigRawKESSignature = sigRawKESSignature' }
    | sigRawKESSignature' <- shrink sigRawKESSignature
    ]
    ++
    [ sig { sigRawKESPeriod = sigRawKESPeriod' }
    | sigRawKESPeriod' <- shrink sigRawKESPeriod
    ]
    ++
    [ sig { sigRawOpCertificate = sigRawOpCertificate' }
    | sigRawOpCertificate' <- shrink sigRawOpCertificate
    ]
    ++
    [ sig { sigRawColdKey = sigRawColdKey' }
    | sigRawColdKey' <- shrink sigRawColdKey
    ]
    ++
    [ sig { sigRawExpiresAt = sigRawExpiresAt' }
    | sigRawExpiresAt' <- shrink sigRawExpiresAt
    ]

mkSigRawWithSignedBytes :: SigRaw -> SigRawWithSignedBytes
mkSigRawWithSignedBytes sigRaw =
    SigRawWithSignedBytes {
      sigRaw,
      sigRawSignedBytes
    }
  where
    sigRawSignedBytes = CBOR.toLazyByteString (encodeSigRaw' sigRaw)

instance Arbitrary SigRawWithSignedBytes where
  arbitrary = mkSigRawWithSignedBytes <$> arbitrary
  shrink SigRawWithSignedBytes {sigRaw} = mkSigRawWithSignedBytes <$> shrink sigRaw

-- NOTE: this function is not exposed in the main library on purpose.  We
-- should never construct `Sig` by serialising `SigRaw`.
--
mkSig :: SigRawWithSignedBytes -> Sig
mkSig sigRawWithSignedBytes@SigRawWithSignedBytes { sigRaw } =
    SigWithBytes {
      sigRawBytes,
      sigRawWithSignedBytes
    }
  where
    sigRawBytes = CBOR.toLazyByteString (encodeSigRaw sigRaw)

instance Arbitrary Sig where
  arbitrary = mkSig <$> arbitrary
  shrink SigWithBytes {sigRawWithSignedBytes} = mkSig <$> shrink sigRawWithSignedBytes

-- encode only signed part
encodeSigRaw' :: SigRaw
              -> CBOR.Encoding
encodeSigRaw' SigRaw {
    sigRawId,
    sigRawBody,
    sigRawKESPeriod,
    sigRawExpiresAt
  }
  =  CBOR.encodeListLen 7
  <> encodeSigId sigRawId
  <> CBOR.encodeBytes (getSigBody sigRawBody)
  <> CBOR.encodeWord sigRawKESPeriod
  <> CBOR.encodeWord32 (floor sigRawExpiresAt)

-- encode together with KES signature, OCert and cold key.
encodeSigRaw :: SigRaw
             -> CBOR.Encoding
encodeSigRaw sigRaw@SigRaw { sigRawKESSignature, sigRawOpCertificate, sigRawColdKey } =
     encodeSigRaw' sigRaw
  <> CBOR.encodeBytes (getSigKESSignature sigRawKESSignature)
  <> toCBOR (getSigOpCertificate sigRawOpCertificate)
  <> CBOR.encodeBytes (getSigColdKey sigRawColdKey)


shrinkSigFn :: Sig -> [Sig]
shrinkSigFn SigWithBytes {sigRawWithSignedBytes = SigRawWithSignedBytes { sigRaw, sigRawSignedBytes } } =
    mkSig . (\sigRaw' -> SigRawWithSignedBytes { sigRaw = sigRaw', sigRawSignedBytes }) <$> shrink sigRaw

prop_codec :: AnyMessage SigSubmission -> Property
prop_codec msg =
  runST (prop_anncodecM codecSigSubmission msg)

prop_codec_id :: AnyMessage SigSubmission -> Property
prop_codec_id msg =
  runST (prop_codecM codecSigSubmissionId msg)

prop_codec_splits2 :: AnyMessage SigSubmission -> Property
prop_codec_splits2 msg =
  runST (prop_anncodec_splitsM splits2 codecSigSubmission msg)

prop_codec_splits3 :: AnyMessage SigSubmission -> Property
prop_codec_splits3 msg =
  labelMsg msg $
  runST (prop_anncodec_splitsM splits3 codecSigSubmission msg)


prop_codec_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_cbor msg =
  runST (prop_codec_cborM codecSigSubmission msg)

prop_codec_valid_cbor
  :: AnyMessage SigSubmission
  -> Property
prop_codec_valid_cbor = prop_codec_valid_cbor_encoding codecSigSubmission
