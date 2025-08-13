{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.DMQ.Protocol.SigSubmission where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad.ST (runST)
import Data.Bifunctor (second)
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Typeable
import Data.Word (Word32)
import GHC.TypeNats (KnownNat)
import System.IO.Unsafe (unsafePerformIO)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties hiding (prop_codec)

import Cardano.Binary (ToCBOR (..))
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..), VerKeyKES)
import Cardano.Crypto.KES.Class qualified as KES
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.KES.OCert qualified as KES
import Cardano.KESAgent.Protocols.StandardCrypto (MockCrypto, StandardCrypto)
import Test.Crypto.Instances

import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type

import Ouroboros.Network.Protocol.TxSubmission2.Test (labelMsg)

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2, splits3)

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC


tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "SigSubmission"
      [ testGroup "mockcrypto"
        [ testProperty "codec"               prop_codec_mockcrypto
        , testProperty "codec id"            prop_codec_id_mockcrypto
        , testProperty "codec 2-splits"    $ withMaxSize 20
                                           $ withMaxSuccess 20
                                             prop_codec_splits2_mockcrypto
        , testProperty "codec 3-splits"    $ withMaxSize 10
                                           $ withMaxSuccess 10
                                             prop_codec_splits3_mockcrypto
        , testProperty "codec cbor"          prop_codec_cbor_mockcrypto
        , testProperty "codec valid cbor"    prop_codec_valid_cbor_mockcrypto
        ]
      , testGroup "standardcrypto"
        [ testProperty "codec"               prop_codec_standardcrypto
        , testProperty "codec id"            prop_codec_id_standardcrypto
        , testProperty "codec 2-splits"    $ withMaxSize 20
                                           $ withMaxSuccess 20
                                             prop_codec_splits2_standardcrypto
        -- StandardCrypt produces too large messages for this test to run:
        {-
        , testProperty "codec 3-splits"    $ withMaxSize 10
                                           $ withMaxSuccess 10
                                             prop_codec_splits3_standardcrypto
        -}
        , testProperty "codec cbor"          prop_codec_cbor_standardcrypto
        , testProperty "codec valid cbor"    prop_codec_valid_cbor_standardcrypto
        ]
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

mkVerKeyKES
  :: forall kesCrypto.
     KESAlgorithm kesCrypto
  => PinnedSizedBytes (SeedSizeKES kesCrypto)
  -> IO (VerKeyKES kesCrypto)
mkVerKeyKES seed = do
    withMLockedSeedFromPSB seed $ \mseed ->
      KES.genKeyKES mseed >>= deriveVerKeyKES


data WithConstr ctx key a =
    WithConstr { constr :: key -> a,
                 mkKey  :: ctx -> IO key,
                 ctx    :: ctx
               }
deriving instance Functor (WithConstr ctx key)


constWithConstr :: a -> WithConstr [ctx] [key] a
constWithConstr a =
    WithConstr { constr = const a,
                 mkKey  = \_ -> pure [],
                 ctx = []
               }

listWithConstr :: forall ctx key a b.
                  ([a] -> b)
               -> [WithConstr  ctx   key  a]
               ->  WithConstr [ctx] [key] b
listWithConstr constr' as =
    WithConstr {
      constr = \keys -> constr' (zipWith ($) constrs keys),
      mkKey  = \ctxs -> sequence (zipWith ($) mkKeys ctxs),
      ctx    = ctx <$> as

    }
  where
    constrs :: [(key -> a)]
    constrs = constr <$> as

    mkKeys :: [(ctx -> IO key)]
    mkKeys = mkKey <$> as


-- shrink ctx
shrinkWithConstrCtx :: Arbitrary ctx => WithConstr ctx key a -> [WithConstr ctx key a]
shrinkWithConstrCtx constr@WithConstr { ctx } =
  [ constr { ctx = ctx' }
  | ctx' <- shrink ctx
  ]


sequenceWithConstr
  :: (a -> key -> a)
  ->     WithConstr ctx key [a]
  -> IO [WithConstr ctx key  a]
sequenceWithConstr update constr@WithConstr { mkKey, ctx } = do
    as <- runWithConstr constr
    return [ WithConstr { constr = update a, mkKey, ctx }
           | a <- as
           ]


-- unsafePerformIO :(
shrinkWithConstr :: Arbitrary ctx
                 => (a -> key -> a)
                 -> (a -> [a])
                 -> WithConstr ctx key a
                 -> [WithConstr ctx key a]
shrinkWithConstr update shrinker constr =
       unsafePerformIO (sequenceWithConstr update $ shrinker <$> constr)
    ++ shrinkWithConstrCtx constr


runWithConstr :: WithConstr ctx key a -> IO a
runWithConstr WithConstr { constr, mkKey, ctx } = constr <$> mkKey ctx


type VerKeyKESCTX            size          = PinnedSizedBytes size
type WithConstrVerKeyKES     size crypto a = WithConstr (VerKeyKESCTX size) (VerKeyKES crypto) a
type WithConstrVerKeyKESList size crypto a = WithConstr [VerKeyKESCTX size] [VerKeyKES crypto] a

mkVerKeyKESConstr
  :: forall kesCrypto.
     KESAlgorithm kesCrypto
  => VerKeyKESCTX (SeedSizeKES kesCrypto)
  -> WithConstrVerKeyKES (SeedSizeKES kesCrypto)
                         kesCrypto
                         (VerKeyKES kesCrypto)
mkVerKeyKESConstr ctx =
  WithConstr { constr = id,
               mkKey  = mkVerKeyKES,
               ctx
             }

instance ( size ~ SeedSizeKES kesCrypto
         , KnownNat size
         , KESAlgorithm kesCrypto
         )
      => Arbitrary (WithConstrVerKeyKES size kesCrypto (VerKeyKES kesCrypto)) where
  arbitrary = mkVerKeyKESConstr <$> arbitrary
  shrink = shrinkWithConstrCtx


instance ( Crypto crypto
         , DSIGN.Signable (DSIGN crypto) (KES.OCertSignable crypto)
         , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
         , kesCrypto ~ KES crypto
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         )
      => Arbitrary (WithConstrVerKeyKES size kesCrypto (OCert crypto)) where
  arbitrary = do
    verKeyKES <- arbitrary
    n <- arbitrary
    seedColdKey :: PinnedSizedBytes (DSIGN.SeedSizeDSIGN (DSIGN crypto))
                <- arbitrary
    let !skCold = DSIGN.genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    period <- KES.KESPeriod <$> arbitrary
    return $ fmap (\vkKES -> KES.makeOCert vkKES n period skCold) verKeyKES
  shrink = shrinkWithConstrCtx


instance ( kesCrypto ~ KES crypto
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         , Arbitrary (WithConstrVerKeyKES size kesCrypto (OCert crypto))
         )
      => Arbitrary (WithConstrVerKeyKES size kesCrypto (SigOpCertificate crypto)) where
  arbitrary = fmap SigOpCertificate <$> arbitrary
  shrink = shrinkWithConstrCtx


instance ( Crypto crypto
         , kesCrypto ~ KES crypto
         , size ~ SeedSizeKES kesCrypto
         , Arbitrary (WithConstrVerKeyKES size kesCrypto (OCert crypto))
         )
      => Arbitrary (WithConstrVerKeyKES size kesCrypto (SigRawWithSignedBytes crypto)) where
  arbitrary = do
    sigRawId <- arbitrary
    sigRawBody <- arbitrary
    sigRawExpiresAt <- arbitrary
    opCert <- arbitrary
    sigRawKESPeriod <- arbitrary
    sigRawKESSignature <- arbitrary
    sigRawColdKey <- arbitrary
    return $ fmap (\cert -> let sigRawOpCertificate = SigOpCertificate cert
                                sigRaw = SigRaw {
                                    sigRawId,
                                    sigRawBody,
                                    sigRawKESPeriod,
                                    sigRawOpCertificate,
                                    sigRawColdKey,
                                    sigRawExpiresAt,
                                    sigRawKESSignature = undefined -- to be filled below
                                }
                                signedBytes = CBOR.toStrictByteString (encodeSigRaw' sigRaw)
                            in
                              SigRawWithSignedBytes {
                                sigRawSignedBytes = BL.fromStrict signedBytes,
                                sigRaw = sigRaw { sigRawKESSignature }
                              }
                  ) opCert
  shrink = shrinkWithConstrSigRawWithSignedBytes


-- Shrinking function for the above instance.  It's easier to get its type
-- signature right here.
--
-- It updates the KES signature accordingly.
--
shrinkWithConstrSigRawWithSignedBytes
  ::  forall crypto.
      Crypto crypto
  =>  WithConstrVerKeyKES (SeedSizeKES (KES crypto)) (KES crypto) (SigRawWithSignedBytes crypto)
  -> [WithConstrVerKeyKES (SeedSizeKES (KES crypto)) (KES crypto) (SigRawWithSignedBytes crypto)]
shrinkWithConstrSigRawWithSignedBytes = shrinkWithConstr updateFn shrinkSigRawWithSignedBytesFn
  where
    updateFn :: SigRawWithSignedBytes crypto
             -> VerKeyKES (KES crypto)
             -> SigRawWithSignedBytes crypto
    updateFn
      SigRawWithSignedBytes {
        sigRaw = sigRaw@SigRaw { sigRawOpCertificate = SigOpCertificate ocert },
        sigRawSignedBytes
      }
      ocertVkHot
      =
      let sigRaw' = sigRaw {
              sigRawOpCertificate = SigOpCertificate ocert { ocertVkHot }
            }
      in SigRawWithSignedBytes {
        sigRaw = sigRaw',
        sigRawSignedBytes
      }



-- | Pure shrinking function for `SigRawWithSignedBytes`.  It does not update
-- the KES signature, but it does update `sigRawSignedBytes`.
shrinkSigRawWithSignedBytesFn
  :: forall crypto.
     SigRawWithSignedBytes crypto
  -> [SigRawWithSignedBytes crypto]
shrinkSigRawWithSignedBytesFn SigRawWithSignedBytes { sigRaw } =
  [ SigRawWithSignedBytes { sigRaw = sigRaw',
                            sigRawSignedBytes = sigRawSignedBytes' }
  | sigRaw' <- shrinkSigRawFn sigRaw
  , let sigRawSignedBytes' = CBOR.toLazyByteString (encodeSigRaw' sigRaw')
  ]
shrinkSigRawFn :: SigRaw crypto -> [SigRaw crypto]
shrinkSigRawFn sig@SigRaw { sigRawId,
                             sigRawBody,
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
  [ sig { sigRawExpiresAt = sigRawExpiresAt' }
  | sigRawExpiresAt' <- shrink sigRawExpiresAt
  ]

instance Arbitrary SigColdKey where
  arbitrary = SigColdKey <$> arbitrary
  shrink = map SigColdKey . shrink . getSigColdKey


mkSigRawWithSignedBytes :: SigRaw crypto -> SigRawWithSignedBytes crypto
mkSigRawWithSignedBytes sigRaw =
    SigRawWithSignedBytes {
      sigRaw,
      sigRawSignedBytes
    }
  where
    sigRawSignedBytes = CBOR.toLazyByteString (encodeSigRaw' sigRaw)

-- NOTE: this function is not exposed in the main library on purpose.  We
-- should never construct `Sig` by serialising `SigRaw`.
--
mkSig :: forall crypto.
         ( Crypto crypto
         , Typeable crypto
         )
      => SigRawWithSignedBytes crypto -> Sig crypto
mkSig sigRawWithSignedBytes@SigRawWithSignedBytes { sigRaw } =
    SigWithBytes {
      sigRawBytes,
      sigRawWithSignedBytes
    }
  where
    sigRawBytes = CBOR.toLazyByteString (encodeSigRaw sigRaw)


-- encode only signed part
encodeSigRaw' :: SigRaw crypto
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
encodeSigRaw :: ( Crypto crypto
                , Typeable crypto
                )
             => SigRaw crypto
             -> CBOR.Encoding
encodeSigRaw sigRaw@SigRaw { sigRawKESSignature, sigRawOpCertificate, sigRawColdKey } =
     encodeSigRaw' sigRaw
  <> CBOR.encodeBytes (getSigKESSignature sigRawKESSignature)
  <> toCBOR (getSigOpCertificate sigRawOpCertificate)
  <> CBOR.encodeBytes (getSigColdKey sigRawColdKey)


shrinkSigFn :: forall crypto.
               ( Crypto crypto
               , Typeable crypto
               )
            => Sig crypto -> [Sig crypto]
shrinkSigFn SigWithBytes {sigRawWithSignedBytes = SigRawWithSignedBytes { sigRaw, sigRawSignedBytes } } =
    mkSig . (\sigRaw' -> SigRawWithSignedBytes { sigRaw = sigRaw', sigRawSignedBytes }) <$> shrinkSigRawFn sigRaw

instance ( Crypto crypto
         , Typeable crypto
         , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
         , DSIGN.Signable (DSIGN crypto) (KES.OCertSignable crypto)
         , kesCrypto ~ KES crypto
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         )
      => Arbitrary (WithConstrVerKeyKES size kesCrypto (Sig crypto)) where
  arbitrary = fmap mkSig <$> arbitrary
  shrink = shrinkWithConstr updateSigFn shrinkSigFn

updateSigFn :: Sig crypto -> VerKeyKES (KES crypto) -> Sig crypto
updateSigFn
  sig@Sig {sigOpCertificate = SigOpCertificate opCert}
  ocertVkHot
  =
  sig { sigOpCertificate = SigOpCertificate opCert { ocertVkHot } }


instance ( kesCrypto ~ KES crypto
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         , Arbitrary (WithConstrVerKeyKES size kesCrypto (Sig crypto))
         )
      => Arbitrary (WithConstrVerKeyKESList size kesCrypto (AnyMessage (SigSubmission crypto))) where
  arbitrary = oneof
    [ pure . constWithConstr $ AnyMessage MsgInit
    , constWithConstr . AnyMessage  <$>
        (MsgRequestTxIds SingBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , constWithConstr . AnyMessage <$>
        (MsgRequestTxIds SingNonBlocking
                     <$> arbitrary
                     <*> arbitrary)

    , constWithConstr . AnyMessage <$>
        MsgReplyTxIds <$> (BlockingReply . NonEmpty.fromList
                                         . map (second SizeInBytes)
                                         . QC.getNonEmpty
                                       <$> arbitrary)

    , constWithConstr . AnyMessage <$> MsgReplyTxIds <$> (NonBlockingReply . map (second SizeInBytes) <$> arbitrary)

    , constWithConstr . AnyMessage <$> MsgRequestTxs <$> arbitrary

    , listWithConstr (AnyMessage . MsgReplyTxs)
        <$> (arbitrary :: Gen [WithConstrVerKeyKES size kesCrypto (Sig crypto)])

    , constWithConstr .  AnyMessage <$> pure MsgDone
    ]
  shrink = shrinkWithConstr updateFn shrinkFn
    where
      updateFn :: AnyMessage (SigSubmission crypto) -> [VerKeyKES kesCrypto] -> AnyMessage (SigSubmission crypto)
      updateFn (AnyMessage (MsgReplyTxs txs)) vkKeyKESs = AnyMessage (MsgReplyTxs (zipWith updateSigFn txs vkKeyKESs))
      updateFn msg _ = msg

      shrinkFn :: AnyMessage (SigSubmission crypto) -> [AnyMessage (SigSubmission crypto)]
      shrinkFn = \case
        (AnyMessage MsgInit) -> []
        (AnyMessage (MsgRequestTxIds blockingStyle numTxIdsToAck numTxIdsToReq)) ->
          [ AnyMessage (MsgRequestTxIds blockingStyle numTxIdsToAck' numTxIdsToReq)
          | numTxIdsToAck' <- NumTxIdsToAck <$> shrink (getNumTxIdsToAck numTxIdsToAck)
          ]
          ++
          [ AnyMessage (MsgRequestTxIds blockingStyle numTxIdsToAck numTxIdsToReq')
          | numTxIdsToReq' <- NumTxIdsToReq <$> shrink (getNumTxIdsToReq numTxIdsToReq)
          ]
        (AnyMessage (MsgReplyTxIds (BlockingReply txids))) ->
          [ AnyMessage (MsgReplyTxIds (BlockingReply (NonEmpty.fromList txids')))
          | txids' <- shrinkList (const []) (NonEmpty.toList txids)
          , not (null txids')
          ]
        (AnyMessage (MsgReplyTxIds (NonBlockingReply txids))) ->
          [ AnyMessage (MsgReplyTxIds (NonBlockingReply txids'))
          | txids' <- shrinkList (const []) txids
          ]
        (AnyMessage (MsgRequestTxs txids)) ->
          [ AnyMessage (MsgRequestTxs txids')
          | txids' <- shrinkList (const []) txids
          ]
        (AnyMessage (MsgReplyTxs txs)) ->
          [ AnyMessage (MsgReplyTxs txs')
          | txs' <- shrinkList (const []) txs
          ]
        (AnyMessage MsgDone) -> []



type AnySigMessage crypto = WithConstrVerKeyKESList (SeedSizeKES (KES crypto)) (KES crypto) (AnyMessage (SigSubmission crypto))


prop_codec :: forall crypto. (Crypto crypto, Typeable crypto)
           => AnySigMessage crypto -> Property
prop_codec constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         . labelMsg msg
         $ runST (prop_anncodecM codecSigSubmission msg)


prop_codec_mockcrypto :: Blind (AnySigMessage MockCrypto)
                      -> Property
prop_codec_mockcrypto = prop_codec . getBlind

prop_codec_standardcrypto :: Blind (AnySigMessage StandardCrypto)
                          -> Property
prop_codec_standardcrypto = prop_codec . getBlind


prop_codec_id :: forall crypto. Crypto crypto
              => AnySigMessage crypto -> Property
prop_codec_id constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         . labelMsg msg
         $ runST (prop_codecM codecSigSubmissionId msg)

prop_codec_id_mockcrypto :: Blind (AnySigMessage MockCrypto)
                         -> Property
prop_codec_id_mockcrypto = prop_codec_id . getBlind

prop_codec_id_standardcrypto :: Blind (AnySigMessage StandardCrypto)
                             -> Property
prop_codec_id_standardcrypto = prop_codec_id . getBlind


prop_codec_splits2 :: forall crypto. (Crypto crypto, Typeable crypto)
                   => AnySigMessage crypto -> Property
prop_codec_splits2 constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         . labelMsg msg
         $ runST (prop_anncodec_splitsM splits2 codecSigSubmission msg)

prop_codec_splits2_mockcrypto :: Blind (AnySigMessage MockCrypto) -> Property
prop_codec_splits2_mockcrypto = prop_codec_splits2 . getBlind

prop_codec_splits2_standardcrypto :: Blind (AnySigMessage StandardCrypto) -> Property
prop_codec_splits2_standardcrypto = prop_codec_splits2 . getBlind


prop_codec_splits3 :: forall crypto. (Crypto crypto, Typeable crypto)
                   => AnySigMessage crypto -> Property
prop_codec_splits3 constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         . labelMsg msg
         $ runST (prop_anncodec_splitsM splits3 codecSigSubmission msg)

prop_codec_splits3_mockcrypto :: Blind (AnySigMessage MockCrypto) -> Property
prop_codec_splits3_mockcrypto = prop_codec_splits3 . getBlind

prop_codec_splits3_standardcrypto :: Blind (AnySigMessage StandardCrypto) -> Property
prop_codec_splits3_standardcrypto = prop_codec_splits3 . getBlind


prop_codec_cbor
  :: forall crypto.
     ( Crypto crypto
     , Typeable crypto
     )
  => AnySigMessage crypto
  -> Property
prop_codec_cbor constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         $ runST (prop_codec_cborM codecSigSubmission msg)

prop_codec_cbor_mockcrypto
  :: Blind (AnySigMessage MockCrypto)
  -> Property
prop_codec_cbor_mockcrypto = prop_codec_cbor . getBlind

prop_codec_cbor_standardcrypto
  :: Blind (AnySigMessage StandardCrypto)
  -> Property
prop_codec_cbor_standardcrypto = prop_codec_cbor . getBlind


prop_codec_valid_cbor
  :: forall crypto.
     ( Crypto crypto
     , Typeable crypto
     )
  => AnySigMessage crypto
  -> Property
prop_codec_valid_cbor constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         $ prop_codec_valid_cbor_encoding codecSigSubmission msg

prop_codec_valid_cbor_mockcrypto
  :: Blind (AnySigMessage MockCrypto)
  -> Property
prop_codec_valid_cbor_mockcrypto = prop_codec_valid_cbor . getBlind

prop_codec_valid_cbor_standardcrypto
  :: Blind (AnySigMessage StandardCrypto)
  -> Property
prop_codec_valid_cbor_standardcrypto = prop_codec_valid_cbor . getBlind
