{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE BlockArguments       #-}
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
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module DMQ.Protocol.SigSubmission.Test where

import Codec.CBOR.Encoding qualified as CBOR
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (zipWithM, (>=>))
import Control.Monad.ST (runST)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Typeable
import Data.Word (Word32)
import GHC.TypeNats (KnownNat)
import System.IO.Unsafe (unsafePerformIO)

import Network.TypedProtocol.Codec
import Network.TypedProtocol.Codec.Properties hiding (prop_codec)

import Cardano.Crypto.DSIGN.Class (DSIGNAlgorithm, SignKeyDSIGN,
           deriveVerKeyDSIGN, encodeVerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Crypto.KES.Class (KESAlgorithm (..), VerKeyKES, encodeSigKES)
import Cardano.Crypto.KES.Class qualified as KES
import Cardano.Crypto.PinnedSizedBytes (PinnedSizedBytes, psbToByteString)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.Evolution qualified as KES
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.KES.OCert qualified as KES
import Cardano.KESAgent.Protocols.StandardCrypto (MockCrypto, StandardCrypto)
import Test.Crypto.Instances

import DMQ.Protocol.SigSubmission.Codec
import DMQ.Protocol.SigSubmission.Type

import Ouroboros.Network.Protocol.TxSubmission2.Test (labelMsg)

import Test.Ouroboros.Network.Protocol.Utils (prop_codec_cborM,
           prop_codec_valid_cbor_encoding, splits2)

import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty
import Test.Tasty.QuickCheck as QC


tests :: TestTree
tests =
  testGroup "DMQ.Protocol"
    [ testGroup "SigSubmission"
      [ testGroup "Codec"
        [ testGroup "MockCrypto"
          [ testProperty "OCert"               prop_codec_ocert_mockcrypto
          , testProperty "Sig"                 prop_codec_sig_mockcrypto
          , testProperty "codec"               prop_codec_mockcrypto
          , testProperty "codec id"            prop_codec_id_mockcrypto
          , testProperty "codec 2-splits"    $ withMaxSize 20
                                             $ withMaxSuccess 20
                                               prop_codec_splits2_mockcrypto
          -- MockCrypto produces too large messages for this test to run:
          -- , testProperty "codec 3-splits"    $ withMaxSize 10
          --                                    $ withMaxSuccess 10
          --                                      prop_codec_splits3_mockcrypto
          , testProperty "codec cbor"          prop_codec_cbor_mockcrypto
          , testProperty "codec valid cbor"    prop_codec_valid_cbor_mockcrypto
          , testProperty "OCert"               prop_codec_cbor_mockcrypto
          ]
        , testGroup "StandardCrypto"
          [ testProperty "OCert"               prop_codec_ocert_standardcrypto
          , testProperty "Sig"                 prop_codec_sig_standardcrypto
          , testProperty "codec"               prop_codec_standardcrypto
          , testProperty "codec id"            prop_codec_id_standardcrypto
          , testProperty "codec 2-splits"    $ withMaxSize 20
                                             $ withMaxSuccess 20
                                               prop_codec_splits2_standardcrypto
          -- StandardCrypto produces too large messages for this test to run:
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
    , testGroup "Crypto"
      [ testGroup "MockCrypto"
        [ testProperty "KES sign verify"     prop_sign_verify_mockcrypto
        , testProperty "validateSig"         prop_validateSig_mockcrypto
        ]
      , testGroup "StandardCrypto"
        [ testProperty "KES sign verify"     prop_sign_verify_standardcrypto
        , testProperty "validateSig"         prop_validateSig_standardcrypto
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


-- | Make a KES key pair.
--
mkKeysKES
  :: forall kesCrypto.
     KESAlgorithm kesCrypto
  => PinnedSizedBytes (SeedSizeKES kesCrypto)
  -> IO (SignKeyKES kesCrypto, VerKeyKES kesCrypto)
mkKeysKES seed =
    withMLockedSeedFromPSB seed $ \mseed -> do
      snKESKey <- KES.genKeyKES mseed
      (snKESKey,) <$> deriveVerKeyKES snKESKey


-- | The idea of this data type is to go around limitation of QuickCheck `Gen`
-- type, which does not allow IO actions.  So instead we generate some random
-- context (e.g. key seed) and then the data is created when the property
-- runs.
--
-- Keeping the `key` seprate allows to have access to it when shrinking, see
-- `shrinkWithConstr`, this is important when the signed data is shrinked and
-- we need to update a KES signature as well.
--
-- However the limitation is shrinking: it requires `unsafePerformIO` anyway,
-- see `shrinkWithConstr`.
--
-- TODO: to avoid complexity can we use `UnsoundPureKESAlgorithm` instead of
-- `KESAlgorithm`?
--
data WithConstr ctx key a =
    WithConstr { constr :: key -> IO a,
                 mkKey  :: ctx -> IO key,
                 ctx    :: ctx
               }
deriving instance Functor (WithConstr ctx key)

withConstrBind :: WithConstr ctx key a -> (a -> IO b) -> WithConstr ctx key b
withConstrBind WithConstr { constr, mkKey, ctx } fn =
    WithConstr { constr = constr >=> fn,
                 mkKey,
                 ctx
               }

runWithConstr :: WithConstr ctx key a -> IO a
runWithConstr WithConstr { constr, mkKey, ctx } = mkKey ctx >>= constr

constrWithKeys
  :: (keys -> IO a)
  -> WithConstr ctx keys keys
  -> WithConstr ctx keys a
constrWithKeys f WithConstr { constr, mkKey, ctx } =
    WithConstr { constr = constr >=> f,
                 mkKey,
                 ctx
               }

constWithConstr :: a -> WithConstr [ctx] [key] a
constWithConstr a =
    WithConstr { constr = const (pure a),
                 mkKey  = \_ -> pure [],
                 ctx = []
               }

listWithConstr :: forall ctx key a b.
                  ([a] -> b)
               -> [WithConstr  ctx   key  a]
               ->  WithConstr [ctx] [key] b
listWithConstr constr' as =
    WithConstr {
      constr = \keys -> constr' <$> zipWithM ($) constrs keys,
      mkKey  = \ctxs -> zipWithM ($) mkKeys ctxs,
      ctx    = ctx <$> as

    }
  where
    constrs :: [key -> IO a]
    constrs = constr <$> as

    mkKeys :: [ctx -> IO key]
    mkKeys = mkKey <$> as


-- shrink ctx
shrinkWithConstrCtx :: Arbitrary ctx => WithConstr ctx key a -> [WithConstr ctx key a]
shrinkWithConstrCtx constr@WithConstr { ctx } =
  [ constr { ctx = ctx' }
  | ctx' <- shrink ctx
  ]


sequenceWithConstr
  :: (a -> key -> IO a)
  ->     WithConstr ctx key [a]
  -> IO [WithConstr ctx key  a]
sequenceWithConstr update constr@WithConstr { mkKey, ctx } = do
    as <- runWithConstr constr
    return [ WithConstr { constr = update a, mkKey, ctx }
           | a <- as
           ]


-- unsafePerformIO :(
shrinkWithConstr :: Arbitrary ctx
                 => (a -> key -> IO a)
                 -> (a -> [a])
                 ->  WithConstr ctx key a
                 -> [WithConstr ctx key a]
shrinkWithConstr update shrinker constr =
       unsafePerformIO (sequenceWithConstr update $ shrinker <$> constr)
    ++ shrinkWithConstrCtx constr

shrinkWithConstr' :: Arbitrary ctx
                  => (a -> key ->  a)
                  -> (a -> [a])
                  -> WithConstr ctx key a
                  -> [WithConstr ctx key a]
shrinkWithConstr' update = shrinkWithConstr (\a k -> pure (update a k))


type KESCTX            size          = PinnedSizedBytes size
type WithConstrKES     size crypto a = WithConstr (KESCTX size)  (SignKeyKES crypto, VerKeyKES crypto)  a
type WithConstrKESList size crypto a = WithConstr [KESCTX size] [(SignKeyKES crypto, VerKeyKES crypto)] a


mkKeysKESConstr
  :: forall kesCrypto.
     KESAlgorithm kesCrypto
  => KESCTX (SeedSizeKES kesCrypto)
  -> WithConstrKES (SeedSizeKES kesCrypto)
                    kesCrypto
                   (SignKeyKES kesCrypto, VerKeyKES kesCrypto)
mkKeysKESConstr ctx =
  WithConstr { constr = pure,
               mkKey  = mkKeysKES,
               ctx
             }

instance ( size ~ SeedSizeKES kesCrypto
         , KnownNat size
         , KESAlgorithm kesCrypto
         )
      => Arbitrary (WithConstrKES size kesCrypto (SignKeyKES kesCrypto, VerKeyKES kesCrypto)) where
  arbitrary = mkKeysKESConstr <$> arbitrary
  shrink = shrinkWithConstrCtx


-- | An auxiliary data type to hold KES keys along with an OCert, payload and
-- its KES signature.
data CryptoCtx crypto = CryptoCtx {
    snKESKey :: SignKeyKES (KES crypto),
    -- ^ signing KES key
    vnKESKey :: VerKeyKES (KES crypto),
    -- ^ verification KES key
    coldKey  :: SignKeyDSIGN (DSIGN crypto),
    -- ^ signing cold key
    ocert    :: OCert crypto
    -- ^ ocert
  }


instance ( Crypto crypto
         , DSIGN.Signable (DSIGN crypto) (KES.OCertSignable crypto)
         , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
         , ContextKES (KES crypto) ~ ()
         , kesCrypto ~ KES crypto
         , KESAlgorithm kesCrypto
         , Signable kesCrypto ByteString
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         )
      => Arbitrary (WithConstrKES size kesCrypto (CryptoCtx crypto)) where
  arbitrary = do
    withKeys <- arbitrary
    n <- arbitrary
    seedColdKey :: PinnedSizedBytes (DSIGN.SeedSizeDSIGN (DSIGN crypto))
                <- arbitrary
    let !coldKey = DSIGN.genKeyDSIGN (mkSeedFromBytes . psbToByteString $ seedColdKey)
    period <- KES.KESPeriod <$> arbitrary
    return $ constrWithKeys
             (\(snKESKey, vnKESKey) ->
               return $ CryptoCtx {
                 snKESKey,
                 vnKESKey,
                 coldKey,
                 ocert = KES.makeOCert vnKESKey n period coldKey
               })
             withKeys
  shrink = shrinkWithConstrCtx


instance ( Crypto crypto
         , kesCrypto ~ KES crypto
         , ContextKES kesCrypto ~ ()
         , size ~ SeedSizeKES kesCrypto
         , Signable kesCrypto ByteString
         , dsignCrypto ~ DSIGN crypto
         , DSIGNAlgorithm dsignCrypto
         , Arbitrary (WithConstrKES size kesCrypto (CryptoCtx crypto))
         )
      => Arbitrary (WithConstrKES size kesCrypto (SigRawWithSignedBytes crypto)) where
  arbitrary = do
    sigRawId <- arbitrary
    sigRawExpiresAt <- arbitrary
    let maxKESOffset :: Word
        maxKESOffset = totalPeriodsKES (Proxy :: Proxy kesCrypto)
    -- offset since `ocertKESPeriod`, so that the signature is still valid
    kesOffset <- arbitrary `suchThat` (< maxKESOffset)
    payload <- arbitrary
    crypto <- arbitrary
    return $ withConstrBind crypto \CryptoCtx {ocert, coldKey, snKESKey} -> do
      let sigRawOpCertificate :: SigOpCertificate crypto
          sigRawOpCertificate = SigOpCertificate ocert

          sigRawBody :: SigBody
          sigRawBody = SigBody payload

          sigRawColdKey :: SigColdKey crypto
          sigRawColdKey = SigColdKey $ deriveVerKeyDSIGN coldKey

          sigRawKESPeriod :: KESPeriod
          sigRawKESPeriod = KESPeriod $ unKESPeriod (ocertKESPeriod ocert)
                                      + kesOffset

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

      -- evolve the key to the target period
      mbSnKESKey <- KES.updateKESTo () sigRawKESPeriod ocert (KES.SignKeyWithPeriodKES snKESKey 0)
      case mbSnKESKey of
        Just (KES.SignKeyWithPeriodKES snKESKey' _) -> do
          -- signed bytes with the snKESKey'
          sigRawKESSignature
             <- SigKESSignature
            <$> KES.signKES () kesOffset signedBytes snKESKey'
          return SigRawWithSignedBytes {
              sigRawSignedBytes = BL.fromStrict signedBytes,
              sigRaw = sigRaw { sigRawKESSignature }
            }
        Nothing ->
          error $ "arbitrary SigRawWithSignedBytes: could not evolve KES key to the target period by KES offset: "
               ++ show kesOffset

  shrink = shrinkWithConstrSigRawWithSignedBytes


-- Shrinking function for the above instance.  It's easier to get its type
-- signature right here.
--
-- It updates the KES signature accordingly.
--
shrinkWithConstrSigRawWithSignedBytes
  ::  forall crypto.
      ( Crypto crypto
      , ContextKES (KES crypto) ~ ()
      , Signable (KES crypto) ByteString
      )
  =>  WithConstrKES (SeedSizeKES (KES crypto)) (KES crypto) (SigRawWithSignedBytes crypto)
  -> [WithConstrKES (SeedSizeKES (KES crypto)) (KES crypto) (SigRawWithSignedBytes crypto)]
shrinkWithConstrSigRawWithSignedBytes = shrinkWithConstr updateFn shrinkSigRawWithSignedBytesFn
  where
    updateFn :: SigRawWithSignedBytes crypto
             -> (SignKeyKES (KES crypto), VerKeyKES (KES crypto))
             -> IO (SigRawWithSignedBytes crypto)
    updateFn
      SigRawWithSignedBytes {
        sigRaw = sigRaw@SigRaw { sigRawOpCertificate = SigOpCertificate ocert,
                                 sigRawKESPeriod
                               },
        sigRawSignedBytes
      }
      (snKeyKES, ocertVkHot)
      = do
      let sigRaw' = sigRaw {
              sigRawOpCertificate = SigOpCertificate ocert { ocertVkHot }
            }
      -- update KES key to sigRawKESPeriod
      Just (KES.SignKeyWithPeriodKES snKeyKES' _)
        <- KES.updateKESTo () sigRawKESPeriod ocert (KES.SignKeyWithPeriodKES snKeyKES 0)
      -- sign the message
      sign <- KES.signKES () (KES.unKESPeriod sigRawKESPeriod - KES.unKESPeriod (ocertKESPeriod ocert))
                             (BL.toStrict sigRawSignedBytes)
                             snKeyKES'
      pure $ SigRawWithSignedBytes {
        sigRaw = sigRaw' { sigRawKESSignature = SigKESSignature sign },
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


-- | Pure shrinking function for `SigRaw`.  It does not update the KES
-- signature.
--
shrinkSigRawFn :: SigRaw crypto -> [SigRaw crypto]
shrinkSigRawFn sig@SigRaw { sigRawId,
                            sigRawBody,
                            sigRawKESPeriod,
                            sigRawExpiresAt,
                            sigRawOpCertificate = SigOpCertificate ocert
                          } =
  [ sig { sigRawId = sigRawId' }
  | sigRawId' <- shrink sigRawId
  ]
  ++
  [ sig { sigRawBody = sigRawBody' }
  | sigRawBody' <- shrink sigRawBody
  ]
  ++
  [ sig { sigRawKESPeriod = sigRawKESPeriod' }
  | sigRawKESPeriod' <- KESPeriod <$> shrink (unKESPeriod sigRawKESPeriod)
  , sigRawKESPeriod' >= ocertKESPeriod ocert
  ]
  ++
  [ sig { sigRawExpiresAt = sigRawExpiresAt' }
  | sigRawExpiresAt' <- shrink sigRawExpiresAt
  ]


-- NOTE: this function is not exposed in the main library on purpose.  We
-- should never construct `Sig` by serialising `SigRaw`.
--
mkSig :: forall crypto. Crypto crypto
      => SigRawWithSignedBytes crypto -> Sig crypto
mkSig sigRawWithSignedBytes@SigRawWithSignedBytes { sigRaw } =
    SigWithBytes {
      sigRawBytes,
      sigRawWithSignedBytes
    }
  where
    sigRawBytes = CBOR.toLazyByteString (encodeSigRaw  sigRaw)


-- encode only signed part
encodeSigRaw' :: SigRaw crypto
              -> CBOR.Encoding
encodeSigRaw' SigRaw {
    sigRawId,
    sigRawBody,
    sigRawKESPeriod,
    sigRawExpiresAt
  }
  =  CBOR.encodeListLen 4
  <> encodeSigId sigRawId
  <> CBOR.encodeBytes (getSigBody sigRawBody)
  <> CBOR.encodeWord (unKESPeriod sigRawKESPeriod)
  <> CBOR.encodeWord32 (floor sigRawExpiresAt)

-- encode together with KES signature, OCert and cold key.
encodeSigRaw :: Crypto crypto
             => SigRaw crypto
             -> CBOR.Encoding
encodeSigRaw sigRaw@SigRaw { sigRawKESSignature, sigRawOpCertificate, sigRawColdKey } =
     CBOR.encodeListLen 4
  <> encodeSigRaw' sigRaw
  <> encodeSigKES (getSigKESSignature sigRawKESSignature)
  <> encodeSigOpCertificate sigRawOpCertificate
  <> encodeVerKeyDSIGN (getSigColdKey sigRawColdKey)

-- note: KES signature is updated by updateSigFn
shrinkSigFn :: forall crypto.
               Crypto crypto
            => Sig crypto -> [Sig crypto]
shrinkSigFn SigWithBytes {sigRawWithSignedBytes = SigRawWithSignedBytes { sigRaw, sigRawSignedBytes } } =
    mkSig . (\sigRaw' -> SigRawWithSignedBytes { sigRaw = sigRaw', sigRawSignedBytes }) <$> shrinkSigRawFn sigRaw


updateSigFn :: forall crypto.
               KESAlgorithm (KES crypto)
            => ContextKES (KES crypto) ~ ()
            => Signable (KES crypto) ByteString
            => Sig crypto
            -> (SignKeyKES (KES crypto), VerKeyKES (KES crypto))
            -> IO (Sig crypto)
updateSigFn
  sig@Sig { sigOpCertificate = SigOpCertificate opCert,
            sigBody          = SigBody body
          }
  (snKESKey, vnKESKey)
  = do
  signature <- KES.signKES () (KES.unKESPeriod (ocertKESPeriod opCert)) body snKESKey
  return $ sig { sigOpCertificate = SigOpCertificate opCert { ocertVkHot = vnKESKey},
                 sigKESSignature  = SigKESSignature signature
               }


instance ( Crypto crypto
         , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
         , DSIGN.Signable (DSIGN crypto) (KES.OCertSignable crypto)
         , kesCrypto ~ KES crypto
         , ContextKES kesCrypto ~ ()
         , Signable kesCrypto ByteString
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         )
      => Arbitrary (WithConstrKES size kesCrypto (Sig crypto)) where
  arbitrary = fmap mkSig <$> arbitrary
  shrink = shrinkWithConstr updateSigFn shrinkSigFn


instance ( kesCrypto ~ KES crypto
         , KESAlgorithm kesCrypto
         , ContextKES kesCrypto ~ ()
         , Signable kesCrypto ByteString
         , size ~ SeedSizeKES kesCrypto
         , KnownNat size
         , Arbitrary (WithConstrKES size kesCrypto (Sig crypto))
         )
      => Arbitrary (WithConstrKESList size kesCrypto (AnyMessage (SigSubmission crypto))) where
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
        <$> (arbitrary :: Gen [WithConstrKES size kesCrypto (Sig crypto)])

    , constWithConstr .  AnyMessage <$> pure MsgDone
    ]
  shrink = shrinkWithConstr updateFn shrinkFn
    where
      updateFn :: AnyMessage (SigSubmission crypto)
               -> [(SignKeyKES kesCrypto, VerKeyKES kesCrypto)]
               -> IO (AnyMessage (SigSubmission crypto))
      updateFn (AnyMessage (MsgReplyTxs txs)) keys = do
        sigs <- traverse (uncurry updateSigFn) (zip txs keys)
        return $ AnyMessage (MsgReplyTxs sigs)
      updateFn msg _ = pure msg

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


prop_codec_ocert
  :: forall crypto. Crypto crypto
  => WithConstrKES (SeedSizeKES (KES crypto)) (KES crypto) (CryptoCtx crypto)
  -> Property
prop_codec_ocert constr = ioProperty $ do
  CryptoCtx { ocert } <- runWithConstr constr
  return . counterexample (show ocert)
         $ let encoded = CBOR.toLazyByteString (encodeSigOpCertificate (SigOpCertificate ocert))
           in case CBOR.deserialiseFromBytes decodeSigOpCertificate encoded of
                Left err -> counterexample (show err) False
                Right (bytes, SigOpCertificate ocert') ->
                       ocert === ocert'
                  .&&. BL.null bytes

prop_codec_ocert_mockcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES MockCrypto)) (KES MockCrypto) (CryptoCtx MockCrypto))
  -> Property
prop_codec_ocert_mockcrypto = prop_codec_ocert . getBlind

prop_codec_ocert_standardcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES StandardCrypto)) (KES StandardCrypto) (CryptoCtx StandardCrypto))
  -> Property
prop_codec_ocert_standardcrypto = prop_codec_ocert . getBlind


-- Verify `Sig` encoding/decoding roundtrip:
-- * `SigRaw` is preserved by encoding/decoding.
-- * bytes match the encoding of `encodeSig`.
-- * signed bytes match the encoding of `encodeSigRaw'`.
prop_codec_sig
  :: forall crypto. Crypto crypto
  => WithConstrKES (SeedSizeKES (KES crypto)) (KES crypto) (Sig crypto)
  -> Property
prop_codec_sig constr = ioProperty $ do
  sig <- runWithConstr constr
  return . counterexample (show sig)
         . counterexample ("sigRawBytes (hex): \"" ++ show (CBORBytes (sigRawBytes sig)) ++ "\"")
         $ let encoded = CBOR.toLazyByteString (encodeSigRaw (sigRaw (sigRawWithSignedBytes sig)))
           in counterexample ("encoded (hex): \"" ++ show (CBORBytes encoded) ++ "\"") $
             case CBOR.deserialiseFromBytes (decodeSig @crypto) encoded of
               Left err ->
                 counterexample (show err) False
               Right (leftovers, f) ->
                 -- split the properties for better counterexample reporting

                     -- SigRaw is preserved
                      sigRaw (sigRawWithSignedBytes sig)
                      ===
                      sigRaw (sigRawWithSignedBytes (mkSig (f encoded)))

                      -- signed bytes are preserved
                 .&&. sigRawSignedBytes (sigRawWithSignedBytes sig)
                      ===
                      sigRawSignedBytes (sigRawWithSignedBytes (mkSig (f encoded)))

                     -- bytes are preserved
                 .&&. sigRawBytes sig
                      ===
                      sigRawBytes (mkSig (f encoded))

                      -- no leftovers
                 .&&. BL.null leftovers

prop_codec_sig_mockcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES MockCrypto)) (KES MockCrypto) (Sig MockCrypto))
  -> Property
prop_codec_sig_mockcrypto = prop_codec_sig . getBlind

prop_codec_sig_standardcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES MockCrypto)) (KES MockCrypto) (Sig MockCrypto))
  -> Property
prop_codec_sig_standardcrypto = prop_codec_sig . getBlind


type AnySigMessage crypto = WithConstrKESList (SeedSizeKES (KES crypto)) (KES crypto) (AnyMessage (SigSubmission crypto))


prop_codec :: forall crypto. Crypto crypto
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


prop_codec_splits2 :: forall crypto. Crypto crypto
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


{-
-- TODO: we need a different splits3 function that does not explore all the
-- ways of splitting a message into three chunks.
prop_codec_splits3 :: forall crypto. Crypto crypto
                   => AnySigMessage crypto -> Property
prop_codec_splits3 constr = ioProperty $ do
  msg <- runWithConstr constr
  return . counterexample (show msg)
         . labelMsg msg
         $ runST (prop_anncodec_splitsM splits3 codecSigSubmission msg)

prop_codec_splits3_mockcrypto :: Blind (AnySigMessage MockCrypto) -> Property
prop_codec_splits3_mockcrypto = prop_codec_splits3 . getBlind
-}

prop_codec_splits3_standardcrypto :: Blind (AnySigMessage StandardCrypto) -> Property
prop_codec_splits3_standardcrypto = prop_codec_splits3 . getBlind


prop_codec_cbor
  :: forall crypto. Crypto crypto
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
  :: forall crypto. Crypto crypto
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


-- | Check that the KES signature is valid.
--
prop_validateSig
  :: ( Crypto crypto
     , DSIGN.ContextDSIGN (DSIGN crypto) ~ ()
     , DSIGN.Signable (DSIGN crypto) (KES.OCertSignable crypto)
     , KES.ContextKES (KES crypto) ~ ()
     , KES.Signable (KES crypto) ByteString
     )
  => WithConstrKES size kesCrypt (Sig crypto)
  -> Property
prop_validateSig constr = ioProperty $ do
    sig <- runWithConstr constr
    return $ case validateSig KES.defEvolutionConfig sig of
      Left err -> counterexample ("KES seed: " ++ show (ctx constr))
                . counterexample ("KES vk key: " ++ show (ocertVkHot . getSigOpCertificate . sigOpCertificate $ sig))
                . counterexample (show sig)
                . counterexample (show err)
                $ False
      Right () -> property True

prop_validateSig_mockcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES MockCrypto)) (KES MockCrypto) (Sig MockCrypto))
  -> Property
prop_validateSig_mockcrypto = prop_validateSig . getBlind

-- TODO: FAILS, why?
prop_validateSig_standardcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES StandardCrypto)) (KES StandardCrypto) (Sig StandardCrypto))
  -> Property
prop_validateSig_standardcrypto = prop_validateSig . getBlind


-- | Sign & verify a payload with KES keys.
--
prop_sign_verify
  :: ( Crypto crypto
     , ContextKES (KES crypto) ~ ()
     , Signable (KES crypto) ByteString
     )
  => WithConstrKES (SeedSizeKES (KES crypto)) (KES crypto) (CryptoCtx crypto)
  -- ^ KES keys
  -> ByteString
  -- ^ payload
  -> Property
prop_sign_verify constr payload = ioProperty $ do
    CryptoCtx { snKESKey, vnKESKey } <- runWithConstr constr
    signed <- KES.signKES () 0 payload snKESKey
    let res = KES.verifyKES () vnKESKey 0 payload signed
    return $ counterexample "KES signature does not verify"
           $ case res of
              Left err -> counterexample (show err)
                        . counterexample ("vnKESKey:  " ++ show vnKESKey)
                        . counterexample ("signature: " ++ show signed)
                        $ False
              Right () -> property True


prop_sign_verify_mockcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES MockCrypto)) (KES MockCrypto) (CryptoCtx MockCrypto))
  -> ByteString
  -> Property
prop_sign_verify_mockcrypto = prop_sign_verify . getBlind

prop_sign_verify_standardcrypto
  :: Blind (WithConstrKES (SeedSizeKES (KES StandardCrypto)) (KES StandardCrypto) (CryptoCtx StandardCrypto))
  -> ByteString
  -> Property
prop_sign_verify_standardcrypto = prop_sign_verify . getBlind
