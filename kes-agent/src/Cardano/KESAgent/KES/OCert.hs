{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A stripped-down version of the @OCert@ and @Crypto@ types used in
-- @cardano-ledger@. We only replicate what we need here, so as to avoid
-- depending on @cardano-ledger@.
--
-- In order to keep things simple, we do /not/ use the same CBOR serialization
-- format for the 'OCert' type; this would require using or replicating the
-- @CBORGroup@ functionality from @cardano-ledger@, which I feel would be too
-- much to replicate here, and since the KES Agent protocols are not
-- performance critical, the small overhead introduced by using the default
-- CBOR serialization seems like an acceptable tradeoff.
module Cardano.KESAgent.KES.OCert
  where

import Cardano.KESAgent.KES.Crypto

import Cardano.Binary
import Cardano.Crypto.DSIGN.Class as DSIGN
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Util ( SignableRepresentation (..) )

import Control.Monad ( when )
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Builder.Extra qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Typeable ( Typeable )
import Data.Word
import GHC.Generics ( Generic )
import NoThunks.Class ( NoThunks (..) )
import Quiet ( Quiet (..) )

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, Typeable)
  deriving newtype (NoThunks, FromCBOR, ToCBOR)
  deriving (Show) via Quiet KESPeriod

-- | Signable part of an operational certificate
data OCertSignable c
  = OCertSignable !(VerKeyKES (KES c)) !Word64 !KESPeriod

instance (Typeable c, ToCBOR (VerKeyKES (KES c))) => ToCBOR (OCertSignable c) where
  toCBOR (OCertSignable vk opcertN kesPeriod) =
    toCBOR (vk, opcertN, kesPeriod)

instance (Typeable c, FromCBOR (VerKeyKES (KES c))) => FromCBOR (OCertSignable c) where
  fromCBOR = do
    (vk, opcertN, kesPeriod) <- fromCBOR
    return $ OCertSignable vk opcertN kesPeriod

instance
  forall c.
  Crypto c =>
  SignableRepresentation (OCertSignable c)
  where
  getSignableRepresentation (OCertSignable vk counter period) =
    LBS.toStrict . BSB.toLazyByteString $
      BSB.byteStringCopy (rawSerialiseVerKeyKES vk)
        <> BSB.word64BE counter
        <> BSB.word64BE (fromIntegral $ unKESPeriod period)

-- | Operational certificate (\"opcert\")
data OCert c = OCert
  { -- | The operational hot key
    ocertVkHot :: !(VerKeyKES (KES c)),
    -- | counter
    ocertN :: !Word64,
    -- | Start of key evolving signature period
    ocertKESPeriod :: !KESPeriod,
    -- | Signature of block operational certificate content
    ocertSigma :: !(SignedDSIGN (DSIGN c) (OCertSignable c))
  }
  deriving (Generic, Typeable)

deriving instance (DSIGNAlgorithm (DSIGN c), Show (VerKeyKES (KES c))) => Show (OCert c)
deriving instance (DSIGNAlgorithm (DSIGN c), Eq (VerKeyKES (KES c))) => Eq (OCert c)

-- | NB this CBOR format is incompatible with the one defined in
-- cardano-ledger.
instance ( Crypto c
         , Typeable c
         , Typeable (OCert c)
         ) => ToCBOR (OCert c) where
  toCBOR ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)

-- | NB this CBOR format is incompatible with the one defined in
-- cardano-ledger.
instance
  ( Crypto c
  , Typeable c
  , Typeable (OCert c)
  ) => FromCBOR (OCert c)
  where
  fromCBOR =
    OCert
      <$> decodeVerKeyKES
      <*> fromCBOR
      <*> fromCBOR
      <*> decodeSignedDSIGN

-- | Generate an operational certificate for a given hot key.
makeOCert :: Crypto c
          => ContextDSIGN (DSIGN c) ~ ()
          => DSIGN.Signable (DSIGN c) (OCertSignable c)
          => VerKeyKES (KES c)
          -> Word64
          -> KESPeriod
          -> SignKeyDSIGN (DSIGN c)
          -> OCert c
makeOCert vkHot n kesPeriod skCold =
  OCert vkHot n kesPeriod sig
  where
    signable = OCertSignable vkHot n kesPeriod
    sig = signedDSIGN () signable skCold

validateOCert :: forall c
               . Crypto c
              => ContextDSIGN (DSIGN c) ~ ()
              => DSIGN.Signable (DSIGN c) (OCertSignable c)
              => VerKeyDSIGN (DSIGN c)
              -> VerKeyKES (KES c)
              -> OCert c
              -> Either String ()
validateOCert vkCold vkHot ocert = do
  when (ocertVkHot ocert /= vkHot) (Left "Verification key does not match")
  verifyDSIGN () vkCold signable sig
  where
    signable :: OCertSignable c
    signable = OCertSignable (ocertVkHot ocert) (ocertN ocert) (ocertKESPeriod ocert)

    sig :: SigDSIGN (DSIGN c)
    SignedDSIGN sig = ocertSigma ocert
