{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Cardano.KESAgent.OCert
where

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Binary

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Builder.Extra as BSB

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (..))
import Data.Word

class ( KESAlgorithm (KES c)
      , KESSignAlgorithm IO (KES c)
      , DSIGNAlgorithm (DSIGN c)
      ) => Crypto c where
  type KES c :: *
  type DSIGN c :: *

newtype KESPeriod = KESPeriod {unKESPeriod :: Word}
  deriving (Eq, Generic, Ord, Typeable)
  deriving newtype (NoThunks, FromCBOR, ToCBOR)
  deriving (Show) via Quiet KESPeriod

-- | Signable part of an operational certificate
data OCertSignable c
  = OCertSignable !(VerKeyKES (KES c)) !Word64 !KESPeriod

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

instance ( Crypto c
         , Typeable c
         , Typeable (OCert c)
         ) => ToCBOR (OCert c) where
  toCBOR ocert =
    encodeVerKeyKES (ocertVkHot ocert)
      <> toCBOR (ocertN ocert)
      <> toCBOR (ocertKESPeriod ocert)
      <> encodeSignedDSIGN (ocertSigma ocert)

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

