{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Ouroboros.Network.SizeInBytes (SizeInBytes (..), WithBytes (..)) where

import Control.DeepSeq (NFData (..))
import Data.Monoid (Sum (..))
import Data.Typeable
import Data.Word (Word32)
import GHC.Generics
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Data.ByteString.Lazy

import Cardano.Binary (ToCBOR (toCBOR))
import Cardano.Binary qualified as Codec
import Cardano.Ledger.Binary.Decoding
import Data.Measure qualified as Measure
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Quiet (Quiet (..))

data WithBytes a = WithBytes {
  wbValue :: !a,
  wbBytes :: ShortByteString }
  deriving stock (Eq, Generic)
  deriving (NoThunks) via AllowThunksIn '["wbBytes"] (WithBytes a)

instance (Show a) => Show (WithBytes a) where
  show (WithBytes { wbValue }) = show wbValue
  
instance (Typeable a, DecCBOR (Annotator a)) => DecCBOR (Annotator (WithBytes a)) where
  decCBOR = do
    (Annotator getValue, Annotator getBytes) <- withSlice decCBOR
    return . Annotator $ \fullbytes -> WithBytes
                                         (getValue fullbytes)
                                         (toShort . toStrict . getBytes $ fullbytes)

instance Typeable a => ToCBOR (WithBytes a) where
  toCBOR (WithBytes { wbBytes })= Codec.encodePreEncoded (fromShort wbBytes)
  
newtype SizeInBytes = SizeInBytes { getSizeInBytes :: Word32 }
  deriving (Eq, Ord)
  deriving Show      via Quiet SizeInBytes
  deriving Enum      via Word32
  deriving Num       via Word32
  deriving Real      via Word32
  deriving Integral  via Word32
  deriving NoThunks  via Word32
  deriving Semigroup via Sum Word32
  deriving Monoid    via Sum Word32
  deriving Generic
  deriving newtype NFData
  deriving Measure.Measure        via Word32
  deriving Measure.BoundedMeasure via Word32
