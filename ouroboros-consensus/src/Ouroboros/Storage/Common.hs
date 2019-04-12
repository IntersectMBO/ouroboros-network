{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Ouroboros.Storage.Common (
    -- * Epochs
    EpochNo(..)
  , EpochSize(..)
    -- * File formats
  , SlotOffset
    -- * Indexing
  , Tip(..)
  , tipIsGenesis
    -- * Serialisation
  , encodeTip
  , decodeTip
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Word
import           GHC.Generics

{-------------------------------------------------------------------------------
  Epochs
-------------------------------------------------------------------------------}

-- | An epoch, i.e. the number of the epoch.
newtype EpochNo = EpochNo { unEpochNo :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic)

newtype EpochSize = EpochSize { unEpochSize :: Word64 }
  deriving (Eq, Ord, Enum, Num, Show, Generic, Real, Integral)

{-------------------------------------------------------------------------------
  File formats
-------------------------------------------------------------------------------}

-- | The offset of a slot in an index file.
type SlotOffset = Word64

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Tip of the chain
data Tip r = Tip r | TipGen
  deriving (Show, Eq, Generic)

tipIsGenesis :: Tip r -> Bool
tipIsGenesis TipGen  = True
tipIsGenesis (Tip _) = False

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

instance Serialise r => Serialise (Tip r) where
  encode = encodeTip encode
  decode = decodeTip decode

encodeTip :: (r     -> Encoding)
          -> (Tip r -> Encoding)
encodeTip encodeR tip =
    case tip of
      TipGen -> Enc.encodeListLen 0
      Tip r  -> Enc.encodeListLen 1 <> encodeR r

decodeTip :: (forall s. Decoder s r)
          -> (forall s. Decoder s (Tip r))
decodeTip decodeR = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return TipGen
      1 -> Tip <$> decodeR
      _ -> fail "decodeTip: invalid tag"
