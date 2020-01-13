{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}

module Ouroboros.Storage.Common (
    -- * Epochs
    EpochNo(..)
  , EpochSize(..)
    -- * Indexing
  , Tip(..)
  , tipIsGenesis
  , tipToPoint
    -- * Serialisation
  , encodeTip
  , decodeTip
    -- * BinaryInfo
  , BinaryInfo (..)
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.Word
import           GHC.Generics

import           Cardano.Prelude (NoUnexpectedThunks)
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..))

import           Ouroboros.Network.Block (Point, genesisPoint)

import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

-- | Tip of the chain
data Tip r = Tip !r | TipGen
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, NoUnexpectedThunks)

-- | 'TipGen' is always smaller than 'Tip'
instance Ord r => Ord (Tip r) where
  compare x y = case (x, y) of
    (TipGen, TipGen) -> EQ
    (TipGen, Tip _)  -> LT
    (Tip _,  TipGen) -> GT
    (Tip a,  Tip b)  -> compare a b

instance Condense r => Condense (Tip r) where
  condense TipGen  = "genesis"
  condense (Tip r) = condense r

tipIsGenesis :: Tip r -> Bool
tipIsGenesis TipGen  = True
tipIsGenesis (Tip _) = False

tipToPoint :: Tip (Point blk) -> Point blk
tipToPoint TipGen  = genesisPoint
tipToPoint (Tip p) = p

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

{-------------------------------------------------------------------------------
  BinaryInfo
-------------------------------------------------------------------------------}

-- | Information about the serialised block.
data BinaryInfo blob = BinaryInfo
  { binaryBlob   :: !blob
  , headerOffset :: !Word16
    -- ^ The offset within the 'binaryBlob' at which the header starts.
  , headerSize   :: !Word16
    -- ^ How many bytes the header is long. Extracting the 'headerSize' bytes
    -- from 'binaryBlob' starting from 'headerOffset' should yield the header.

    -- In the future, i.e. Shelley, we might want to extend this to include a
    -- field to tell where the transaction body ends and where the transaction
    -- witnesses begin so we can only extract the transaction body.
  } deriving (Show, Generic, Functor)
