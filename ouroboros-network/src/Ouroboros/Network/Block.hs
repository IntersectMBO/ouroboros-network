{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE GADTSyntax                 #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- | Abstract view over blocks
--
-- The network layer does not make any concrete assumptions about what blocks
-- look like.
module Ouroboros.Network.Block (
    SlotNo(..)
  , BlockNo(..)
  , HeaderHash
  , HasHeader(..)
  , StandardHash
  , ChainHash(..)
  , castHash
  , TBlockPoint (..)
  , TPoint(..)
  , fromTPoint
  , BlockPoint
  , Point
  , blockPoint
  , chainHashFromPoint
  , ChainUpdate(..)
  , mapChainUpdate
  , BlockMeasure(..)
  , blockMeasure
    -- * Serialisation
  , encodeTBlockPoint
  , encodeTPoint
  , encodeChainHash
  , decodeTBlockPoint
  , decodeTPoint
  , decodeChainHash
  ) where

import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.FingerTree (Measured)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Serialise, Generic)

-- | The 0-based index of the block in the blockchain.
-- BlockNo is <= SlotNo and is only equal at slot N if there is a block
-- for every slot where N <= SlotNo.
newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Serialise, Generic)

-- | Header hash
type family HeaderHash b :: *

-- | Abstract over the shape of blocks (or indeed just block headers)
class (StandardHash b, Measured BlockMeasure b, Typeable b) => HasHeader b where
    blockHash      :: b -> HeaderHash b
    blockPrevHash  :: b -> ChainHash b
    blockSlot      :: b -> SlotNo
    blockNo        :: b -> BlockNo

    blockInvariant :: b -> Bool

-- | When implementing 'HasHeader', use this method to implement the 'measure'
-- method of the 'Measured' super class.
blockMeasure :: HasHeader b => b -> BlockMeasure
blockMeasure b = BlockMeasure (blockSlot b) (blockSlot b) 1

-- | 'StandardHash' summarises the constraints we want header hashes to have
--
-- Without this class we would need to write
--
-- > deriving instance Eq (HeaderHash block) => Eq (ChainHash block)`
--
-- That requires @UndecidableInstances@; not a problem by itself, but it also
-- means that we can then not use @deriving Eq@ anywhere else for datatypes
-- that reference 'Hash', which is very frustrating; see
--
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#inferred-context-for-deriving-clauses>
--
-- Introducing the 'StandardHash' class avoids this problem.
--
-- Having these constraints directly as part of the 'HasHeader' class is
-- possible but libraries that /use/ the networking layer may wish to be able to
-- talk about 'StandardHash' independently of 'HasHeader' since the latter may
-- impose yet further constraints.
class ( Eq        (HeaderHash b)
      , Ord       (HeaderHash b)
      , Show      (HeaderHash b)
      , Typeable  (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash (HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

castHash :: HeaderHash b ~ HeaderHash b' => ChainHash b -> ChainHash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash b) = BlockHash b

{-------------------------------------------------------------------------------
  Point on a chain
-------------------------------------------------------------------------------}

-- | A block is identified by a slot and a hash.
data TBlockPoint slot hash = TBlockPoint
  { pointSlot :: slot
  , pointHash :: hash
  }
  deriving (Eq, Ord, Show)

type BlockPoint block = TBlockPoint SlotNo (HeaderHash block)

-- | `BlockPoint` with an origin point.
data TPoint t where
  Origin :: TPoint t
  Point  :: t -> TPoint t
  deriving (Eq, Ord, Show)

instance Functor TPoint where
  fmap _ Origin    = Origin
  fmap f (Point t) = Point (f t)

fromTPoint :: r -> (t -> r) -> TPoint t -> r
fromTPoint r _ Origin    = r
fromTPoint _ f (Point t) = f t

type Point block = TPoint (BlockPoint block)

blockPoint :: HasHeader block => block -> BlockPoint block
blockPoint b = TBlockPoint (blockSlot b) (blockHash b)

chainHashFromPoint :: Point block -> ChainHash block
chainHashFromPoint Origin     = GenesisHash
chainHashFromPoint (Point bp) = BlockHash (pointHash bp)

{-------------------------------------------------------------------------------
  ChainUpdate type
-------------------------------------------------------------------------------}

-- | A representation of two actions to update a chain: add a block or roll
-- back to a previous point.
--
data ChainUpdate block = AddBlock block
                       | RollBack (Point block)

deriving instance (Show block, Show (HeaderHash block)) => Show (ChainUpdate block)
deriving instance (Eq block, Eq (HeaderHash block)) => Eq (ChainUpdate block)

mapChainUpdate :: HeaderHash a ~ HeaderHash b
               => (a -> b) -> ChainUpdate a -> ChainUpdate b
mapChainUpdate f (AddBlock b) = AddBlock $ f b
mapChainUpdate _ (RollBack p) = RollBack $ p

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

--TODO: these two instances require UndecidableInstances
instance Serialise (HeaderHash b) => Serialise (ChainHash b) where
  encode = encodeChainHash encode
  decode = decodeChainHash decode

instance (Serialise slot, Serialise hash) => Serialise (TBlockPoint slot hash) where
  encode = encodeTBlockPoint encode encode
  decode = decodeTBlockPoint decode decode

instance (Serialise t) => Serialise (TPoint t) where
  encode = encodeTPoint encode
  decode = decodeTPoint decode

encodeChainHash :: (HeaderHash block -> Encoding)
                -> (ChainHash  block -> Encoding)
encodeChainHash encodeHash chainHash =
    case chainHash of
      GenesisHash -> Enc.encodeListLen 0
      BlockHash h -> Enc.encodeListLen 1 <> encodeHash h

decodeChainHash :: (forall s. Decoder s (HeaderHash block))
                -> (forall s. Decoder s (ChainHash  block))
decodeChainHash decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return GenesisHash
      1 -> BlockHash <$> decodeHash
      _ -> fail "decodeChainHash: invalid tag"

encodeTBlockPoint :: (slot -> Encoding)
                  -> (hash -> Encoding)
                  -> (TBlockPoint slot hash -> Encoding)
encodeTBlockPoint encodeSlot encodeHash bpoint =
       Enc.encodeListLen 2
    <> encodeSlot (pointSlot bpoint)
    <> encodeHash (pointHash bpoint)

decodeTBlockPoint :: (forall s. Decoder s slot)
                  -> (forall s. Decoder s hash)
                  -> (forall s. Decoder s (TBlockPoint slot hash))
decodeTBlockPoint decodeSlot decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      2 -> TBlockPoint <$> decodeSlot <*> decodeHash
      _ -> fail "decodeTPoint: invalid tag"

encodeTPoint :: (t -> Encoding)
             -> (TPoint t -> Encoding)
encodeTPoint encodet tpoint = case tpoint of
    Origin  -> Enc.encodeListLen 0
    Point t -> Enc.encodeListLen 1 <> encodet t

decodeTPoint :: (forall s. Decoder s t)
             -> (forall s. Decoder s (TPoint t))
decodeTPoint decodet = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return Origin
      1 -> Point <$> decodet
      _ -> fail "decodeTPoint: invalid tag"

{-------------------------------------------------------------------------------
  Finger Tree Measure
-------------------------------------------------------------------------------}

-- | The measure used for 'Ouroboros.Network.ChainFragment.ChainFragment'.
data BlockMeasure = BlockMeasure {
       bmMinSlot :: !SlotNo,
       bmMaxSlot :: !SlotNo,
       bmSize    :: !Int
     }
  deriving Show


instance Semigroup BlockMeasure where
  vl <> vr =
    BlockMeasure (min (bmMinSlot vl) (bmMinSlot vr))
                 (max (bmMaxSlot vl) (bmMaxSlot vr))
                 (bmSize vl + bmSize vr)

instance Monoid BlockMeasure where
  mempty = BlockMeasure maxBound minBound 0
  mappend = (<>)
