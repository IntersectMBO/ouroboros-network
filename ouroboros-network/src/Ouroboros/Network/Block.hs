{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

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
  , Point(..)
  , pointSlot
  , pointHash
  , castPoint
  , blockPoint
  , Tip(..)
  , encodeTip
  , decodeTip
  , ChainUpdate(..)
  , MaxSlotNo (..)
  , maxSlotNoFromMaybe
  , maxSlotNoToMaybe
  , maxSlotNoFromWithOrigin
  , BlockMeasure(..)
  , blockMeasure
  , genesisPoint
  , genesisSlotNo
  , genesisBlockNo
    -- * Serialisation
  , encodePoint
  , encodeChainHash
  , decodePoint
  , decodeChainHash

  , pattern GenesisPoint
  , pattern BlockPoint
  , atSlot
  , withHash
  ) where

import           Cardano.Binary (ToCBOR (..))
import           Codec.CBOR.Decoding (Decoder)
import qualified Codec.CBOR.Decoding as Dec
import           Codec.CBOR.Encoding (Encoding)
import qualified Codec.CBOR.Encoding as Enc
import           Codec.Serialise (Serialise (..))
import           Data.FingerTree (Measured)
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Point (WithOrigin (..), block, origin,
                     withOriginToMaybe)
import qualified Ouroboros.Network.Point as Point (Block (..))

-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Serialise, Generic, NoUnexpectedThunks)

instance ToCBOR SlotNo where
  toCBOR = encode

-- | The 0-based index of the block in the blockchain.
-- BlockNo is <= SlotNo and is only equal at slot N if there is a block
-- for every slot where N <= SlotNo.
newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving (Show, Eq, Ord, Enum, Bounded, Num, Serialise, Generic, NoUnexpectedThunks)

genesisSlotNo :: SlotNo
genesisSlotNo = SlotNo 0

genesisBlockNo :: BlockNo
genesisBlockNo = BlockNo 0

genesisPoint :: Point block
genesisPoint = Point origin

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
      , NoUnexpectedThunks (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash (HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

instance (StandardHash block, Typeable block) => NoUnexpectedThunks (ChainHash block)
  -- use generic instance

castHash :: HeaderHash b ~ HeaderHash b' => ChainHash b -> ChainHash b'
castHash GenesisHash   = GenesisHash
castHash (BlockHash b) = BlockHash b

{-------------------------------------------------------------------------------
  Point on a chain
-------------------------------------------------------------------------------}

-- | A point on the chain is identified by its 'Slot' and 'HeaderHash'.
--
-- The 'Slot' tells us where to look and the 'HeaderHash' either simply serves
-- as a check, or in some contexts it disambiguates blocks from different forks
-- that were in the same slot.
--
-- It's a newtype rather than a type synonym, because using a type synonym
-- would lead to ambiguity, since HeaderHash is a non-injective type family.
newtype Point block = Point
    { getPoint :: WithOrigin (Point.Block SlotNo (HeaderHash block))
    }

deriving instance StandardHash block => Eq   (Point block)
deriving instance StandardHash block => Ord  (Point block)
deriving instance StandardHash block => Show (Point block)
deriving instance (StandardHash block, Typeable block) => NoUnexpectedThunks (Point block)

pattern GenesisPoint :: Point block
pattern GenesisPoint = Point Origin

pattern BlockPoint :: SlotNo -> HeaderHash block -> Point block
pattern BlockPoint { atSlot, withHash } = Point (At (Point.Block atSlot withHash))

{-# COMPLETE GenesisPoint, BlockPoint #-}

pointSlot :: Point block -> WithOrigin SlotNo
pointSlot (Point pt) = fmap Point.blockPointSlot pt

pointHash :: Point block -> ChainHash block
pointHash (Point pt) = case pt of
    Origin -> GenesisHash
    At blk -> BlockHash (Point.blockPointHash blk)

castPoint :: (HeaderHash a ~ HeaderHash b) => Point a -> Point b
castPoint (Point Origin)                       = Point Origin
castPoint (Point (At (Point.Block slot hash))) = Point (block slot hash)

blockPoint :: HasHeader block => block -> Point block
blockPoint b = Point (block (blockSlot b) (blockHash b))

{-------------------------------------------------------------------------------
  Tip of a chain
-------------------------------------------------------------------------------}

-- | Used in chain-sync protocol to advertise the tip of the server's chain.
--
data Tip b = Tip
  { tipPoint   :: !(Point b)
  , tipBlockNo :: !BlockNo
  } deriving (Eq, Show, Generic)

encodeTip :: (HeaderHash blk -> Encoding)
          -> (Tip        blk -> Encoding)
encodeTip encodeHeaderHash Tip { tipPoint, tipBlockNo } = mconcat
    [ Enc.encodeListLen 2
    , encodePoint encodeHeaderHash tipPoint
    , encode                       tipBlockNo
    ]

decodeTip :: (forall s. Decoder s (HeaderHash blk))
          -> (forall s. Decoder s (Tip        blk))
decodeTip decodeHeaderHash = do
  Dec.decodeListLenOf 2
  tipPoint    <- decodePoint decodeHeaderHash
  tipBlockNo  <- decode
  return Tip { tipPoint, tipBlockNo }

{-------------------------------------------------------------------------------
  ChainUpdate type
-------------------------------------------------------------------------------}

-- | A representation of two actions to update a chain: add a block or roll
-- back to a previous point.
--
-- The type parameter @a@ is there to allow a 'Functor' instance. Typically,
-- it will be instantiated with @block@ itself.
data ChainUpdate block a = AddBlock a
                         | RollBack (Point block)
  deriving (Eq, Show, Functor, Foldable, Traversable)

{-------------------------------------------------------------------------------
  MaxSlotNo
-------------------------------------------------------------------------------}

-- | The highest slot number seen.
data MaxSlotNo
  = NoMaxSlotNo
    -- ^ No block/header has been seen yet, so we don't have a highest slot
    -- number.
  | MaxSlotNo !SlotNo
    -- ^ The highest slot number seen.
  deriving (Eq, Show, Generic)

-- The derived instances would do the same, but for clarity, we write it out
-- explicitly.
instance Ord MaxSlotNo where
  compare NoMaxSlotNo       (MaxSlotNo _) = LT
  compare NoMaxSlotNo       NoMaxSlotNo   = EQ
  compare (MaxSlotNo _)  NoMaxSlotNo      = GT
  compare (MaxSlotNo s1) (MaxSlotNo s2)   = compare s1 s2

maxSlotNoFromMaybe :: Maybe SlotNo -> MaxSlotNo
maxSlotNoFromMaybe = maybe NoMaxSlotNo MaxSlotNo

maxSlotNoToMaybe :: MaxSlotNo -> Maybe SlotNo
maxSlotNoToMaybe NoMaxSlotNo   = Nothing
maxSlotNoToMaybe (MaxSlotNo s) = Just s

maxSlotNoFromWithOrigin :: WithOrigin SlotNo -> MaxSlotNo
maxSlotNoFromWithOrigin = maxSlotNoFromMaybe . withOriginToMaybe

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

--TODO: these two instances require UndecidableInstances
instance Serialise (HeaderHash b) => Serialise (ChainHash b) where
  encode = encodeChainHash encode
  decode = decodeChainHash decode

instance Serialise (HeaderHash block) => Serialise (Point block) where
  encode = encodePoint encode
  decode = decodePoint decode

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

encodePoint :: (HeaderHash block -> Encoding)
            -> (Point      block -> Encoding)
encodePoint encodeHash (Point pt) = case pt of
    Origin -> Enc.encodeListLen 0
    At blk ->
           Enc.encodeListLen 2
        <> encode     (Point.blockPointSlot blk)
        <> encodeHash (Point.blockPointHash blk)

decodePoint :: (forall s. Decoder s (HeaderHash block))
            -> (forall s. Decoder s (Point      block))
decodePoint decodeHash = do
    tag <- Dec.decodeListLen
    case tag of
      0 -> return (Point origin)
      2 -> do
        slot <- decode
        hash <- decodeHash
        return (Point (block slot hash))
      _ -> fail "decodePoint: invalid tag"

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
