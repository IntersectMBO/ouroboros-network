{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , HasHeader(..)
  , StandardHash
  , ChainHash(..)
  , castHash
  , Point(..)
  , castPoint
  , blockPoint
  , ChainUpdate(..)
  , BlockMeasure(..)
  , blockMeasure
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Hashable
import           Data.FingerTree (Measured)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Codec.Serialise (Serialise (..))
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.CBOR.Decoding (decodeListLenOf)


-- | The 0-based index for the Ourboros time slot.
newtype SlotNo = SlotNo { unSlotNo :: Word64 }
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Num, Serialise, Generic)

-- | The 0-based index of the block in the blockchain.
-- BlockNo is <= SlotNo and is only equal at slot N if there is a block
-- for every slot where N <= SlotNo.
newtype BlockNo = BlockNo { unBlockNo :: Word64 }
  deriving (Show, Eq, Ord, Hashable, Enum, Bounded, Num, Serialise)

-- | Abstract over the shape of blocks (or indeed just block headers)
class (StandardHash b, Measured BlockMeasure b) => HasHeader b where
    -- TODO: I /think/ we should be able to make this injective, but I'd have
    -- to check after the redesign of the block abstraction (which will live
    -- in the consensus layer), to make sure injectivity is compatible with that
    -- design (and compatible with the concrete instantiation used in the
    -- network layer tests).
    type HeaderHash b :: *

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
      , Serialise (HeaderHash b)
      ) => StandardHash b

data ChainHash b = GenesisHash | BlockHash (HeaderHash b)
  deriving (Generic)

deriving instance StandardHash block => Eq   (ChainHash block)
deriving instance StandardHash block => Ord  (ChainHash block)
deriving instance StandardHash block => Show (ChainHash block)

-- | 'Hashable' instance for 'Hash'
--
-- We don't insist that 'Hashable' in 'StandardHash' because 'Hashable' is
-- only used in the network layer /tests/.
--
-- This requires @UndecidableInstances@ because @Hashable (HeaderHash b)@
-- is no smaller than @Hashable (ChainHash b)@.
instance Hashable (HeaderHash b) => Hashable (ChainHash b) where
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
data Point block = Point {
       pointSlot :: SlotNo,
       pointHash :: ChainHash block
     }
   deriving (Eq, Ord, Show)

castPoint :: (HeaderHash a ~ HeaderHash b) => Point a -> Point b
castPoint (Point a b) = Point a (castHash b)

blockPoint :: HasHeader block => block -> Point block
blockPoint b =
    Point {
      pointSlot = blockSlot b,
      pointHash = BlockHash (blockHash b)
    }

{-------------------------------------------------------------------------------
  ChainUpdate type
-------------------------------------------------------------------------------}

-- | A representation of two actions to update a chain: add a block or roll
-- back to a previous point.
--
data ChainUpdate block = AddBlock block
                       | RollBack (Point block)
  deriving (Eq, Show)


{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance StandardHash b => Serialise (ChainHash b) where
  -- use the Generic instance

instance HasHeader block => Serialise (Point block) where

  encode Point { pointSlot = s, pointHash = h } =
      encodeListLen 2
   <> encode s
   <> encode h

  decode = do
      decodeListLenOf 2
      Point <$> decode
            <*> decode

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
