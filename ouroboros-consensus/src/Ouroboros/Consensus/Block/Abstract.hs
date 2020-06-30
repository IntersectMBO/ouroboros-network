{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Block.Abstract (
    -- * Protocol
    BlockProtocol
    -- * Configuration
  , BlockConfig
  , CodecConfig
    -- * Previous hash
  , GetPrevHash(..)
  , blockPrevHash
    -- * Working with headers
  , GetHeader(..)
  , getBlockHeaderFields
  , headerHash
  , headerPoint
  , headerToIsEBB
  , blockIsEBB
  , blockToIsEBB
    -- * Raw hash
  , ConvertRawHash(..)
  , encodeRawHash
  , decodeRawHash
    -- * Existentials
  , SomeBlock(..)
    -- * Re-export basic definitions from @ouroboros-network@
  , blockHash
  , blockMeasure
  , BlockMeasure -- opaque
  , blockNo
  , blockPoint
  , blockSlot
  , castHash
  , castHeaderFields
  , castPoint
  , ChainHash(..)
  , HasHeader(..)
  , HeaderFields(..)
  , HeaderHash
  , Point(GenesisPoint, BlockPoint)
  , pointHash
  , pointSlot
  , StandardHash
    -- * Re-export basic definitions from @cardano-base@
  , BlockNo(..)
  , EpochNo(..)
  , EpochSize(..)
  , fromWithOrigin
  , SlotNo(..)
  , withOrigin
  , withOriginFromMaybe
  , WithOrigin(Origin, NotOrigin)
  , withOriginToMaybe
  ) where

import qualified Codec.Serialise as Serialise
import           Codec.Serialise.Decoding (Decoder)
import           Codec.Serialise.Encoding (Encoding)
import qualified Data.ByteString as Strict
import           Data.FingerTree.Strict (Measured (..))
import           Data.Maybe (isJust)
import           Data.Word (Word32)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..), WithOrigin (Origin), fromWithOrigin,
                     withOrigin, withOriginFromMaybe, withOriginToMaybe)
import qualified Cardano.Slotting.Slot as Cardano

import           Ouroboros.Network.Block (BlockMeasure, pattern BlockPoint,
                     ChainHash (..), pattern GenesisPoint, HasHeader (..),
                     HeaderFields (..), HeaderHash, Point, StandardHash,
                     blockHash, blockMeasure, blockNo, blockPoint, blockSlot,
                     castHash, castHeaderFields, castPoint, pointHash,
                     pointSlot)

import           Ouroboros.Consensus.Block.EBB

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

-- | Map block to consensus protocol
type family BlockProtocol blk :: *

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Static configuration required to work with this type of blocks
data family BlockConfig blk :: *

-- | Static configuration required for serialisation and deserialisation of
-- types pertaining to this type of block.
--
-- Data family instead of type family to get better type inference.
data family CodecConfig blk :: *

{-------------------------------------------------------------------------------
  Get hash of previous block
-------------------------------------------------------------------------------}

class (HasHeader blk, GetHeader blk) => GetPrevHash blk where
  -- | Get the hash of the predecessor of this block
  --
  -- This gets its own abstraction, because it will be a key part of the path
  -- to getting rid of EBBs: when we have blocks @A - EBB - B@, the prev hash
  -- of @B@ will be reported as @A@.
  headerPrevHash :: CodecConfig blk -> Header blk -> ChainHash blk

blockPrevHash :: GetPrevHash blk => CodecConfig blk -> blk -> ChainHash blk
blockPrevHash cfg = castHash . headerPrevHash cfg . getHeader

{-------------------------------------------------------------------------------
  Link block to its header
-------------------------------------------------------------------------------}

class HasHeader (Header blk) => GetHeader blk where
  data family Header blk :: *
  getHeader          :: blk -> Header blk
  -- | Check whether the header is the header of the block.
  --
  -- For example, by checking whether the hash of the body stored in the
  -- header matches that of the block.
  blockMatchesHeader :: Header blk -> blk -> Bool

  -- | When the given header is the header of an Epoch Boundary Block, returns
  -- its epoch number.
  headerIsEBB        :: Header blk -> Maybe EpochNo

headerToIsEBB :: GetHeader blk => Header blk -> IsEBB
headerToIsEBB = toIsEBB . isJust . headerIsEBB

blockIsEBB :: GetHeader blk => blk -> Maybe EpochNo
blockIsEBB = headerIsEBB . getHeader

blockToIsEBB :: GetHeader blk => blk -> IsEBB
blockToIsEBB = headerToIsEBB . getHeader

type instance BlockProtocol (Header blk) = BlockProtocol blk

{-------------------------------------------------------------------------------
  Some automatic instances for 'Header'
-------------------------------------------------------------------------------}

type instance HeaderHash (Header blk) = HeaderHash blk

instance HasHeader blk => StandardHash (Header blk)

instance HasHeader (Header blk) => Measured BlockMeasure (Header blk) where
  measure = blockMeasure

-- | Get the 'HeaderFields' of a block, without requiring 'HasHeader blk'
--
-- This is primarily useful as a a simple definition of 'HasHeader' for
-- block types:
--
-- > instance HasHeader SomeBlock where
-- >   getHeaderFields = getBlockHeaderFields
--
-- provided that there is a 'HasHeader' instance for the header.
--
-- Unfortunately we cannot give a 'HasHeader' instance once and for all; if we
-- mapped from a header to a block instead we could do
--
-- > instance HasHeader hdr => HasHeader (Block hdr) where
-- >  ..
--
-- but we can't do that when we do things this way around.
getBlockHeaderFields :: GetHeader blk => blk -> HeaderFields blk
getBlockHeaderFields = castHeaderFields . getHeaderFields . getHeader

{-------------------------------------------------------------------------------
  Convenience wrappers around 'HasHeader' that avoids unnecessary casts
-------------------------------------------------------------------------------}

headerHash :: HasHeader (Header blk) => Header blk -> HeaderHash blk
headerHash = blockHash

headerPoint :: HasHeader (Header blk) => Header blk -> Point blk
headerPoint = castPoint . blockPoint

{-------------------------------------------------------------------------------
  Raw hash
-------------------------------------------------------------------------------}

-- | Convert a hash from/to raw bytes
class ConvertRawHash blk where
  -- | Get the raw bytes from a hash
  toRawHash   :: proxy blk -> HeaderHash blk -> Strict.ByteString

  -- | Construct the hash from a raw hash
  --
  -- PRECONDITION: the bytestring's size must match 'hashSize'
  fromRawHash :: proxy blk -> Strict.ByteString -> HeaderHash blk

  -- | The size of the hash in number of bytes
  hashSize    :: proxy blk -> Word32

encodeRawHash :: ConvertRawHash blk
              => proxy blk -> HeaderHash blk -> Encoding
encodeRawHash p = Serialise.encode . toRawHash p

decodeRawHash :: ConvertRawHash blk
              => proxy blk -> forall s. Decoder s (HeaderHash blk)
decodeRawHash p = fromRawHash p <$> Serialise.decode

{-------------------------------------------------------------------------------
  Existentials
-------------------------------------------------------------------------------}

-- | Hide the type argument of a block-indexed GADT
--
-- @SomeBlock f blk@ is isomorphic to @Some (f blk)@, but is more convenient
-- in partial applications.
data SomeBlock (f :: * -> * -> *) blk where
  SomeBlock :: f blk a -> SomeBlock f blk

{-------------------------------------------------------------------------------
  Custom patterns for WithOrigin

  This avoids clashing with our (extensive) use of 'At' for testing.
-------------------------------------------------------------------------------}

{-# COMPLETE Origin, NotOrigin #-}

pattern NotOrigin :: t -> WithOrigin t
pattern NotOrigin t = Cardano.At t
