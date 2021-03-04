{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Ouroboros.Consensus.Block.Abstract (
    -- * Protocol
    BlockProtocol
    -- * Configuration
  , BlockConfig
  , CodecConfig
  , StorageConfig
    -- * Previous hash
  , GetPrevHash(..)
  , blockPrevHash
    -- * Working with headers
  , Header
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
    -- * Utilities for working with WithOrigin
  , succWithOrigin
    -- * Re-export basic definitions from @ouroboros-network@
  , blockHash
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
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Kind (Type)
import           Data.Maybe (isJust)
import           Data.Word (Word32)

import           Cardano.Slotting.Block (BlockNo (..))
import           Cardano.Slotting.Slot (EpochNo (..), EpochSize (..),
                     SlotNo (..), WithOrigin (Origin), fromWithOrigin,
                     withOrigin, withOriginFromMaybe, withOriginToMaybe)
import qualified Cardano.Slotting.Slot as Cardano

import           Ouroboros.Network.Block (ChainHash (..), HasHeader (..),
                     HeaderFields (..), HeaderHash, Point, StandardHash,
                     blockHash, blockNo, blockPoint, blockSlot, castHash,
                     castHeaderFields, castPoint, pattern BlockPoint,
                     pattern GenesisPoint, pointHash, pointSlot)

import           Ouroboros.Consensus.Block.EBB

{-------------------------------------------------------------------------------
  Protocol
-------------------------------------------------------------------------------}

-- | Map block to consensus protocol
type family BlockProtocol blk :: Type

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | Static configuration required to work with this type of blocks
data family BlockConfig blk :: Type

-- | Static configuration required for serialisation and deserialisation of
-- types pertaining to this type of block.
--
-- Data family instead of type family to get better type inference.
data family CodecConfig blk :: Type

-- | Config needed for the
-- 'Ouroboros.Consensus.Node.InitStorage.NodeInitStorage' class. Defined here to
-- avoid circular dependencies.
data family StorageConfig blk :: Type

{-------------------------------------------------------------------------------
  Get hash of previous block
-------------------------------------------------------------------------------}

class (HasHeader blk, GetHeader blk) => GetPrevHash blk where
  -- | Get the hash of the predecessor of this block
  headerPrevHash :: Header blk -> ChainHash blk

blockPrevHash :: GetPrevHash blk => blk -> ChainHash blk
blockPrevHash = castHash . headerPrevHash . getHeader

{-------------------------------------------------------------------------------
  Link block to its header
-------------------------------------------------------------------------------}

data family Header blk :: Type

class HasHeader (Header blk) => GetHeader blk where
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
--
-- Variants of 'toRawHash' and 'fromRawHash' for 'ShortByteString' are
-- included. Override the default implementations to avoid an extra step in
-- case the 'HeaderHash' is a 'ShortByteString' under the hood.
class ConvertRawHash blk where
  -- | Get the raw bytes from a hash
  toRawHash :: proxy blk -> HeaderHash blk -> Strict.ByteString
  toRawHash p = Short.fromShort . toShortRawHash p

  -- | Construct the hash from a raw hash
  --
  -- PRECONDITION: the bytestring's size must match 'hashSize'
  fromRawHash :: proxy blk -> Strict.ByteString -> HeaderHash blk
  fromRawHash p = fromShortRawHash p . Short.toShort

  -- | Variant of 'toRawHash' for 'ShortByteString'
  toShortRawHash :: proxy blk -> HeaderHash blk -> ShortByteString
  toShortRawHash p = Short.toShort . toRawHash p

  -- | Variant of 'fromRawHash' for 'ShortByteString'
  fromShortRawHash :: proxy blk -> ShortByteString -> HeaderHash blk
  fromShortRawHash p = fromRawHash p . Short.fromShort

  -- | The size of the hash in number of bytes
  hashSize :: proxy blk -> Word32

  {-# MINIMAL hashSize
            , (toRawHash | toShortRawHash)
            , (fromRawHash | fromShortRawHash) #-}

encodeRawHash :: ConvertRawHash blk
              => proxy blk -> HeaderHash blk -> Encoding
encodeRawHash p = Serialise.encode . toShortRawHash p

decodeRawHash :: ConvertRawHash blk
              => proxy blk -> forall s. Decoder s (HeaderHash blk)
decodeRawHash p = fromShortRawHash p <$> Serialise.decode

{-------------------------------------------------------------------------------
  Utilities for working with WithOrigin
-------------------------------------------------------------------------------}

{-# COMPLETE Origin, NotOrigin #-}

-- | Custom pattern for 'WithOrigin'
--
-- This avoids clashing with our (extensive) use of 'At' for testing.
pattern NotOrigin :: t -> WithOrigin t
pattern NotOrigin t = Cardano.At t

-- | Return the successor of a 'WithOrigin' value. Useful in combination with
-- 'SlotNo' and 'BlockNo'.
succWithOrigin :: (Bounded t, Enum t) => WithOrigin t -> t
succWithOrigin = withOrigin minBound succ
