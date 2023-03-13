{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Storage.Common (
    -- * Indexing
    tipIsGenesis
    -- * PrefixLen
  , PrefixLen (..)
  , addPrefixLen
  , takePrefix
    -- * BinaryBlockInfo
  , BinaryBlockInfo (..)
  , extractHeader
    -- * Iterator bounds
  , StreamFrom (..)
  , StreamTo (..)
  , validBounds
    -- * BlockComponent
  , BlockComponent (..)
    -- * Re-exports
  , SizeInBytes
  ) where

import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as Short
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Network.SizeInBytes (SizeInBytes)

{-------------------------------------------------------------------------------
  Indexing
-------------------------------------------------------------------------------}

tipIsGenesis :: WithOrigin r -> Bool
tipIsGenesis Origin        = True
tipIsGenesis (NotOrigin _) = False

{-------------------------------------------------------------------------------
  PrefixLen
-------------------------------------------------------------------------------}

-- | Number of bytes from the start of a block needed to reconstruct the
-- nested context.
--
-- See 'reconstructPrefixLen'.
newtype PrefixLen = PrefixLen {
      getPrefixLen :: Word8
    }
  deriving stock   (Eq, Ord, Show, Generic)
  deriving newtype (NoThunks)

addPrefixLen :: Word8 -> PrefixLen -> PrefixLen
addPrefixLen m (PrefixLen n) = PrefixLen (m + n)

takePrefix :: PrefixLen -> BL.ByteString -> ShortByteString
takePrefix (PrefixLen n) =
    Short.toShort . BL.toStrict . BL.take (fromIntegral n)

{-------------------------------------------------------------------------------
  BinaryBlockInfo
-------------------------------------------------------------------------------}

-- | Information about the serialised block.
data BinaryBlockInfo = BinaryBlockInfo
  { headerOffset :: !Word16
    -- ^ The offset within the serialised block at which the header starts.
  , headerSize   :: !Word16
    -- ^ How many bytes the header is long. Extracting the 'headerSize' bytes
    -- from serialised block starting from 'headerOffset' should yield the
    -- header. Before passing the extracted bytes to the decoder for headers,
    -- an envelope can be around using 'nodeAddHeaderEnvelope'.

    -- In the future, i.e. Shelley, we might want to extend this to include a
    -- field to tell where the transaction body ends and where the transaction
    -- witnesses begin so we can only extract the transaction body.
  } deriving (Eq, Show, Generic)


-- | Extract the header from the given 'ByteString' using the
-- 'BinaryBlockInfo'.
extractHeader :: BinaryBlockInfo -> ByteString -> ByteString
extractHeader BinaryBlockInfo { headerOffset, headerSize } =
      BL.take (fromIntegral headerSize)
    . BL.drop (fromIntegral headerOffset)

{-------------------------------------------------------------------------------
  Iterator bounds
-------------------------------------------------------------------------------}

-- | The lower bound for an iterator
--
-- Hint: use @'StreamFromExclusive' 'genesisPoint'@ to start streaming from
-- Genesis.
data StreamFrom blk =
    StreamFromInclusive !(RealPoint blk)
  | StreamFromExclusive !(Point     blk)
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

newtype StreamTo blk =
    StreamToInclusive (RealPoint blk)
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NoThunks)

-- | Check whether the bounds make sense
--
-- An example of bounds that don't make sense:
--
-- > StreamFromExclusive (BlockPoint 3 ..)
-- > StreamToInclusive   (RealPoint  3 ..)
--
-- This function does not check whether the bounds correspond to existing
-- blocks.
validBounds :: StandardHash blk => StreamFrom blk -> StreamTo blk -> Bool
validBounds from (StreamToInclusive (RealPoint sto hto)) =
    case from of
      StreamFromExclusive GenesisPoint         -> True
      -- EBBs spoil the fun again: when 'StreamFromExclusive' refers to an EBB
      -- in slot X and 'StreamToInclusive' to the regular block in the same slot
      -- X, the bound is still valid. Without EBBs, we would have @sfrom < sto@.
      --
      -- We /can/ rule out streaming exclusively from the block to the same
      -- block.
      StreamFromExclusive (BlockPoint sfrom hfrom) -> hfrom /= hto && sfrom <= sto
      StreamFromInclusive (RealPoint  sfrom _)     -> sfrom <= sto

{-------------------------------------------------------------------------------
  BlockComponent
-------------------------------------------------------------------------------}

-- | Which component of the block to read from a database: the whole block,
-- its header, its hash, the block size, ..., or combinations thereof.
--
-- NOTE: when requesting multiple components, we will not optimise/cache them.
data BlockComponent blk a where
  -- | Verify the integrity of the block by checking its signature and/or
  -- hashes. The interpreter should throw an exception when the block does not
  -- pass the check.
  GetVerifiedBlock :: BlockComponent blk blk
  GetBlock         :: BlockComponent blk blk
  GetRawBlock      :: BlockComponent blk ByteString
  GetHeader        :: BlockComponent blk (Header blk)
  GetRawHeader     :: BlockComponent blk ByteString
  GetHash          :: BlockComponent blk (HeaderHash blk)
  GetSlot          :: BlockComponent blk SlotNo
  GetIsEBB         :: BlockComponent blk IsEBB
  -- TODO: use `SizeInBytes` rather than Word32
  GetBlockSize     :: BlockComponent blk Word32
  GetHeaderSize    :: BlockComponent blk Word16
  GetNestedCtxt    :: BlockComponent blk (SomeSecond (NestedCtxt Header) blk)
  GetPure          :: a
                   -> BlockComponent blk a
  GetApply         :: BlockComponent blk (a -> b)
                   -> BlockComponent blk a
                   -> BlockComponent blk b

instance Functor (BlockComponent blk) where
  fmap f = (GetPure f <*>)

instance Applicative (BlockComponent blk) where
  pure  = GetPure
  (<*>) = GetApply
