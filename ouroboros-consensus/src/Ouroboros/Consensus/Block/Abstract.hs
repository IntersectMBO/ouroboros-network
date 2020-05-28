{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Ouroboros.Consensus.Block.Abstract (
    -- * Protocol
    BlockProtocol
    -- * Configuration
  , BlockConfig
    -- * Working with headers
  , GetHeader(..)
  , headerHash
  , headerPrevHash
  , headerPoint
  , headerToIsEBB
  , blockIsEBB
  , blockToIsEBB
    -- * Raw hash
  , ConvertRawHash(..)
  ) where

import qualified Data.ByteString as Strict
import           Data.FingerTree.Strict (Measured (..))
import           Data.Maybe (isJust)
import           Data.Word (Word32)

import           Cardano.Slotting.Slot (EpochNo)

-- TODO: Should we re-export (a subset of?) this module so that we don't need
-- to import from ouroboros-network so often?
import           Ouroboros.Network.Block

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

{-------------------------------------------------------------------------------
  Link block to its header
-------------------------------------------------------------------------------}

class GetHeader blk where
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

  Unfortunately we cannot give a 'HasHeader' instance; if we mapped from a
  header to a block instead we could do

  > instance HasHeader hdr => HasHeader (Block hdr) where
  >  ..

  but we can't do that when we do things this way around.
-------------------------------------------------------------------------------}

type instance HeaderHash (Header blk) = HeaderHash blk

instance HasHeader blk => StandardHash (Header blk)

instance HasHeader (Header blk) => Measured BlockMeasure (Header blk) where
  measure = blockMeasure

{-------------------------------------------------------------------------------
  Convenience wrappers around 'HasHeader' that avoids unnecessary casts
-------------------------------------------------------------------------------}

headerHash :: HasHeader (Header blk) => Header blk -> HeaderHash blk
headerHash = blockHash

headerPrevHash :: HasHeader (Header blk) => Header blk -> ChainHash blk
headerPrevHash = castHash . blockPrevHash

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
