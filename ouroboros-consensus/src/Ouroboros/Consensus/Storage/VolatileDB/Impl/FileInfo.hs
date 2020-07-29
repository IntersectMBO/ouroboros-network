{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (
    FileInfo      -- opaque
    -- * Construction
  , empty
  , addBlock
  , fromParsedBlockInfos
    -- * Queries
  , maxSlotNo
  , hashes
  , canGC
  , isFull
  , maxSlotNoInFiles
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | The internal information the VolatileDB keeps for each file.
--
-- We parameterise by @hash@ instead of @blk@ because @HeaderHash blk@ is a
-- type family and would lead to ambiguity problems.
data FileInfo hash = FileInfo {
      maxSlotNo :: !MaxSlotNo
    , hashes    :: !(Set hash)
    } deriving (Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FileInfo hash
empty = FileInfo {
      maxSlotNo = NoMaxSlotNo
    , hashes  = Set.empty
    }

-- | Adds a block to a 'FileInfo'.
addBlock :: Ord hash => SlotNo -> hash -> FileInfo hash -> FileInfo hash
addBlock slotNo hash FileInfo { maxSlotNo, hashes } =
    FileInfo {
        maxSlotNo = maxSlotNo `max` MaxSlotNo slotNo
      , hashes    = Set.insert hash hashes
      }

-- | Construct a 'FileInfo' from the parser result.
fromParsedBlockInfos ::
     forall hash blk. (Ord hash, hash ~ HeaderHash blk)
  => [ParsedBlockInfo blk] -> FileInfo hash
fromParsedBlockInfos parsedBlockInfos = FileInfo {
      maxSlotNo = foldMap parsedBlockInfoToMaxSlotNo parsedBlockInfos
    , hashes    = Set.fromList $ map (biHash . pbiBlockInfo) parsedBlockInfos
    }
  where
    parsedBlockInfoToMaxSlotNo :: ParsedBlockInfo blk -> MaxSlotNo
    parsedBlockInfoToMaxSlotNo = MaxSlotNo . biSlotNo . pbiBlockInfo

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Checks if this file can be GCed.
canGC ::
     FileInfo hash
  -> SlotNo -- ^ The slot which we want to GC
  -> Bool
canGC FileInfo { maxSlotNo } slot =
    case maxSlotNo of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | Has this file reached its maximum size?
isFull :: BlocksPerFile -> FileInfo hash -> Bool
isFull maxBlocksPerFile FileInfo { hashes } =
    fromIntegral (Set.size hashes) >= unBlocksPerFile maxBlocksPerFile

maxSlotNoInFiles :: [FileInfo hash] -> MaxSlotNo
maxSlotNoInFiles = foldMap maxSlotNo
