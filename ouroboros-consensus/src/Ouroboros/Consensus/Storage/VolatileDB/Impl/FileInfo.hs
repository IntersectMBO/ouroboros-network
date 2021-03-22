{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (
    -- * opaque
    FileInfo
    -- * Construction
  , addBlock
  , empty
  , fromParsedBlockInfos
    -- * Queries
  , canGC
  , hashes
  , isFull
  , maxSlotNo
  , maxSlotNoInFiles
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.VolatileDB.API (BlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | The internal information the VolatileDB keeps for each file.
data FileInfo blk = FileInfo {
      maxSlotNo :: !MaxSlotNo
    , hashes    :: !(Set (HeaderHash blk))
    }
  deriving (Generic)

deriving instance StandardHash blk => Show (FileInfo blk)
deriving instance StandardHash blk => NoThunks (FileInfo blk)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FileInfo blk
empty = FileInfo {
      maxSlotNo = NoMaxSlotNo
    , hashes  = Set.empty
    }

-- | Adds a block to a 'FileInfo'.
addBlock :: StandardHash blk => SlotNo -> HeaderHash blk -> FileInfo blk -> FileInfo blk
addBlock slotNo hash FileInfo { maxSlotNo, hashes } =
    FileInfo {
        maxSlotNo = maxSlotNo `max` MaxSlotNo slotNo
      , hashes    = Set.insert hash hashes
      }

-- | Construct a 'FileInfo' from the parser result.
fromParsedBlockInfos ::
     forall blk. StandardHash blk
  => [ParsedBlockInfo blk] -> FileInfo blk
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
     FileInfo blk
  -> SlotNo -- ^ The slot which we want to GC
  -> Bool
canGC FileInfo { maxSlotNo } slot =
    case maxSlotNo of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | Has this file reached its maximum size?
isFull :: BlocksPerFile -> FileInfo blk -> Bool
isFull maxBlocksPerFile FileInfo { hashes } =
    fromIntegral (Set.size hashes) >= unBlocksPerFile maxBlocksPerFile

maxSlotNoInFiles :: [FileInfo blk] -> MaxSlotNo
maxSlotNoInFiles = foldMap maxSlotNo
