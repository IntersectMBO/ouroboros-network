{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (
    FileInfo      -- opaque
  , FileBlockInfo -- opaque
    -- * Construction
  , empty
  , addBlock
  , fromParsedInfo
  , mkFileBlockInfo
    -- * Queries
  , canGC
  , blockIds
  , isFull
  , maxSlotInFiles
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo)

import           Ouroboros.Consensus.Storage.VolatileDB.Types

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | The internal information the db keeps for each file
data FileInfo blockId = MkFileInfo {
      fLatestSlot :: !MaxSlotNo
    , fContents   :: !(Map BlockOffset (FileBlockInfo blockId))
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | Information about a block in a file
data FileBlockInfo blockId = FileBlockInfo {
      fsBlockSize :: !BlockSize
    , fsBlockId   :: !blockId
    } deriving (Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FileInfo blockId
empty = MkFileInfo {
      fLatestSlot = NoMaxSlotNo
    , fContents   = Map.empty
    }

-- | Adds a block to a 'FileInfo'.
addBlock :: SlotNo
         -> BlockOffset
         -> FileBlockInfo blockId
         -> FileInfo blockId
         -> FileInfo blockId
addBlock slotNo blockOffset blockInfo MkFileInfo { fLatestSlot, fContents } =
    MkFileInfo {
        fLatestSlot = fLatestSlot `max` MaxSlotNo slotNo
      , fContents   = Map.insert blockOffset blockInfo fContents
      }

-- | Construct a 'FileInfo' from the parser result.
fromParsedInfo :: forall blockId. ParsedInfo blockId -> FileInfo blockId
fromParsedInfo parsedInfo = MkFileInfo maxSlotNo contents
  where
    maxSlotNo = foldMap parsedBlockInfoToMaxSlotNo parsedInfo

    parsedBlockInfoToMaxSlotNo :: ParsedBlockInfo blockId -> MaxSlotNo
    parsedBlockInfoToMaxSlotNo (_, (_, blockInfo)) =
      MaxSlotNo $ bslot blockInfo

    contents :: Map BlockOffset (FileBlockInfo blockId)
    contents = Map.fromList
      [ (blockOffset, FileBlockInfo blockSize (bbid blockInfo))
      | (blockOffset, (blockSize, blockInfo)) <- parsedInfo
      ]

mkFileBlockInfo :: BlockSize -> blockId -> FileBlockInfo blockId
mkFileBlockInfo = FileBlockInfo

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Checks if this file can be GCed.
canGC :: FileInfo blockId
      -> SlotNo -- ^ The slot number of any block in the immutable DB.
      -> Bool
canGC MkFileInfo { fLatestSlot } slot =
    case fLatestSlot of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | All @blockId@ in this file.
blockIds :: FileInfo blockId -> [blockId]
blockIds MkFileInfo { fContents } = fsBlockId <$> Map.elems fContents

-- | Has this file reached its maximum size?
isFull :: BlocksPerFile -> FileInfo blockId -> Bool
isFull maxBlocksPerFile MkFileInfo { fContents } =
    fromIntegral (Map.size fContents) >= unBlocksPerFile maxBlocksPerFile

maxSlotInFiles :: [FileInfo blockId] -> MaxSlotNo
maxSlotInFiles = foldMap fLatestSlot
