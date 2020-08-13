{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo (
    FileInfo      -- opaque
    -- * Construction
  , empty
  , addBlock
  , fromParsedInfo
    -- * Queries
  , maxSlotNo
  , blockIds
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
import           Ouroboros.Consensus.Storage.VolatileDB.Types

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | The internal information the db keeps for each file
data FileInfo blockId = FileInfo {
      maxSlotNo :: !MaxSlotNo
    , blockIds  :: !(Set blockId)
    } deriving (Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FileInfo blockId
empty = FileInfo {
      maxSlotNo = NoMaxSlotNo
    , blockIds  = Set.empty
    }

-- | Adds a block to a 'FileInfo'.
addBlock ::
     Ord blockId
  => SlotNo
  -> blockId
  -> FileInfo blockId
  -> FileInfo blockId
addBlock slotNo blockId FileInfo { maxSlotNo, blockIds } =
    FileInfo {
        maxSlotNo = maxSlotNo `max` MaxSlotNo slotNo
      , blockIds  = Set.insert blockId blockIds
      }

-- | Construct a 'FileInfo' from the parser result.
fromParsedInfo ::
     forall blockId. Ord blockId
  => ParsedInfo blockId
  -> FileInfo blockId
fromParsedInfo parsedInfo = FileInfo {
      maxSlotNo = foldMap parsedBlockInfoToMaxSlotNo parsedInfo
    , blockIds  = Set.fromList $ map (bbid . pbiBlockInfo) parsedInfo
    }
  where
    parsedBlockInfoToMaxSlotNo :: ParsedBlockInfo blockId -> MaxSlotNo
    parsedBlockInfoToMaxSlotNo = MaxSlotNo . bslot . pbiBlockInfo

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Checks if this file can be GCed.
canGC ::
     FileInfo blockId
  -> SlotNo -- ^ The slot which we want to GC
  -> Bool
canGC FileInfo { maxSlotNo } slot =
    case maxSlotNo of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | Has this file reached its maximum size?
isFull :: BlocksPerFile -> FileInfo blockId -> Bool
isFull maxBlocksPerFile FileInfo { blockIds } =
    fromIntegral (Set.size blockIds) >= unBlocksPerFile maxBlocksPerFile

maxSlotNoInFiles :: [FileInfo blockId] -> MaxSlotNo
maxSlotNoInFiles = foldMap maxSlotNo
