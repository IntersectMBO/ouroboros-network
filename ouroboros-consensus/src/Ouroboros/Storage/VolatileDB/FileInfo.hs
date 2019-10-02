{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Storage.VolatileDB.FileInfo (
    FileInfo     -- opaque
  , FileSlotInfo(..) -- TODO: opaque
    -- * Construction
  , empty
  , addSlot
  , fromParsedInfo
    -- * Queries
  , canGC
  , blockIds
  , isFull
  , maxSlot
  , maxSlotInFiles
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo,
                     maxSlotNoFromMaybe, maxSlotNoToMaybe)

import           Ouroboros.Consensus.Util

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.VolatileDB.Types
import           Ouroboros.Storage.VolatileDB.Util

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- The Internal information the db keeps for each file.
data FileInfo blockId = MkFileInfo {
      fLatestSlot :: !MaxSlotNo
    , fNBlocks    :: !Int
    , fContents   :: !(Map SlotOffset (FileSlotInfo blockId))
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | Information about a slot in a file
data FileSlotInfo blockId = FileSlotInfo {
      fsBlockSize :: !BlockSize
    , fsBlockId   :: !blockId
    } deriving (Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: FileInfo blockId
empty = MkFileInfo {
      fLatestSlot = NoMaxSlotNo
    , fNBlocks    = 0
    , fContents   = Map.empty
    }

-- | Add slot to a file
addSlot :: SlotNo
        -> SlotOffset
        -> FileSlotInfo blockId
        -> FileInfo blockId -> FileInfo blockId
addSlot slotNo slotOffset slotInfo MkFileInfo{..} = MkFileInfo {
      fLatestSlot = updateSlotNoBlockId fLatestSlot [slotNo]
    , fNBlocks    = fNBlocks + 1
    , fContents   = Map.insert slotOffset slotInfo fContents
    }

-- | Construct 'FileInfo' from the parser result
--
-- Additionally returns information about the last 'SlotOffset' in the file,
-- unless the file is empty.
fromParsedInfo :: ParsedInfo blockId
               -> (FileInfo blockId, Maybe (blockId, SlotNo))
fromParsedInfo parsed =
    case lastSlotInfo parsed' of
      Nothing     -> (MkFileInfo NoMaxSlotNo   nBlocks contents, Nothing)
      Just (b, s) -> (MkFileInfo (MaxSlotNo s) nBlocks contents, Just (b, s))
  where
    parsed'  = Map.fromList parsed
    nBlocks  = Map.size parsed'
    contents = Map.map sizeAndId parsed'

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Check if this file can be GCed
canGC :: FileInfo blockId -> SlotNo -> Bool
canGC MkFileInfo{..} slot =
    case fLatestSlot of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | All block IDs in this file
blockIds :: FileInfo blockId -> [blockId]
blockIds MkFileInfo{..} = fsBlockId <$> Map.elems fContents

-- | Has this file reached its maximum size?
isFull :: Int -> FileInfo blockId -> Bool
isFull maxNumBlocks MkFileInfo{..} = fNBlocks >= fromIntegral maxNumBlocks

maxSlot :: FileInfo blockId -> MaxSlotNo
maxSlot = fLatestSlot

maxSlotInFiles :: [FileInfo blockId] -> MaxSlotNo
maxSlotInFiles = maxSlotNoFromMaybe
               . safeMaximum
               . mapMaybe (maxSlotNoToMaybe . maxSlot)

{-------------------------------------------------------------------------------
  Internal auxiliary

  TODO: Some of these functions might still benefit from some cleanup
-------------------------------------------------------------------------------}

sizeAndId :: (BlockSize, BlockInfo blockId) -> FileSlotInfo blockId
sizeAndId (size, bInfo) = FileSlotInfo size (bbid bInfo)

-- | @blockId@ and 'SlotNo' for the last 'SlotOffset' in the map
lastSlotInfo :: forall blockId.
                Map SlotOffset (Word64, BlockInfo blockId)
             -> Maybe (blockId, SlotNo)
lastSlotInfo mp = f <$> safeMaximumOn getSlot (Map.elems mp)
  where
    f :: (SlotOffset, BlockInfo blockId) -> (blockId, SlotNo)
    f (_, bInfo) = (bbid bInfo, bslot bInfo)

    getSlot :: (SlotOffset, BlockInfo blockId) -> SlotNo
    getSlot (_, bInfo) = bslot bInfo
