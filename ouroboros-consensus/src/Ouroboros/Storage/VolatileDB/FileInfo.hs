{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Information about the files stored by the volatile DB
--
-- Intended for qualified import.
module Ouroboros.Storage.VolatileDB.FileInfo (
    FileInfo     -- opaque
  , FileSlotInfo -- opaque
    -- * Construction
  , empty
  , addSlot
  , fromParsedInfo
  , mkFileSlotInfo
    -- * Queries
  , canGC
  , blockIds
  , isFull
  , maxSlot
  , maxSlotInFiles
  , getHandle
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

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Common
import           Ouroboros.Storage.VolatileDB.Types
import           Ouroboros.Storage.VolatileDB.Util


{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- The Internal information the db keeps for each file.
data FileInfo blockId h = MkFileInfo {
      fLatestSlot :: !MaxSlotNo
    , fNBlocks    :: !Int
    , fContents   :: !(Map SlotOffset (FileSlotInfo blockId))
    , fReadHandle :: !(Handle h)
    } deriving (Show, Generic, NoUnexpectedThunks)

-- | Information about a slot in a file
data FileSlotInfo blockId = FileSlotInfo {
      fsBlockSize :: !BlockSize
    , fsBlockId   :: !blockId
    } deriving (Show, Generic, NoUnexpectedThunks)

{-------------------------------------------------------------------------------
  Construction
-------------------------------------------------------------------------------}

empty :: Handle h -> FileInfo blockId h
empty hndl = MkFileInfo {
      fLatestSlot = NoMaxSlotNo
    , fNBlocks    = 0
    , fContents   = Map.empty
    , fReadHandle = hndl
    }

-- | Adds slot to a file.
addSlot :: SlotNo
        -> SlotOffset
        -> FileSlotInfo blockId
        -> FileInfo blockId h -> FileInfo blockId h
addSlot slotNo slotOffset slotInfo fileInfo@MkFileInfo{..} = fileInfo {
      fLatestSlot = updateSlotNoBlockId fLatestSlot [slotNo]
    , fNBlocks    = fNBlocks + 1
    , fContents   = Map.insert slotOffset slotInfo fContents
    , fReadHandle = fReadHandle
    }

-- | Construct 'FileInfo' from the parser result
--
-- Additionally returns information about the last 'SlotOffset' in the file,
-- unless the file is empty.
fromParsedInfo :: ParsedInfo blockId
               -> Handle h
               -> (FileInfo blockId h, Maybe (blockId, SlotNo))
fromParsedInfo parsed hndl =
    case lastSlotInfo parsed' of
      Nothing     -> (MkFileInfo NoMaxSlotNo   nBlocks contents hndl, Nothing)
      Just (b, s) -> (MkFileInfo (MaxSlotNo s) nBlocks contents hndl, Just (b, s))
  where
    parsed'  = Map.fromList parsed
    nBlocks  = Map.size parsed'
    contents = Map.map sizeAndId parsed'

mkFileSlotInfo :: BlockSize -> blockId -> FileSlotInfo blockId
mkFileSlotInfo = FileSlotInfo

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Check if this file can be GCed
canGC :: FileInfo blockId h -> SlotNo -> Bool
canGC MkFileInfo{..} slot =
    case fLatestSlot of
      NoMaxSlotNo      -> True
      MaxSlotNo latest -> latest < slot

-- | All block IDs in this file
blockIds :: FileInfo blockId h -> [blockId]
blockIds MkFileInfo{..} = fsBlockId <$> Map.elems fContents

-- | Has this file reached its maximum size?
isFull :: Int -> FileInfo blockId h -> Bool
isFull maxNumBlocks MkFileInfo{..} = fNBlocks >= fromIntegral maxNumBlocks

maxSlot :: FileInfo blockId h -> MaxSlotNo
maxSlot = fLatestSlot

maxSlotInFiles :: [FileInfo blockId h] -> MaxSlotNo
maxSlotInFiles = maxSlotNoFromMaybe
               . safeMaximum
               . mapMaybe (maxSlotNoToMaybe . maxSlot)

getHandle :: FileInfo blockId h -> Handle h
getHandle = fReadHandle

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
lastSlotInfo mp = getBlockId <$> safeMaximumOn getSlotNo (Map.elems mp)
  where
    getBlockId :: (SlotOffset, BlockInfo blockId) -> (blockId, SlotNo)
    getBlockId (_, bInfo) = (bbid bInfo, bslot bInfo)

    getSlotNo :: (SlotOffset, BlockInfo blockId) -> SlotNo
    getSlotNo (_, bInfo) = bslot bInfo
