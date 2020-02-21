{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | In-memory model implementation of 'VolatileDB'
module Test.Ouroboros.Storage.VolatileDB.Model
    ( DBModel (..)
    , initDBModel
      -- * Basic API
    , closeModel
    , isOpenModel
    , reOpenModel
    , getBlockComponentModel
    , putBlockModel
    , garbageCollectModel
    , getBlockIdsModel
    , getSuccessorsModel
    , getPredecessorModel
    , getIsMemberModel
    , getMaxSlotNoModel
      -- * Corruptions
    , runCorruptionsModel
      -- * Exported for testing purposes
    , getDBFileIds
    , getDBFiles
    , getCurrentFile
    , blockIndex
    , blockIds
    , BlocksInFile (..)
    ) where

import           Control.Monad.Except (MonadError, throwError)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Storage.Common (BinaryInfo (..),
                     BlockComponent (..), extractHeader)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import           Ouroboros.Consensus.Storage.VolatileDB.API
import           Ouroboros.Consensus.Storage.VolatileDB.Util (filePath, parseFd)

import           Test.Ouroboros.Storage.TestBlock (Corruptions,
                     FileCorruption (..))

data DBModel blockId = DBModel {
      blocksPerFile :: BlocksPerFile
      -- ^ How many blocks each file has (should follow the real
      -- implementation).
    , open          :: Bool
      -- ^ Indicates if the DB is open.
    , fileIndex     :: Map FileId (BlocksInFile blockId)
      -- ^ What each file contains in the real implementation.
      --
      -- INVARIANT: the map is never empty.
      --
      -- INVARIANT: the 'BlocksInFile' associated with the highest 'FileId'
      -- has always fewer than 'blocksPerFile' blocks.
    , maxSlotNo     :: MaxSlotNo
      -- ^ Highest ever stored 'SlotNo'.
      --
      -- Note that this doesn't need to match the result of
      -- 'computeMaxSlotNo', as we could have seen a slot that has been
      -- garbage collected already. This is unlikely to happen in practice,
      -- though.
    }
  deriving (Show, Generic)

initDBModel :: BlocksPerFile -> DBModel blockId
initDBModel blocksPerFile = DBModel {
      blocksPerFile   = blocksPerFile
    , open            = True
    , fileIndex       = Map.singleton 0 emptyFile
    , maxSlotNo       = NoMaxSlotNo
    }

blockIndex
  :: Ord blockId
  => DBModel blockId
  -> Map blockId (BlockInfo blockId, ByteString)
blockIndex = foldMap fileToBlockIndex . fileIndex

blockIds :: DBModel blockId -> [blockId]
blockIds = concatMap fileBlockIds . fileIndex

getBlockToPredecessor
  :: Ord blockId
  => DBModel blockId -> Map blockId (WithOrigin blockId)
getBlockToPredecessor = foldMap fileBlockToPredecessor . fileIndex

getCurrentFile :: DBModel blockId -> FsPath
getCurrentFile = filePath . getCurrentFileId

getCurrentFileId :: DBModel blockId -> FileId
getCurrentFileId =
      -- Relies on the first invariant of 'fileIndex'
      maybe (error "empty fileIndex") (fst . fst)
    . Map.maxViewWithKey
    . fileIndex

-- | Restore the invariants of 'fileIndex'
restoreInvariants :: DBModel blockId -> DBModel blockId
restoreInvariants dbm = case fst <$> Map.maxViewWithKey fileIndex of
    Nothing
      -> dbm {
           fileIndex = Map.insert 0 emptyFile fileIndex
         }
    Just (fileId, file)
      | let n = nbBlocksInFile file
      , fromIntegral n == unBlocksPerFile blocksPerFile
      , let newFileId = fileId + 1
        -- The file is full, start a new one
      -> dbm {
           fileIndex = Map.insert newFileId emptyFile fileIndex
         }
      | otherwise
        -- The file is not full, continue appending to it
      -> dbm
  where
    DBModel { blocksPerFile, fileIndex } = dbm

computeMaxSlotNo :: Map FileId (BlocksInFile blockId) -> MaxSlotNo
computeMaxSlotNo = foldMap fileMaxSlotNo

whenOpen :: MonadError VolatileDBError m
         => DBModel blockId
         -> a
         -> m a
whenOpen dbm k
    | open dbm
    = return k
    | otherwise
    = throwError $ UserError ClosedDBError

getDBFileIds :: DBModel blockId -> [FileId]
getDBFileIds = Map.keys . fileIndex

getDBFiles :: DBModel blockId -> [FsPath]
getDBFiles = map filePath . getDBFileIds

{------------------------------------------------------------------------------
  BlocksInFile
------------------------------------------------------------------------------}

-- | The blocks in a file, in the same order as they would be written to the
-- file in the real implementation.
newtype BlocksInFile blockId = BlocksInFile {
      getBlocksInFile :: [(BlockInfo blockId, ByteString)]
    }
  deriving (Show, Generic)

emptyFile :: BlocksInFile blockId
emptyFile = BlocksInFile []

nbBlocksInFile :: BlocksInFile blockid -> Int
nbBlocksInFile = length . getBlocksInFile

appendBlock
  :: BlockInfo blockId -> ByteString
  -> BlocksInFile blockId
  -> BlocksInFile blockId
appendBlock blockInfo bytes (BlocksInFile blocks) =
    BlocksInFile (blocks ++ [(blockInfo, bytes)])

-- | The highest slot number in this file.
fileMaxSlotNo :: BlocksInFile blockId -> MaxSlotNo
fileMaxSlotNo = foldMap (MaxSlotNo . bslot . fst) . getBlocksInFile

fileToBlockIndex
  :: Ord blockId
  => BlocksInFile blockId
  -> Map blockId (BlockInfo blockId, ByteString)
fileToBlockIndex = Map.fromList . map addKey . getBlocksInFile
  where
    addKey v@(blockInfo, _) = (bbid blockInfo, v)

fileBlockIds :: BlocksInFile blockId -> [blockId]
fileBlockIds = map (bbid . fst) . getBlocksInFile

fileBlockToPredecessor
  :: Ord blockId
  => BlocksInFile blockId
  -> Map blockId (WithOrigin blockId)
fileBlockToPredecessor (BlocksInFile blocks) = Map.fromList
    [ (bbid, bpreBid)
    | (BlockInfo { bbid, bpreBid }, _) <- blocks
    ]

fileSize :: Integral a => BlocksInFile blockId -> a
fileSize = fromIntegral . sum . map (BL.length . snd) . getBlocksInFile

-- | Only include blocks that come before the given offset.
--
-- > fileTruncateTo (fileSize blocks) == blocks
fileTruncateTo :: Word64 -> BlocksInFile blockId -> BlocksInFile blockId
fileTruncateTo validUntil = BlocksInFile . go 0 . getBlocksInFile
  where
    -- Invariant: offset <= validUntil
    go
      :: Word64
      -> [(BlockInfo blockId, ByteString)]
      -> [(BlockInfo blockId, ByteString)]
    go offset = \case
      []
        -> []
      b@(_, bytes):bs
        | let newOffset = offset + fromIntegral (BL.length bytes)
        , newOffset <= validUntil
        -> b:go newOffset bs
        | otherwise
        -> []

{------------------------------------------------------------------------------
  Model API
------------------------------------------------------------------------------}

closeModel :: DBModel blockId -> DBModel blockId
closeModel dbm = dbm { open = False }

isOpenModel :: DBModel blockId -> Bool
isOpenModel = open

reOpenModel :: DBModel blockId -> DBModel blockId
reOpenModel dbm@DBModel { fileIndex }
    | open dbm
    = dbm
    | otherwise
    = restoreInvariants $ dbm {
        open      = True
        -- When restarting, we forget the 'MaxSlotNo' and compute it based on
        -- the blocks we still have.
      , maxSlotNo = computeMaxSlotNo fileIndex
      }

getBlockComponentModel
  :: Ord blockId
  => BlockComponent (VolatileDB blockId m) b
  -> blockId
  -> DBModel blockId
  -> Either VolatileDBError (Maybe b)
getBlockComponentModel blockComponent blockId dbm = whenOpen dbm $
    (`extractBlockComponent` blockComponent) <$>
    Map.lookup blockId (blockIndex dbm)

extractBlockComponent
  :: forall m blockId b.
     (BlockInfo blockId, ByteString)
  -> BlockComponent (VolatileDB blockId m) b
  -> b
extractBlockComponent (BlockInfo {..}, bytes) = go
  where
    go :: forall b'. BlockComponent (VolatileDB blockId m) b' -> b'
    go = \case
      GetBlock      -> ()
      GetRawBlock   -> bytes
      GetHeader     -> ()
      GetRawHeader  -> header
      GetHash       -> bbid
      GetSlot       -> bslot
      GetIsEBB      -> bisEBB
      GetBlockSize  -> fromIntegral $ BL.length bytes
      GetHeaderSize -> bheaderSize
      GetPure a     -> a
      GetApply f bc -> go f $ go bc

    header = extractHeader BinaryInfo {
        binaryBlob   = bytes
      , headerOffset = bheaderOffset
      , headerSize   = bheaderSize
      }

putBlockModel
  :: forall blockId. Ord blockId
  => BlockInfo blockId
  -> Builder
  -> DBModel blockId
  -> Either VolatileDBError (DBModel blockId)
putBlockModel blockInfo@BlockInfo { bbid, bslot } builder dbm = whenOpen dbm $
    case Map.lookup bbid (blockIndex dbm) of
      -- Block already stored
      Just _  -> dbm
      Nothing -> restoreInvariants $ dbm {
          -- The invariants guarantee that @getCurrentFileId dbm@ is in
          -- @fileIndex@.
          fileIndex  = Map.adjust
            (appendBlock blockInfo bytes)
            (getCurrentFileId dbm)
            fileIndex
        , maxSlotNo  = maxSlotNo `max` MaxSlotNo bslot
        }
  where
    DBModel { fileIndex, maxSlotNo } = dbm

    bytes = toLazyByteString builder

garbageCollectModel
  :: forall blockId.
     SlotNo
  -> DBModel blockId
  -> Either VolatileDBError (DBModel blockId)
garbageCollectModel slot dbm = whenOpen dbm $
     keepCurrentFile (getCurrentFileId dbm) $ dbm {
         fileIndex = fileIndex'
       }
  where
    (_garbageCollected, fileIndex') = Map.partition canGC (fileIndex dbm)

    canGC :: BlocksInFile blockId -> Bool
    canGC file = fileMaxSlotNo file < MaxSlotNo slot

    -- | Make sure that the current file doesn't change, even when it was
    -- garbage collected. We do this to keep in sync with the implementation,
    -- which garbage collects the current file (i.e., truncates) when
    -- possible, so we must recreate the (empty) file.
    --
    -- When we change the real implementation to never garbage collect the
    -- current file, this function can be removed and 'canGC' can be extended
    -- to ignore the current file.
    keepCurrentFile :: FileId -> DBModel blockId -> DBModel blockId
    keepCurrentFile prevCurrentFileId dbm' = dbm' {
          fileIndex = Map.insertWith
            (\_new existing -> existing)
            prevCurrentFileId
            emptyFile
            (fileIndex dbm')
        }

getBlockIdsModel :: DBModel blockId -> Either VolatileDBError [blockId]
getBlockIdsModel dbm = whenOpen dbm $ blockIds dbm

getSuccessorsModel
  :: forall blockId. Ord blockId
  => DBModel blockId
  -> Either VolatileDBError (WithOrigin blockId -> Set blockId)
getSuccessorsModel dbm = whenOpen dbm $ \predecessor ->
    fromMaybe Set.empty $ Map.lookup predecessor successors
  where
    successors :: Map (WithOrigin blockId) (Set blockId)
    successors = Map.foldrWithKey'
      (\blockId predecessor ->
        Map.insertWith (<>) predecessor (Set.singleton blockId))
      Map.empty
      (getBlockToPredecessor dbm)

getPredecessorModel
  :: Ord blockId
  => DBModel blockId
  -> Either VolatileDBError (blockId -> WithOrigin blockId)
getPredecessorModel dbm = whenOpen dbm $ \blockId ->
    fromMaybe (error msg) $ Map.lookup blockId $ getBlockToPredecessor dbm
   where
     msg = "precondition violated: block not member of the VolatileDB"

getIsMemberModel
  :: Ord blockId
  => DBModel blockId
  -> Either VolatileDBError (blockId -> Bool)
getIsMemberModel dbm = whenOpen dbm $ \blockId ->
    Map.member blockId (blockIndex dbm)

getMaxSlotNoModel
  :: DBModel blockId
  -> Either VolatileDBError MaxSlotNo
getMaxSlotNoModel dbm = whenOpen dbm $ maxSlotNo dbm

{------------------------------------------------------------------------------
  Corruptions
------------------------------------------------------------------------------}

runCorruptionsModel :: Corruptions -> DBModel blockId -> DBModel blockId
runCorruptionsModel corrs dbm = foldr (uncurry runCorruption) dbm corrs

runCorruption
  :: FileCorruption
  -> FsPath
  -> DBModel blockId
  -> DBModel blockId
runCorruption corruption file dbm@DBModel { fileIndex } = case corruption of
    DeleteFile      -> dbm {
          fileIndex = Map.delete fileId fileIndex
        }
    DropLastBytes n -> dbm {
          fileIndex = Map.adjust (fileTruncateTo validBytes) fileId fileIndex
        }
      where
        validBytes | n > size  = 0
                   | otherwise = size - n
    -- When we simulate corruption, we will do a bitflip in the filesystem. In
    -- the model, this corresponds to truncation, forcing the implementation
    -- to detect the corruption and to truncate accordingly.
    Corrupt offset  -> dbm {
          fileIndex = Map.adjust (fileTruncateTo validBytes) fileId fileIndex
        }
      where
        validBytes = offset `mod` size
  where
    fileId = unsafeParseFd file
    size   = fileSize (fileIndex Map.! fileId)

unsafeParseFd :: FsPath -> FileId
unsafeParseFd file = fromMaybe
    (error $ "Could not parse filename " <> show file)
    (parseFd file)
