{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

-- | In-memory model implementation of 'VolatileDB'
module Test.Ouroboros.Storage.VolatileDB.Model (
    DBModel (..)
  , initDBModel
    -- * Basic API
  , closeModel
  , filterByPredecessorModel
  , garbageCollectModel
  , getBlockComponentModel
  , getBlockInfoModel
  , getMaxSlotNoModel
  , isOpenModel
  , putBlockModel
  , reOpenModel
    -- * Corruptions
  , runCorruptionsModel
    -- * Exported for testing purposes
  , BlocksInFile (..)
  , blockHashes
  , blockIndex
  , getCurrentFile
  , getDBFileIds
  , getDBFiles
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.ByteString.Lazy as BL
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.Common (BlockComponent (..),
                     extractHeader)
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath)
import           Ouroboros.Consensus.Storage.Serialisation
                     (BinaryBlockInfo (..), EncodeDisk (..),
                     HasBinaryBlockInfo (..))
import           Ouroboros.Consensus.Storage.VolatileDB
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types (FileId,
                     unBlocksPerFile)
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util (filePath,
                     parseFd)

import           Test.Ouroboros.Storage.TestBlock (Corruptions,
                     FileCorruption (..))

data DBModel blk = DBModel {
      blocksPerFile :: BlocksPerFile
      -- ^ How many blocks each file has (should follow the real
      -- implementation).
    , open          :: Bool
      -- ^ Indicates if the DB is open.
    , fileIndex     :: Map FileId (BlocksInFile blk)
      -- ^ What each file contains in the real implementation.
      --
      -- INVARIANT: the map is never empty.
      --
      -- INVARIANT: the 'BlocksInFile' associated with the highest 'FileId'
      -- has always fewer than 'blocksPerFile' blocks.
    , codecConfig   :: CodecConfig blk
    }
  deriving (Generic)

deriving instance (Show blk, Show (CodecConfig blk)) => Show (DBModel blk)

initDBModel :: BlocksPerFile -> CodecConfig blk -> DBModel blk
initDBModel blocksPerFile codecConfig = DBModel {
      blocksPerFile = blocksPerFile
    , open          = True
    , fileIndex     = Map.singleton 0 emptyFile
    , codecConfig   = codecConfig
    }

blockIndex :: HasHeader blk => DBModel blk -> Map (HeaderHash blk) blk
blockIndex = foldMap fileToBlockIndex . fileIndex

blockHashes :: HasHeader blk => DBModel blk -> [HeaderHash blk]
blockHashes = concatMap fileHashes . fileIndex

getBlockToPredecessor ::
     (HasHeader blk, GetPrevHash blk)
  => DBModel blk
  -> Map (HeaderHash blk) (ChainHash blk)
getBlockToPredecessor DBModel { fileIndex } =
    foldMap fileBlockToPredecessor fileIndex

getCurrentFile :: DBModel blk -> FsPath
getCurrentFile = filePath . getCurrentFileId

getCurrentFileId :: DBModel blk -> FileId
getCurrentFileId =
      -- Relies on the first invariant of 'fileIndex'
      maybe (error "empty fileIndex") (fst . fst)
    . Map.maxViewWithKey
    . fileIndex

-- | Restore the invariants of 'fileIndex'
restoreInvariants :: DBModel blk -> DBModel blk
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

whenOpen ::
     MonadError (VolatileDBError blk) m
  => DBModel blk
  -> a
  -> m a
whenOpen dbm k
    | open dbm
    = return k
    | otherwise
    = throwError $ ApiMisuse $ ClosedDBError Nothing

getDBFileIds :: DBModel blk -> [FileId]
getDBFileIds = Map.keys . fileIndex

getDBFiles :: DBModel blk -> [FsPath]
getDBFiles = map filePath . getDBFileIds

{------------------------------------------------------------------------------
  BlocksInFile
------------------------------------------------------------------------------}

-- | The blocks in a file, in the same order as they would be written to the
-- file in the real implementation.
newtype BlocksInFile blk = BlocksInFile {
      getBlocksInFile :: [blk]
    }
  deriving (Show, Generic)

emptyFile :: BlocksInFile blk
emptyFile = BlocksInFile []

nbBlocksInFile :: BlocksInFile blk -> Int
nbBlocksInFile = length . getBlocksInFile

appendBlock :: blk -> BlocksInFile blk -> BlocksInFile blk
appendBlock blk (BlocksInFile blks) =
    BlocksInFile (blks ++ [blk])

-- | The highest slot number in this file.
fileMaxSlotNo :: HasHeader blk => BlocksInFile blk -> MaxSlotNo
fileMaxSlotNo = foldMap (MaxSlotNo . blockSlot) . getBlocksInFile

fileToBlockIndex ::
     HasHeader blk
  => BlocksInFile blk
  -> Map (HeaderHash blk) blk
fileToBlockIndex = Map.fromList . map addKey . getBlocksInFile
  where
    addKey blk = (blockHash blk, blk)

fileHashes :: HasHeader blk => BlocksInFile blk -> [HeaderHash blk]
fileHashes = map blockHash . getBlocksInFile

fileBlockToPredecessor ::
     (HasHeader blk, GetPrevHash blk)
  => BlocksInFile blk
  -> Map (HeaderHash blk) (ChainHash blk)
fileBlockToPredecessor (BlocksInFile blks) = Map.fromList
    [ (blockHash blk, blockPrevHash blk)
    | blk <- blks
    ]

fileSize ::
     (Integral a, EncodeDisk blk blk)
  => CodecConfig blk
  -> BlocksInFile blk
  -> a
fileSize ccfg = sum . map (blockSize ccfg) . getBlocksInFile

-- | Only include blocks that come before the given offset.
--
-- > fileTruncateTo ccfg (fileSize blocks) blocks == blocks
fileTruncateTo ::
     forall blk. EncodeDisk blk blk
  => CodecConfig blk
  -> Word64
  -> BlocksInFile blk
  -> BlocksInFile blk
fileTruncateTo ccfg validUntil = BlocksInFile . go 0 . getBlocksInFile
  where
    -- Invariant: offset <= validUntil
    go :: Word64 -> [blk] -> [blk]
    go offset = \case
      []
        -> []
      blk:blks
        | let newOffset = offset + blockSize ccfg blk
        , newOffset <= validUntil
        -> blk : go newOffset blks
        | otherwise
        -> []

blockSize :: (Integral a, EncodeDisk blk blk) => CodecConfig blk -> blk -> a
blockSize ccfg =
      fromIntegral
    . BL.length
    . CBOR.toLazyByteString
    . encodeDisk ccfg

{------------------------------------------------------------------------------
  Model API
------------------------------------------------------------------------------}

closeModel :: DBModel blk -> DBModel blk
closeModel dbm = dbm { open = False }

isOpenModel :: DBModel blk -> Bool
isOpenModel = open

reOpenModel :: DBModel blk -> DBModel blk
reOpenModel dbm
    | open dbm
    = dbm
    | otherwise
    = restoreInvariants $ dbm { open = True }

getBlockComponentModel ::
     forall blk b.
     ( HasHeader blk
     , GetHeader blk
     , HasBinaryBlockInfo blk
     , EncodeDisk blk blk
     , HasNestedContent Header blk
     )
  => BlockComponent blk b
  -> HeaderHash blk
  -> DBModel blk
  -> Either (VolatileDBError blk) (Maybe b)
getBlockComponentModel blockComponent hash dbm@DBModel { codecConfig } =
    whenOpen dbm $
      flip go blockComponent <$>
        Map.lookup hash (blockIndex dbm)
  where
    go :: forall b'. blk -> BlockComponent blk b' -> b'
    go blk = \case
        GetVerifiedBlock -> blk  -- We don't verify
        GetBlock         -> blk
        GetRawBlock      -> blockBytes
        GetHeader        -> getHeader blk
        GetRawHeader     -> extractHeader binaryBlockInfo blockBytes
        GetHash          -> blockHash blk
        GetSlot          -> blockSlot blk
        GetIsEBB         -> blockToIsEBB blk
        GetBlockSize     -> fromIntegral $ BL.length blockBytes
        GetHeaderSize    -> headerSize binaryBlockInfo
        GetNestedCtxt    -> case unnest (getHeader blk) of
                              DepPair nestedCtxt _ -> SomeSecond nestedCtxt
        GetPure a        -> a
        GetApply f bc    -> go blk f $ go blk bc
      where
        binaryBlockInfo = getBinaryBlockInfo blk
        blockBytes = CBOR.toLazyByteString $ encodeDisk codecConfig blk

putBlockModel ::
     forall blk. HasHeader blk
  => blk
  -> DBModel blk
  -> Either (VolatileDBError blk) (DBModel blk)
putBlockModel blk dbm = whenOpen dbm $
    case Map.lookup (blockHash blk) (blockIndex dbm) of
      -- Block already stored
      Just _  -> dbm
      Nothing -> restoreInvariants $ dbm {
          -- The invariants guarantee that @getCurrentFileId dbm@ is in
          -- @fileIndex@.
          fileIndex  = Map.adjust
            (appendBlock blk)
            (getCurrentFileId dbm)
            fileIndex
        }
  where
    DBModel { fileIndex } = dbm

garbageCollectModel ::
     forall blk. HasHeader blk
  => SlotNo
  -> DBModel blk
  -> Either (VolatileDBError blk) (DBModel blk)
garbageCollectModel slot dbm = whenOpen dbm $
     dbm { fileIndex = fileIndex' }
  where
    (_garbageCollected, fileIndex') = Map.partitionWithKey canGC (fileIndex dbm)

    canGC :: FileId -> BlocksInFile blk -> Bool
    canGC fileId file =
      fileId /= getCurrentFileId dbm &&
      fileMaxSlotNo file < MaxSlotNo slot

filterByPredecessorModel ::
     forall blk. (HasHeader blk, GetPrevHash blk)
  => DBModel blk
  -> Either (VolatileDBError blk) (ChainHash blk -> Set (HeaderHash blk))
filterByPredecessorModel dbm = whenOpen dbm $ \predecessor ->
    fromMaybe Set.empty $ Map.lookup predecessor successors
  where
    successors :: Map (ChainHash blk) (Set (HeaderHash blk))
    successors = Map.foldrWithKey'
      (\hash prevHash ->
        Map.insertWith (<>) prevHash (Set.singleton hash))
      Map.empty
      (getBlockToPredecessor dbm)

getBlockInfoModel ::
     (HasHeader blk, GetPrevHash blk, HasBinaryBlockInfo blk)
  => DBModel blk
  -> Either (VolatileDBError blk) (HeaderHash blk -> Maybe (BlockInfo blk))
getBlockInfoModel dbm = whenOpen dbm $ \hash ->
    extractBlockInfo <$> Map.lookup hash (blockIndex dbm)

getMaxSlotNoModel ::
     HasHeader blk
  => DBModel blk
  -> Either (VolatileDBError blk) MaxSlotNo
getMaxSlotNoModel dbm = whenOpen dbm $
    foldMap fileMaxSlotNo $ fileIndex dbm

{------------------------------------------------------------------------------
  Corruptions
------------------------------------------------------------------------------}

runCorruptionsModel ::
     EncodeDisk blk blk
  => Corruptions
  -> DBModel blk
  -> DBModel blk
runCorruptionsModel corrs dbm = foldr (uncurry runCorruption) dbm corrs

runCorruption ::
     EncodeDisk blk blk
  => FileCorruption
  -> FsPath
  -> DBModel blk
  -> DBModel blk
runCorruption corruption file dbm@DBModel { fileIndex, codecConfig = ccfg } =
    case corruption of
      DeleteFile      -> dbm {
            fileIndex = Map.delete fileId fileIndex
          }
      DropLastBytes n -> dbm {
            fileIndex = Map.adjust (fileTruncateTo ccfg validBytes) fileId fileIndex
          }
        where
          validBytes | n > size  = 0
                     | otherwise = size - n
      -- When we simulate corruption, we will do a bitflip in the filesystem. In
      -- the model, this corresponds to truncation, forcing the implementation
      -- to detect the corruption and to truncate accordingly.
      Corrupt offset  -> dbm {
            fileIndex = Map.adjust (fileTruncateTo ccfg validBytes) fileId fileIndex
          }
        where
          validBytes = offset `mod` size
  where
    fileId = unsafeParseFd file
    size   = fileSize ccfg (fileIndex Map.! fileId)

unsafeParseFd :: FsPath -> FileId
unsafeParseFd file = fromMaybe
    (error $ "Could not parse filename " <> show file)
    (parseFd file)
