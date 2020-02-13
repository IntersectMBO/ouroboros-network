{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
-- | In-memory Model implementation of 'VolatileDB' for testing
module Test.Ouroboros.Storage.VolatileDB.Model
    (
      DBModel (..)
    , initDBModel
    -- * basic api
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
    -- * corruptions
    , runCorruptionModel
    , createFileModel
    , createInvalidFileModel
    , duplicateBlockModel
    -- * public for better test tagging and generating
    , getCurrentFile
    , getBlockId
    , getDBFiles
    , getDBBlocksWithFiles
    ) where

import           Control.Monad
import           Control.Monad.State (MonadState, get, modify, put)
import           Data.Bifunctor
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.List (find, sortOn, splitAt)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Data.Word

import           Ouroboros.Network.Block (MaxSlotNo (..), SlotNo)
import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (IsEBB)
import           Ouroboros.Consensus.Util (safeMaximum)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal
import           Ouroboros.Storage.VolatileDB.Util

import           Test.Ouroboros.Storage.VolatileDB.TestBlock

type Index blockId = Map FsPath (MaxSlotNo, [(blockId, WithOrigin blockId)])

data DBModel blockId = DBModel {
      blocksPerFile  :: Int
      -- ^ How many blocks each file has (should follow the real Impl).
    , open           :: Bool
      -- ^ Indicates if the db is open.
    , mp             :: Map blockId (SlotNo, IsEBB, BinaryInfo ByteString)
      -- ^ Superset of blocks in db. Some of them may be gced already.
    , latestGarbaged :: Maybe SlotNo
      -- ^ Last gced slot.
    , index          :: Index blockId
      -- ^ What each file contains in the Impl.
    , maxSlotNo      :: MaxSlotNo
      -- ^ Highest ever stored SlotNo.
    } deriving Show

initDBModel :: Int -> DBModel blockId
initDBModel bpf = DBModel {
      blocksPerFile  = bpf
    , open           = True
    , mp             = Map.empty
    , latestGarbaged = Nothing
    , index          = Map.singleton (Internal.filePath 0) newFileInfo
    , maxSlotNo      = NoMaxSlotNo
    }

newFileInfo :: (MaxSlotNo, [b])
newFileInfo = (NoMaxSlotNo, [])

{------------------------------------------------------------------------------
  DBModel getter utilities
------------------------------------------------------------------------------}

-- ^ The current open file. If the db is closed, this is the next file it
-- should write to.
getCurrentFile :: DBModel blockId -> FsPath
getCurrentFile dbm = filePath $ fromMaybe 0 $
    safeMaximum (unsafeParseFd <$> Map.keys (index dbm))

nextFile :: DBModel blockId -> FsPath
nextFile dbm = case safeMaximum (unsafeParseFd <$> Map.keys (index dbm)) of
    Nothing  -> filePath 0
    Just fid -> filePath $ 1 + fid

unsafeLookupIndex :: DBModel blockId
                  -> FsPath
                  -> (MaxSlotNo, [(blockId, WithOrigin blockId)])
unsafeLookupIndex DBModel {..} file =
    fromMaybe
      (error $ unwords ["file", show file, "does not exist in Index"])
      (Map.lookup file index)

-- | Returns all the file in the Index, sorted on their 'FileId'.
sortedFilesOfIndex :: DBModel blockId
                   -> [(FsPath, MaxSlotNo)]
sortedFilesOfIndex DBModel {..} =
    sortOn (unsafeParseFd . fst) $ fmap (second fst) $ Map.toList index

-- | It throws an error if the file is not found in the index.
unsafeGetBlocks :: DBModel blockId
                -> FsPath
                -> [(blockId, WithOrigin blockId)]
unsafeGetBlocks file = snd . unsafeLookupIndex file

unsafeLastFd :: DBModel blockId -> Maybe FileId
unsafeLastFd DBModel {..} = fst $ findLastFd (Map.keys index)

getBlockId :: DBModel blockId -> [(blockId, WithOrigin blockId)]
getBlockId DBModel {..} = concat $ snd <$> Map.elems index

openNewFile :: DBModel blockId -> Index blockId
openNewFile dbm@DBModel{..} = Map.insert (nextFile dbm) newFileInfo index

getDBFiles :: DBModel blockId -> [FsPath]
getDBFiles DBModel {..} = Map.keys index

getDBBlocksWithFiles :: DBModel blockId -> [(FsPath, blockId)]
getDBBlocksWithFiles DBModel {..} =
  concat $ (\(f,(_, bs)) -> map ((f,) . fst) bs) <$> (Map.toList index)

unsafeGetSlot :: Ord blockId => DBModel blockId -> blockId -> SlotNo
unsafeGetSlot DBModel {..} blockId = first3 $ (Map.!) mp blockId
  where
    first3 (a, _, _) = a

{------------------------------------------------------------------------------
  Model Api
------------------------------------------------------------------------------}

closeModel :: MonadState (DBModel blockId) m => m ()
closeModel = do
    dbm <- get
    put $ dbm {open = False}

isOpenModel :: MonadState (DBModel blockId) m => m Bool
isOpenModel = do
    DBModel {..} <- get
    return open

reOpenModel :: MonadState (DBModel blockId) m
            => m ()
reOpenModel = do
    dbm <- get
    dbm' <- if not $ open dbm
            then recover dbm
            else return dbm
    put dbm' {open = True}

getBlockComponentModel
  :: forall m blockId b. (MonadState (DBModel blockId) m, Ord blockId)
  => ThrowCantCatch VolatileDBError m
  -> BlockComponent (VolatileDB blockId m) b
  -> blockId
  -> m (Maybe b)
getBlockComponentModel err blockComponent blockId = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return $
      (\info -> extractBlockComponent blockId info blockComponent) <$>
      Map.lookup blockId mp

extractBlockComponent
  :: blockId
  -> (SlotNo, IsEBB, BinaryInfo ByteString)
  -> BlockComponent (VolatileDB blockId m) b
  -> b
extractBlockComponent blockId info@(slot, isEBB, block) = \case
    GetBlock      -> ()
    GetRawBlock   -> binaryBlob block
    GetHeader     -> ()
    GetRawHeader  -> extractHeader block
    GetHash       -> blockId
    GetSlot       -> slot
    GetIsEBB      -> isEBB
    GetBlockSize  -> fromIntegral $ BL.length $ binaryBlob block
    GetHeaderSize -> headerSize block
    GetPure a     -> a
    GetApply f bc ->
      extractBlockComponent blockId info f $
      extractBlockComponent blockId info bc


putBlockModel :: forall blockId m.
                 MonadState (DBModel blockId) m
              => Ord blockId
              => ThrowCantCatch VolatileDBError m
              -> BlockInfo blockId
              -> Builder
              -> m ()
putBlockModel err BlockInfo{..} builder = do
    -- This depends on the exact sequence of the operations in the real Impl.
    -- If anything changes there, then this will also need change.
    dbm@DBModel {..} <- get
    let currentFile = getCurrentFile dbm
        (mbid, bids) = unsafeLookupIndex dbm currentFile
        index' = Map.insert currentFile
                            (max mbid (MaxSlotNo bslot), (bbid, bpreBid):bids)
                            index
        binaryInfo = BinaryInfo {
            binaryBlob   = toLazyByteString builder
          , headerOffset = bheaderOffset
          , headerSize   = bheaderSize
          }
    whenClosedUserError dbm err
    case Map.lookup bbid mp of
      Just _block -> return ()
      Nothing -> do
        put dbm {
            mp        = Map.insert bbid (bslot, bisEBB, binaryInfo) mp
          , index     = index'
          , maxSlotNo = maxSlotNo `max` MaxSlotNo bslot
          }
        when (1 + length bids == blocksPerFile)
          createFileModel

garbageCollectModel :: forall m blockId
                     . MonadState (DBModel blockId) m
                    => ThrowCantCatch VolatileDBError m
                    -> SlotNo
                    -> m ()
garbageCollectModel err gcSlot = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    modify $ \dbm' -> dbm' { latestGarbaged = max latestGarbaged (Just gcSlot) }
    collectFiles (getCurrentFile dbm) $ sortedFilesOfIndex dbm
  where

    collectFiles :: FsPath -> [(FsPath, MaxSlotNo)] -> m ()
    collectFiles currentFile = mapM_ collectFile
      where
        collectFile :: (FsPath, MaxSlotNo)
                    -> m ()
        collectFile (path, fileMaxSlotNo)
          | fileMaxSlotNo >= MaxSlotNo gcSlot
          = return ()
          | path == currentFile
          = modifyIndex $ Map.insert path (NoMaxSlotNo,[])
          | otherwise
          = modifyIndex $ Map.delete path

getBlockIdsModel :: forall m blockId
                 . MonadState (DBModel blockId) m
                 => ThrowCantCatch VolatileDBError m
                 -> m [blockId]
getBlockIdsModel err = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return $ fst <$> getBlockId dbm

getSuccessorsModel :: forall m blockId
                   . MonadState (DBModel blockId) m
                   => Ord blockId
                   => ThrowCantCatch VolatileDBError m
                   -> m (WithOrigin blockId -> Set blockId)
getSuccessorsModel err = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return $ \bid ->
      Set.fromList $ fst <$> filter ((==bid) . snd) (getBlockId dbm)

getPredecessorModel :: forall m blockId
                    . MonadState (DBModel blockId) m
                    => Ord blockId
                    => ThrowCantCatch VolatileDBError m
                    -> m (blockId -> WithOrigin blockId)
getPredecessorModel err = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return $ \bid ->
      maybe (error msg) snd $ find ((==bid) . fst) (getBlockId dbm)
  where
    msg = "precondition violated: block not member of the VolatileDB"

getIsMemberModel :: MonadState (DBModel blockId) m
                 => Ord blockId
                 => ThrowCantCatch VolatileDBError m
                 -> m (blockId -> Bool)
getIsMemberModel err = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return (\bid -> Map.member bid mp)

getMaxSlotNoModel :: forall m blockId
                  . MonadState (DBModel blockId) m
                  => ThrowCantCatch VolatileDBError m
                  -> m MaxSlotNo
getMaxSlotNoModel err = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    return maxSlotNo

{------------------------------------------------------------------------------
  Corruptions
------------------------------------------------------------------------------}

runCorruptionModel :: MonadState (DBModel BlockId) m
                   => Corruptions
                   -> m ()
runCorruptionModel corrs = do
    dbm <- get
    let dbm' = foldr corruptDBModel dbm corrs
    put dbm'
  where
    corruptDBModel :: (FileCorruption, FsPath)
                   -> DBModel BlockId
                   -> DBModel BlockId
    corruptDBModel (corr, file) dbm@DBModel{..} = case corr of
        DeleteFile -> dbm {
              mp = Map.withoutKeys mp (Set.fromList $ fst <$> bids)
            , index = Map.delete file index
            }
          where
            bids = unsafeGetBlocks dbm file
        DropLastBytes _n -> dbm {
              mp = mp'
            , index = index'
            }
          where
            bids = unsafeGetBlocks dbm file
            -- we prepend on list of blockIds, so last bytes
            -- are actually at the head of the list.
            (droppedBids, newBids) = splitAt 1 bids
            newMaxSlotNo = foldMap (MaxSlotNo . unsafeGetSlot dbm . fst) newBids
            index' = Map.insert file (newMaxSlotNo, newBids) index
            mp' = Map.withoutKeys mp (Set.fromList $ fst <$> droppedBids)
        AppendBytes _n ->
            -- Appending doesn't actually change anything, since additional
            -- bytes will be truncated. We have taken care that additional
            -- bytes cannot parse as a block. If something like this happens,
            -- there are not much we can do anyway.
            dbm
        PutCorrupted _tb ->
            -- Putting a corrupted block is a no-op since they will be truncated.
            dbm

createFileModel :: forall blockId m. MonadState (DBModel blockId) m
                => m ()
createFileModel = do
    dbm <- get
    put dbm {index = openNewFile dbm}

-- | Creating invalid files is a no-op, since the parser ignores them.
createInvalidFileModel :: forall blockId m. MonadState (DBModel blockId) m
                       => Word32
                       -> m ()
createInvalidFileModel _n = return ()

-- | Inserting a duplicate block is a no-op, since the parser truncates them.
duplicateBlockModel :: forall blockId m. (
                         MonadState (DBModel blockId) m
                       , Typeable blockId
                       , Eq       blockId
                       , Show     blockId
                       )
                    => m ()
duplicateBlockModel = return ()

recover :: MonadState (DBModel blockId) m
        => DBModel blockId
        -> m (DBModel blockId)
recover dbm@DBModel {..} =
    return dbm {
        index     = index'
        -- Recalculate it from the index to match the real implementation
      , maxSlotNo = foldMap fst index'
      }
  where
    lastFd = unsafeLastFd dbm
    ls = Map.toList index
    lessThan = filter (\(_, (_, blocks)) -> length blocks < blocksPerFile) ls
    sorted = sortOn (unsafeParseFd . fst) lessThan
    index' = case (sorted, lastFd) of
      ([], Nothing) -> Map.fromList [(Internal.filePath 0, newFileInfo)]
      (_, Nothing) -> error "invariant violated"
      ([], Just lst) -> let fd' = lst + 1 in
        Map.insert (Internal.filePath fd') newFileInfo index
      (_, Just lst) ->
        let (file, (_msl, _bids)) = last sorted
        in if unsafeParseFd file == lst then index
           else Map.insert (Internal.filePath $ lst + 1) newFileInfo index

{------------------------------------------------------------------------------
  Utilities
------------------------------------------------------------------------------}

whenClosedUserError :: Monad m
                    => DBModel blockId
                    -> ThrowCantCatch VolatileDBError m
                    -> m ()
whenClosedUserError DBModel {..} err =
    when (not open) $
      EH.throwError' err $ UserError ClosedDBError

modifyIndex :: MonadState (DBModel blockId) m
            => (Map FsPath (MaxSlotNo, [(blockId, WithOrigin blockId)])
                  -> Map FsPath (MaxSlotNo, [(blockId, WithOrigin blockId)])
               )
            -> m ()
modifyIndex f = do
    dbm@DBModel {..} <- get
    put dbm { index = f index}
