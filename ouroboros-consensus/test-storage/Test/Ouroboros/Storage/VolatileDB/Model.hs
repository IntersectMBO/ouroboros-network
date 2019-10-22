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
import           Data.Either
import           Data.List (find, sortOn, splitAt, uncons)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing, mapMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           GHC.Stack.Types

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Block (IsEBB)
import           Ouroboros.Consensus.Util (safeMaximum)

import           Ouroboros.Storage.Common (BlockComponent (..))
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal
import           Ouroboros.Storage.VolatileDB.Util

import           Test.Util.FS.Sim.Error

import           Test.Ouroboros.Storage.VolatileDB.TestBlock (Corruptions,
                     FileCorruption (..), binarySize, headerOffset, headerSize)

type Index blockId = Map FsPath (MaxSlotNo, [(blockId, WithOrigin blockId)])

data DBModel blockId = DBModel {
      blocksPerFile  :: Int
      -- ^ How many blocks each file has (should follow the real Impl).
    , parseError     :: Maybe ParserError
      -- ^ An error indicates a broken db and that parsing will fail.
    , open           :: Bool
      -- ^ Indicates if the db is open.
    , mp             :: Map blockId (SlotNo, IsEBB, ByteString)
      -- ^ Superset of blocks in db. Some of them may be gced already.
    , latestGarbaged :: Maybe SlotNo
      -- ^ Last gced slot.
    , index          :: Index blockId
      -- ^ What each file contains in the Impl.
    , maxSlotNo      :: MaxSlotNo
      -- ^ Highest ever stored SlotNo.
    } deriving Show

initDBModel ::Int -> DBModel blockId
initDBModel bpf = DBModel {
      blocksPerFile  = bpf
    , parseError     = Nothing
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

-- | It throws an error if any file of the index does not parse.
unsafeLastFd :: DBModel blockId -> Maybe FileId
unsafeLastFd DBModel {..} =
    fromRight (error "filename in index didn't parse" ) $
      findLastFd (Map.keys index)

getBlockId :: DBModel blockId -> [(blockId, WithOrigin blockId)]
getBlockId DBModel {..} = concat $ snd <$> Map.elems index

openNewFile :: DBModel blockId -> Index blockId
openNewFile dbm@DBModel{..} = Map.insert (nextFile dbm) newFileInfo index

getDBFiles :: DBModel blockId -> [FsPath]
getDBFiles DBModel {..} = Map.keys index

getDBBlocksWithFiles :: DBModel blockId -> [(FsPath, blockId)]
getDBBlocksWithFiles DBModel {..} =
  concat $ (\(f,(_, bs)) -> map ((f,) . fst) bs) <$> (Map.toList index)

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
            => ThrowCantCatch VolatileDBError m
            -> m ()
reOpenModel err = do
    dbm <- get
    dbm' <- if not $ open dbm
            then recover err dbm
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
  -> (SlotNo, IsEBB, ByteString)
  -> BlockComponent (VolatileDB blockId m) b
  -> b
extractBlockComponent blockId info@(slot, isEBB, block) = \case
    GetBlock      -> ()
    GetRawBlock   -> block
    GetHeader     -> ()
    GetRawHeader  -> extractHeader block
    GetHash       -> blockId
    GetSlot       -> slot
    GetIsEBB      -> isEBB
    GetBlockSize  -> fromIntegral $ BL.length block
    GetHeaderSize -> headerSize
    GetPure a     -> a
    GetApply f bc ->
      extractBlockComponent blockId info f $
      extractBlockComponent blockId info bc
  where
    extractHeader = BL.take (fromIntegral headerSize)
                  . BL.drop (fromIntegral headerOffset)

putBlockModel :: forall blockId m.
                 MonadState (DBModel blockId) m
              => Ord blockId
              => ThrowCantCatch VolatileDBError m
              -> Maybe Errors
              -> BlockInfo blockId
              -> Builder
              -> m ()
putBlockModel err cmdErr BlockInfo{..} bs = do
    -- This depends on the exact sequence of the operations in the real Impl.
    -- If anything changes there, then this will also need change.
    dbm@DBModel {..} <- get
    let currentFile = getCurrentFile dbm
    whenClosedUserError dbm err
    case Map.lookup bbid mp of
      Just _bs -> return ()
      Nothing -> case managesToPut cmdErr of
        Just fsErrT -> EH.throwError' err $
          mkError fsErrT currentFile
        Nothing -> doPut dbm currentFile
  where
    -- | Updates the Model if put succeeds.
    doPut :: DBModel blockId -> FsPath -> m ()
    doPut dbm@DBModel {..} currentFile = do
        put dbm {
            mp = Map.insert bbid (bslot, bisEBB, toLazyByteString bs) mp
          , index = index'
          , maxSlotNo = maxSlotNo `max` MaxSlotNo bslot
          }
        when (1 + length bids == blocksPerFile)
          createFileModel
      where
        (mbid, bids) = unsafeLookupIndex dbm currentFile
        index' = Map.insert currentFile
                            (max mbid (MaxSlotNo bslot), (bbid, bpreBid):bids)
                            index

    -- | Given some simulated errors, it predicts if the 'hPut' will be successful.
    managesToPut :: Maybe Errors -> Maybe FsErrorType
    managesToPut errors = do
      errs <- errors
      (mErr, _rest) <- uncons $ getStream (_hPutSome errs)
      errOrPartial <- mErr
      case errOrPartial of
        Left (fsErr, _mCorr) -> return fsErr
        Right _              -> Nothing

garbageCollectModel :: forall m blockId
                     . MonadState (DBModel blockId) m
                    => ThrowCantCatch VolatileDBError m
                    -> Maybe Errors
                    -> SlotNo
                    -> m ()
garbageCollectModel err cmdErr sl = do
    dbm@DBModel {..} <- get
    whenClosedUserError dbm err
    modify $ \dbm' -> dbm' {latestGarbaged = max latestGarbaged (Just sl)}
    -- This depends only on the exact sequence of the operations which change
    -- the filesystem in the real Impl. If anything changes there, then this
    -- will also need change.
    collectFiles (getCurrentFile dbm) $ sortedFilesOfIndex dbm
  where

    collectFiles :: FsPath -> [(FsPath, MaxSlotNo)] -> m ()
    collectFiles currentFile = foldM_ collectFile initialRemoveErrors
      where
        collectFile :: [Maybe FsErrorType]
                    -> (FsPath, MaxSlotNo)
                    -> m [Maybe FsErrorType]
        collectFile removeErrors (path, msl) =
          if cmpMaybe (maxSlotNoToMaybe msl) sl
          then
            -- Τhis file is not collected. We return the list of @removeFile@
            -- errors unchanged, since no @removeFile@ was called.
            return removeErrors
          else if path == currentFile
          then case truncateError of
            Nothing -> do
              -- If we collect the latest file, we truncate instead of removing.
              -- So we only look at the truncate error and not the remove.
              modifyIndex $ Map.insert path (NoMaxSlotNo,[])
              return removeErrors
            Just e -> EH.throwError' err $ mkError e currentFile
          else case removeErrors of
            [] -> do
              -- We remove the file without any error.
              modifyIndex $ Map.delete path
              return []
            Nothing : rest -> do
              -- Same: successful removing.
              modifyIndex $ Map.delete path
              return rest
            (Just e) : _ -> EH.throwError' err $ mkError e currentFile

        -- | The error of the first truncate, if any. We only keep the first
        -- and not a list, since garbage collect can truncate maximum one file.
        truncateError :: Maybe FsErrorType
        truncateError = do
          cErr <- cmdErr
          (h, _) <- uncons $ getStream . _hTruncate $ cErr
          h

        -- | The list of removeFile errors. @Nothing@ indicates that it succeeds.
        initialRemoveErrors :: [Maybe FsErrorType]
        initialRemoveErrors = case cmdErr of
            Nothing   -> []
            Just cErr -> getStream . _removeFile $ cErr

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

runCorruptionModel :: forall blockId m. MonadState (DBModel blockId) m
                   => Ord blockId
                   => (blockId -> SlotNo)
                   -> Corruptions
                   -> m ()
runCorruptionModel guessSlot corrs = do
    dbm <- get
    let dbm' = foldr corruptDBModel dbm corrs
    put dbm'
  where
    corruptDBModel :: (FileCorruption, FsPath)
                   -> DBModel blockId
                   -> DBModel blockId
    corruptDBModel (corr, file) dbm@DBModel{..} = case corr of
        DeleteFile -> dbm {
              mp = Map.withoutKeys mp (Set.fromList $ fst <$> bids)
            , index = Map.delete file index
            }
          where
            bids = unsafeGetBlocks dbm file
        DropLastBytes n -> dbm {
              mp = mp'
            , index = index'
            , parseError = parseError'
            }
          where
            fitsInBlocks = mod n (fromIntegral binarySize) == 0
            -- This is how many bids we want to drop, not how many will actually
            -- be dropped.
            dropBids = div n (fromIntegral binarySize) +
                       if fitsInBlocks then 0 else 1
            bids = unsafeGetBlocks dbm file
            -- we prepend on list of blockIds, so last bytes
            -- are actually at the head of the list.
            (droppedBids, newBids) = splitAt (fromIntegral dropBids) bids
            newMmax = snd <$> maxSlotList ((\(b,_) -> (b, guessSlot b)) <$> newBids)
            index' = Map.insert file (maxSlotNoFromMaybe newMmax, newBids) index
            mp' = Map.withoutKeys mp (Set.fromList $ fst <$> droppedBids)
            parseError' = if (fromIntegral binarySize)*(length droppedBids) > fromIntegral n
                             && not fitsInBlocks
                             && isNothing parseError
                          then Nothing else parseError
        AppendBytes n ->
            -- Appending doesn't actually change anything, since additional
            -- bytes will be truncated. We have taken care that additional
            -- bytes cannot parse as a block. If something like this happens,
            -- there are not much we can do anyway.
            dbm {parseError = parseError'}
          where
            -- We predict what error the parser will throw. It's easier to do
            -- this here, rather than on reopening. reopening will later actualy
            -- throw the error.
            parseError' = if n > 0 && isNothing parseError
                          then Nothing else parseError

createFileModel :: forall blockId m. MonadState (DBModel blockId) m
                => m ()
createFileModel = do
    dbm <- get
    put dbm {index = openNewFile dbm}

createInvalidFileModel :: forall blockId m. MonadState (DBModel blockId) m
                       => FsPath
                       -> m ()
createInvalidFileModel file = do
    dbm <- get
    put dbm {parseError = Just $ InvalidFilename file}

duplicateBlockModel :: forall blockId m. (
                         MonadState (DBModel blockId) m
                       , Typeable blockId
                       , Eq       blockId
                       , Show     blockId
                       )
                    => (FsPath, blockId)
                    -> m ()
duplicateBlockModel (file, bid) = do
    dbm <- get
    let current = getCurrentFile dbm
    put dbm {parseError = Just $ DuplicatedSlot bid file current}

recover :: MonadState (DBModel blockId) m
        => ThrowCantCatch VolatileDBError m
        -> DBModel blockId
        -> m (DBModel blockId)
recover err dbm@DBModel {..} =
    case parseError of
      Just pError -> EH.throwError' err $ UnexpectedError $ ParserError pError
      Nothing -> return dbm {
          index       = index'
        , parseError  = Nothing
          -- Recalculate it from the index to match the real implementation
        , maxSlotNo   = maxSlotNoFromMaybe
                      $ safeMaximum
                      $ mapMaybe (\(mbS, _) -> maxSlotNoToMaybe mbS)
                      $ Map.elems index'
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

-- | The simulated VolatileDB error, when a fs error happens.
mkError :: FsErrorType -> FsPath -> VolatileDBError
mkError errType file = UnexpectedError . FileSystemError $ FsError {
      fsErrorType   = errType
    , fsErrorPath   = file
    , fsErrorString = ""
    , fsErrorNo     = Nothing
    , fsErrorStack  = EmptyCallStack
    , fsLimitation  = False
    }

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
