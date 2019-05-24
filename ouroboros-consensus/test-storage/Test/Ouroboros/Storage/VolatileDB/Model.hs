{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-- | In-memory Model implementation of 'VolatileDB' for testing
module Test.Ouroboros.Storage.VolatileDB.Model
    (
      DBModel (..)
    , closeModel
    , createFileModel
    , createInvalidFileModel
    , duplicateBlockModel
    , garbageCollectModel
    , getBlockIdsModel
    , getBlockModel
    , getIsMemberModel
    , getSuccessorsModel
    , getPredecessorModel
    , initDBModel
    , isOpenModel
    , putBlockModel
    , reOpenModel
    , runCorruptionModel
    ) where

import           Control.Monad
import           Control.Monad.State (MonadState, get, modify, put)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (toStrict)
import           Data.Either
import           Data.List (find, sortOn, splitAt, uncons)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Stack.Types

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ThrowCantCatch)
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal
import           Ouroboros.Storage.VolatileDB.Util
import           Test.Ouroboros.Storage.FS.Sim.Error
import           Test.Ouroboros.Storage.VolatileDB.TestBlock (Corruptions,
                     FileCorruption (..), binarySize)

data DBModel blockId = DBModel {
      blocksPerFile  :: Int  -- how many blocks each file has (should follow the real Impl)
    , parseError     :: Maybe (ParserError blockId) -- an error which indicates the parser will return an error.
    , open           :: Bool -- is the db open.
    , mp             :: Map blockId ByteString -- superset of blocks in db. Some of them may be gced already.
    , latestGarbaged :: Maybe SlotNo -- last gced slot.
    , index          :: Map String (Maybe SlotNo, Int, [(blockId, Maybe blockId)]) -- what each file contains in the real impl.
    , currentFile    :: String -- the current open file. If the db is empty this is the next it wil write.
    , nextFId        :: FileId -- the next file id.
    } deriving Show

initDBModel ::Int -> DBModel blockId
initDBModel bpf = DBModel {
      blocksPerFile  = bpf
    , parseError     = Nothing
    , open           = True
    , mp             = Map.empty
    , latestGarbaged = Nothing
    , index          = Map.singleton (Internal.filePath 0) newFileInfo
    , currentFile    = Internal.filePath 0
    , nextFId        = 1
}

closeModel :: MonadState (DBModel blockId) m => m ()
closeModel = do
    dbm <- get
    put $ dbm {open = False}

isOpenModel :: MonadState (DBModel blockId) m => m Bool
isOpenModel = do
    DBModel {..} <- get
    return open

reOpenModel :: MonadState (DBModel blockId) m
            => ThrowCantCatch (VolatileDBError blockId) m
            -> m ()
reOpenModel err = do
    dbm <- get
    dbm' <- if not $ open dbm
            then recover err dbm
            else return dbm
    put dbm' {open = True}

getBlockModel :: forall m blockId. (MonadState (DBModel blockId) m, Ord blockId)
              => ThrowCantCatch (VolatileDBError blockId) m
              -> blockId
              -> m (Maybe ByteString)
getBlockModel err sl = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else return $ Map.lookup sl mp

putBlockModel :: MonadState (DBModel blockId) m
              => Ord blockId
              => ThrowCantCatch (VolatileDBError blockId) m
              -> Maybe Errors
              -> BlockInfo blockId
              -> Builder
              -> m ()
putBlockModel err cmdErr BlockInfo{..} bs = do
    -- This depends on the exact sequence of the operations in the real Impl.
    -- If anything changes there, then this wil also need change.
    let managesToPut errors = do
            errs <- errors
            (mErr, _rest) <- uncons $ getStream (_hPutSome errs)
            (fsErr, _mCorr) <- mErr
            return fsErr
    dbm@DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else case Map.lookup bbid mp of
        Just _bs -> return ()
        Nothing -> case managesToPut cmdErr of
            Just fsErrT -> EH.throwError' err $ UnexpectedError . FileSystemError $
                FsError {
                      fsErrorType = fsErrT
                    , fsErrorPath = [currentFile]
                    , fsErrorString = ""
                    , fsErrorStack = EmptyCallStack
                    , fsLimitation = False
                }
            Nothing -> do
                let mp' = Map.insert bbid (toStrict $ toLazyByteString bs) mp
                    (mbid, n, bids) = fromMaybe
                        (error "current file does not exist in index")
                        (Map.lookup currentFile index)
                    n' = n + 1
                    index' = Map.insert currentFile (updateSlotNoBlockId mbid [bslot], n', (bbid, bpreBid):bids) index
                    (currentFile', index'', nextFId') =
                        if n' == blocksPerFile
                        then ( Internal.filePath nextFId
                            , Map.insertWith
                                (\ _ _ -> (error $ "new file " <> currentFile' <> "already in index"))
                                currentFile' newFileInfo index'
                            , nextFId + 1)
                        else ( currentFile
                            , index'
                            , nextFId)
                put dbm {
                      mp = mp'
                    , index = index''
                    , currentFile = currentFile'
                    , nextFId = nextFId'
                    }

garbageCollectModel :: forall m blockId
                     . MonadState (DBModel blockId) m
                    => ThrowCantCatch (VolatileDBError blockId) m
                    -> Maybe Errors
                    -> SlotNo
                    -> m ()
garbageCollectModel err cmdErr sl = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else do
        modify $ \dbm' -> dbm' {latestGarbaged = Just $ maxMaybe latestGarbaged sl}
        let tru :: Maybe FsErrorType
            tru = do
                cErr <- cmdErr
                let str = getStream . _hTruncate $ cErr
                (h, _) <- uncons str
                h
        let remLs = case cmdErr of
                Nothing   -> []
                Just cErr -> getStream . _removeFile $ cErr
        let f :: [Maybe FsErrorType]
              -> (String, (Maybe SlotNo, Int, [(blockId, Maybe blockId)]))
              -> m [Maybe FsErrorType]
            f fsErr (path, (msl,_n,_bids)) = case (cmpMaybe msl sl, path == currentFile, tru, fsErr) of
                    (True, _, _, _) -> return fsErr
                    (_, False, _, []) -> do
                        modifyIndex $ Map.delete path
                        return []
                    (_, False, _, Nothing : rest) -> do
                        modifyIndex $ Map.delete path
                        return rest
                    (_, False, _, (Just e) : _rest) -> EH.throwError' err $ UnexpectedError . FileSystemError $
                        FsError {
                              fsErrorType = e
                            , fsErrorPath = [currentFile]
                            , fsErrorString = ""
                            , fsErrorStack = EmptyCallStack
                            , fsLimitation = False
                        }
                    (_, _, Nothing, _) -> do
                        modifyIndex $ Map.insert path (Nothing,0,[])
                        return fsErr
                    (_, _, Just e, _)  -> EH.throwError' err $ UnexpectedError . FileSystemError $
                        FsError {
                              fsErrorType = e
                            , fsErrorPath = [currentFile]
                            , fsErrorString = ""
                            , fsErrorStack = EmptyCallStack
                            , fsLimitation = False
                        }
        -- This depends on the exact sequence of the operations in the real Impl.
        -- If anything changes there, then this will also need change.
        _ <- foldM f remLs (sortOn (unsafeParseFd . fst) $ Map.toList index)
        return ()

getBlockIdsModel :: forall m blockId
                 . MonadState (DBModel blockId) m
                 => ThrowCantCatch (VolatileDBError blockId) m
                 -> m [blockId]
getBlockIdsModel err = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else return $ concat $ (\(_,(_, _, bs)) -> fst <$> bs) <$> (Map.toList $ index)

getSuccessorsModel :: forall m blockId
                   . MonadState (DBModel blockId) m
                   => Ord blockId
                   => ThrowCantCatch (VolatileDBError blockId) m
                   -> m (Maybe blockId -> Set blockId)
getSuccessorsModel err = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else return $ \bid ->
        toSet $ fst <$> filter (\(_b,pb) -> pb == bid) (concat $ (\(_,_,c) -> c) <$> Map.elems index)
  where
    toSet [] = error "precondition violated: block not member of the VolatileDB"
    toSet xs = Set.fromList xs

getPredecessorModel :: forall m blockId
                    . MonadState (DBModel blockId) m
                    => Ord blockId
                    => ThrowCantCatch (VolatileDBError blockId) m
                    -> m (blockId -> Maybe blockId)
getPredecessorModel err = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else return $ \bid ->
        maybe (error msg) snd $ find (\(b,_pb) -> b == bid) (concat $ (\(_,_,c) -> c) <$> Map.elems index)
  where
    msg = "precondition violated: block not member of the VolatileDB"

modifyIndex :: MonadState (DBModel blockId) m
            => (Map String (Maybe SlotNo, Int, [(blockId, Maybe blockId)])
                  -> Map String (Maybe SlotNo, Int, [(blockId, Maybe  blockId)])
               )
            -> m ()
modifyIndex f = do
    dbm@DBModel {..} <- get
    put dbm {index = f index}

runCorruptionModel :: forall blockId m. MonadState (DBModel blockId) m
                   => Ord blockId
                   => (blockId -> SlotNo)
                   -> Corruptions
                   -> m ()
runCorruptionModel guessSlot corrs = do
    dbm <- get
    -- TODO(kde) need to sort corrs if we want to improve Eq instance of
    -- Error Types.
    let dbm' = foldr corrupt' dbm corrs
    put dbm'
        where
            corrupt' :: (FileCorruption, String) -> DBModel blockId -> DBModel blockId
            corrupt' (corr, file) dbm = case corr of
                DeleteFile ->
                    dbm { mp = mp'
                        , index = index'
                    }
                      where
                        (_, _, bids) = fromMaybe
                            (error "tried to corrupt a file which does not exist")
                            (Map.lookup file (index dbm))
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList $ fst <$> bids)
                        index' = Map.delete file (index dbm)
                DropLastBytes n ->
                    dbm { mp = mp'
                        , index = index'
                    -- We predict what error the parser will throw. It's easier to do this
                    -- here, rather than on reOpening. reOpening will later actualy throw the error.
                        , parseError = parseError'
                    }
                      where
                    -- this is how many bids we want to drop, not how many will actually be dropped.
                        dropBids = (div n (fromIntegral binarySize)) +
                                   (if mod n (fromIntegral binarySize) == 0 then 0 else 1 )
                        (_mmax, size, bids) = fromMaybe
                            (error $ "tried to corrupt file " <> file <>  " which does not exist")
                            (Map.lookup file (index dbm))
                        -- we prepend on list of blockIds, so last bytes
                        -- are actually at the head of the list.
                        (droppedBids, newBids) = splitAt (fromIntegral dropBids) bids
                        newMmax = snd <$> maxSlotList ((\(b,_) -> (b, guessSlot b)) <$> newBids)
                        size' = size - fromIntegral (length droppedBids)
                        index' = Map.insert file (newMmax, size', newBids) (index dbm)
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList $ fst <$> droppedBids)
                        parseError' = if (fromIntegral binarySize)*(length droppedBids) > fromIntegral n
                                         && not (mod n (fromIntegral binarySize) == 0)
                                         && isNothing (parseError dbm)
                                      then Nothing else parseError dbm
                AppendBytes n ->
                    dbm {parseError = parseError'}
                      where
                        parseError' = if n > 0 && isNothing (parseError dbm)
                                      then Nothing else parseError dbm
                    -- Appending doesn't actually change anything, since additional bytes will be truncated.

createFileModel :: forall blockId m. MonadState (DBModel blockId) m
                => m ()
createFileModel = do
    dbm <- get
    let currentFile' = filePath $ nextFId dbm
    let index' = Map.insert currentFile' newFileInfo (index dbm)
    put dbm { index = index'
              , nextFId = (nextFId dbm) + 1
              , currentFile = currentFile'}

createInvalidFileModel :: forall blockId m. MonadState (DBModel blockId) m
                       => String
                       -> m ()
createInvalidFileModel file = do
    db <- get
    put $ db {parseError = Just $ InvalidFilename file}

duplicateBlockModel :: forall blockId m. MonadState (DBModel blockId) m
                    => (String, blockId)
                    -> m ()
duplicateBlockModel (file, bid) = do
    db <- get
    let current = currentFile db
    put $ db {parseError = Just $ DuplicatedSlot bid file current}

recover :: MonadState (DBModel blockId) m
        => ThrowCantCatch (VolatileDBError blockId) m
        -> DBModel blockId
        -> m (DBModel blockId)
recover err dbm@DBModel {..} = do
    case parseError of
        Just pError@(InvalidFilename _) -> EH.throwError' err $ UnexpectedError $ ParserError pError
        Just pError@(DuplicatedSlot _ _ _) -> EH.throwError' err $ UnexpectedError $ ParserError pError
        _ -> return $ dbm {index = index', currentFile = cFile, nextFId = fid, parseError = Nothing}
  where
    lastFd = fromRight (error "filename in index didn't parse" )
                       (findLastFd $ Set.fromList $ Map.keys index)
    ls = Map.toList index
    lessThan = filter (\(_, (_, nBlocks, _)) -> nBlocks < blocksPerFile) ls
    sorted = sortOn (unsafeParseFd . fst) lessThan
    (cFile, fid, index') = case (sorted, lastFd) of
        ([], Nothing) -> (Internal.filePath 0, 1, Map.fromList [(Internal.filePath 0, newFileInfo)])
        (_, Nothing) -> error "invariant violated"
        ([], Just lst) -> let fd' = lst + 1 in
            (Internal.filePath fd', fd' + 1, Map.insert (Internal.filePath fd') newFileInfo index)
        (_, Just lst) ->
            let (file, (_msl, _n, _bids)) = last sorted
            in if unsafeParseFd file == lst then (file, lst + 1, index)
               else (Internal.filePath $ lst + 1, lst + 2, Map.insert (Internal.filePath $ lst + 1) newFileInfo index)

newFileInfo :: (Maybe a, Int, [b])
newFileInfo = (Nothing, 0, [])

getIsMemberModel :: MonadState (DBModel blockId) m
                 => Ord blockId
                 => ThrowCantCatch (VolatileDBError blockId) m
                 -> m (blockId -> Bool)
getIsMemberModel err = do
    DBModel {..} <- get
    if not open then EH.throwError' err $ UserError ClosedDBError
    else return (\bid -> Map.member bid mp)

maxMaybe :: Ord slot => Maybe slot -> slot -> slot
maxMaybe Nothing sl    = sl
maxMaybe (Just sl') sl = max sl' sl
