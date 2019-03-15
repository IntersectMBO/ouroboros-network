{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | In-memory Model implementation of 'VolatileDB' for testing
module Test.Ouroboros.Storage.VolatileDB.Model
    (
      DBModel (..)
    , initDBModel
    , openDBModel
    , runCorruptionModel
    ) where

import           Control.Monad
import           Control.Monad.State (MonadState, get, modify, put)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (toStrict)
import           Data.Either
import           Data.List (sortOn, splitAt, uncons)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import           GHC.Stack.Types

import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal
import           Ouroboros.Storage.VolatileDB.Util
import           Test.Ouroboros.Storage.FS.Sim.Error
import           Test.Ouroboros.Storage.VolatileDB.TestBlock (Corruptions,
                     FileCorruption (..), binarySize)

data DBModel blockId = DBModel {
      blocksPerFile  :: Int
    , lenientParse   :: Bool
    , parseError     :: Maybe (ParserError blockId)
    , open           :: Bool
    , mp             :: Map blockId ByteString
    , latestGarbaged :: Maybe SlotNo
    , index          :: Map String (Maybe Slot, Int, [blockId])
    , currentFile    :: String
    , nextFId        :: FileId
    } deriving (Show)

initDBModel ::Int -> Bool -> DBModel blockId
initDBModel bpf lp = DBModel {
      blocksPerFile  = bpf
    , lenientParse   = lp
    , parseError     = Nothing
    , open           = True
    , mp             = Map.empty
    , latestGarbaged = Nothing
    , index          = Map.singleton (Internal.filePath 0) newFileInfo
    , currentFile    = Internal.filePath 0
    , nextFId        = 1
}

type MyState blockId = (DBModel blockId, Maybe Errors)

getDB :: MonadState (MyState blockId) m => m (DBModel blockId)
getDB = do
    (db, _) <- get
    return db

putDB :: MonadState (MyState blockId) m => DBModel blockId -> m ()
putDB db = do
    (_, cmdErr) <- get
    put (db, cmdErr)

openDBModel :: MonadState (MyState blockId) m
            => (Ord blockId)
            => ErrorHandling (VolatileDBError blockId) m
            -> Int
            -> (blockId -> Slot)
            -> Bool
            -> (DBModel blockId, VolatileDB blockId m)
openDBModel err maxNumPerFile toSlot lenientParsing = (dbModel, db)
    where
        dbModel = initDBModel maxNumPerFile lenientParsing
        db =  VolatileDB {
              closeDB        = closeDBModel
            , isOpenDB       = isOpenModel
            , reOpenDB       = reOpenModel err
            , getBlock       = getBlockModel err
            , putBlock       = putBlockModel err maxNumPerFile toSlot
            , garbageCollect = garbageCollectModel err
            , getIsMember    = getIsMemberModel err
        }

closeDBModel :: MonadState (MyState blockId) m => m ()
closeDBModel = do
    dbm <- getDB
    putDB $ dbm {open = False}

isOpenModel :: MonadState (MyState blockId) m => m Bool
isOpenModel = do
    DBModel {..} <- getDB
    return open

reOpenModel :: MonadState (MyState blockId) m
            => ErrorHandling (VolatileDBError blockId) m
            -> m ()
reOpenModel err = do
    dbm <- getDB
    dbm' <- recover err dbm
    putDB dbm' {open = True}

getBlockModel :: forall m blockId. (MonadState (MyState blockId) m, Ord blockId)
              => ErrorHandling (VolatileDBError blockId) m
              -> blockId
              -> m (Maybe ByteString)
getBlockModel err sl = do
    DBModel {..} <- getDB
    if not open then EH.throwError err ClosedDBError
    else return $ Map.lookup sl mp

putBlockModel :: MonadState (MyState blockId) m
              => Ord blockId
              => ErrorHandling (VolatileDBError blockId) m
              -> Int
              -> (blockId -> Slot)
              -> blockId
              -> Builder
              -> m ()
putBlockModel err maxNumPerFile toSlot bid bs = do
    let managesToPut errors = do
            errs <- errors
            (mErr, _rest) <- uncons $ getStream (_hPut errs)
            (fsErr, _mCorr) <- mErr
            return fsErr
    (dbm@DBModel {..}, cmdErr) <- get
    if not open then EH.throwError err ClosedDBError
    else case Map.lookup bid mp of
        Just _bs -> return ()
        Nothing -> case managesToPut cmdErr of
            Just fsErrT -> EH.throwError err $ FileSystemError $
                FsError {
                      fsErrorType = fsErrT
                    , fsErrorPath = [currentFile]
                    , fsErrorString = ""
                    , fsErrorStack = EmptyCallStack
                    , fsLimitation = False
                }
            Nothing -> do
                let mp' = Map.insert bid (toStrict $ toLazyByteString bs) mp
                    (mbid, n, bids) = fromMaybe
                        (error "current file does not exist in index")
                        (Map.lookup currentFile index)
                    n' = n + 1
                    index' = Map.insert currentFile (updateSlotNoBlockId mbid [toSlot bid], n', bid:bids) index
                    (currentFile', index'', nextFId') =
                        if n' == maxNumPerFile
                        then ( Internal.filePath nextFId
                            , Map.insertWith
                                (\ _ _ -> (error $ "new file " <> currentFile' <> "already in index"))
                                currentFile' newFileInfo index'
                            , nextFId + 1)
                        else ( currentFile
                            , index'
                            , nextFId)
                putDB dbm {mp = mp', index = index'', currentFile = currentFile', nextFId = nextFId'}

garbageCollectModel :: forall m blockId
                     . MonadState (MyState blockId) m
                    => ErrorHandling (VolatileDBError blockId) m
                    -> SlotNo
                    -> m ()
garbageCollectModel err sl = do
    (DBModel {..}, cmdErr) <- get
    if not open then EH.throwError err ClosedDBError
    else do
        modify $ \(dbm', cErr) -> (dbm' {latestGarbaged = Just $ maxMaybe latestGarbaged sl}, cErr)
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
              -> (String, (Maybe Slot, Int, [blockId]))
              -> m [Maybe FsErrorType]
            f fsErr (path, (msl,_n,_bids)) = case (cmpMaybe msl sl, path == currentFile, tru, fsErr) of
                    (True, _, _, _) -> return fsErr
                    (_, False, _, []) -> do
                        modifyIndex $ Map.delete path
                        return []
                    (_, False, _, Nothing : rest) -> do
                        modifyIndex $ Map.delete path
                        return rest
                    (_, False, _, (Just e) : _rest) -> EH.throwError err $ FileSystemError $
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
                    (_, _, Just e, _)  -> EH.throwError err $ FileSystemError $
                        FsError {
                              fsErrorType = e
                            , fsErrorPath = [currentFile]
                            , fsErrorString = ""
                            , fsErrorStack = EmptyCallStack
                            , fsLimitation = False
                        }
        _ <- foldM f remLs (sortOn (unsafeParseFd . fst) $ Map.toList index)
        return ()

modifyIndex :: MonadState (MyState blockId) m
            => (Map String (Maybe Slot, Int, [blockId]) -> Map String (Maybe Slot, Int, [blockId]))
            -> m ()
modifyIndex f = do
    dbm@DBModel {..} <- getDB
    putDB dbm {index = f index}

runCorruptionModel :: forall blockId m. MonadState (MyState blockId) m
                   => Ord blockId
                   => (blockId -> Slot)
                   -> Corruptions
                   -> m ()
runCorruptionModel toSlot corrs = do
    dbm <- getDB
    -- TODO(kde) need to sort corrs if we want to improve Eq instance of
    -- Error Types.
    let dbm' = foldr corrupt' dbm corrs
    putDB dbm'
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
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList bids)
                        index' = Map.delete file (index dbm)
                DropLastBytes n ->
                    dbm { mp = mp'
                        , index = index'
                    -- We predict what error the parser will throw. It's easier to do this
                    -- here, rather than on reOpening. reOpening will decide if this error
                    -- will be rethrown to the user or will remain internal, based on whether
                    -- the parser is lenient or not.
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
                        newMmax = snd <$> maxSlotList toSlot newBids
                        size' = size - fromIntegral (length droppedBids)
                        index' = Map.insert file (newMmax, size', newBids) (index dbm)
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList droppedBids)
                        parseError' = if (fromIntegral binarySize)*(length droppedBids) > fromIntegral n
                                         && not (mod n (fromIntegral binarySize) == 0)
                                         && isNothing (parseError dbm)
                                      -- TODO(kde) need to improve error message if we want to compare
                                      -- with the real one.
                                      then Just (DecodeFailed (0, Map.empty) "" 0) else parseError dbm
                AppendBytes n ->
                    dbm {parseError = parseError'}
                      where
                        parseError' = if n > 0 && isNothing (parseError dbm)
                                      then Just (DecodeFailed (0, Map.empty) "" 0) else parseError dbm
                    -- Appending doesn't actually change anything, since additional bytes will be truncated.

recover :: Monad m
        => ErrorHandling (VolatileDBError blockId) m
        -> DBModel blockId
        -> m (DBModel blockId)
recover err dbm@DBModel {..} =
    if not lenientParse && isJust parseError
    then EH.throwError err $ VParserError $ fromJust parseError
    else return $ dbm {index = index', currentFile = cFile, nextFId = fid, parseError = Nothing}
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
               else (Internal.filePath $ lst + 2, lst + 2, Map.insert (Internal.filePath $ lst + 1) newFileInfo index)

newFileInfo :: (Maybe a, Int, [b])
newFileInfo = (Nothing, 0, [])

getIsMemberModel :: MonadState (MyState blockId) m
                 => Ord blockId
                 => ErrorHandling (VolatileDBError blockId) m
                 -> m (blockId -> Bool)
getIsMemberModel err = do
    DBModel {..} <- getDB
    if not open then EH.throwError err ClosedDBError
    else return (\bid -> Map.member bid mp)

maxMaybe :: Ord slot => Maybe slot -> slot -> slot
maxMaybe Nothing sl    = sl
maxMaybe (Just sl') sl = max sl' sl
