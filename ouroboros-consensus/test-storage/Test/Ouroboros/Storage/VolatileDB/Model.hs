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

import           Control.Monad.State (MonadState, get, put)
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.ByteString.Lazy (toStrict)
import           Data.Either
import           Data.List (sortOn, splitAt)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH
import           Ouroboros.Storage.VolatileDB.API
import qualified Ouroboros.Storage.VolatileDB.Impl as Internal
import           Ouroboros.Storage.VolatileDB.Util
import           Test.Ouroboros.Storage.VolatileDB.TestBlock (Corruptions,
                     FileCorruption (..), binarySize)

data DBModel blockId = DBModel {
      blocksPerFile  :: Int
    , open           :: Bool
    , mp             :: Map blockId ByteString
    , latestGarbaged :: Maybe SlotNo
    , index          :: Map String (Maybe Slot, Int, [blockId])
    , currentFile    :: String
    , nextFId        :: FileId
    } deriving (Show)

initDBModel ::Int -> DBModel blockId
initDBModel bpf = DBModel {
      blocksPerFile  = bpf
    , open           = True
    , mp             = Map.empty
    , latestGarbaged = Nothing
    , index          = Map.singleton (Internal.filePath 0) newFileInfo
    , currentFile    = Internal.filePath 0
    , nextFId        = 1
}

openDBModel :: MonadState (DBModel blockId) m
            => (Ord blockId)
            => ErrorHandling (VolatileDBError blockId) m
            -> Int
            -> (blockId -> Slot)
            -> (DBModel blockId, VolatileDB blockId m)
openDBModel err maxNumPerFile toSlot = (dbModel, db)
    where
        dbModel = initDBModel maxNumPerFile
        db =  VolatileDB {
              closeDB        = closeDBModel
            , isOpenDB       = isOpenModel
            , reOpenDB       = reOpenModel
            , getBlock       = getBlockModel err
            , putBlock       = putBlockModel err maxNumPerFile toSlot
            , garbageCollect = garbageCollectModel err
            , getIsMember    = getIsMemberModel err
        }

closeDBModel :: MonadState (DBModel blockId) m => m ()
closeDBModel = do
    dbm <- get
    put $ dbm {open = False}

isOpenModel :: MonadState (DBModel blockId) m => m Bool
isOpenModel = do
    DBModel {..} <- get
    return open

reOpenModel :: MonadState (DBModel blockId) m => m ()
reOpenModel = do
    dbm <- get
    put $ dbm {open = True}

getBlockModel :: forall m blockId. (MonadState (DBModel blockId) m, Ord blockId)
              => ErrorHandling (VolatileDBError blockId) m
              -> blockId
              -> m (Maybe ByteString)
getBlockModel err sl = do
    DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else return $ Map.lookup sl mp

putBlockModel :: MonadState (DBModel blockId) m
              => Ord blockId
              => ErrorHandling (VolatileDBError blockId) m
              -> Int
              -> (blockId -> Slot)
              -> blockId
              -> Builder
              -> m ()
putBlockModel err maxNumPerFile toSlot bid bs = do
    dbm@DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else case Map.lookup bid mp of
        Just _bs -> return ()
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
            put dbm {mp = mp', index = index'', currentFile = currentFile', nextFId = nextFId'}

garbageCollectModel :: MonadState (DBModel blockId) m
                    => ErrorHandling (VolatileDBError blockId) m
                    -> SlotNo
                    -> m ()
garbageCollectModel err sl = do
    dbm@DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else do
        let f :: String -> (Maybe Slot, Int, [blockId]) -> Maybe (Maybe Slot, Int, [blockId])
            f path (msl,n,bids) = if cmpMaybe msl sl then Just (msl,n,bids)
                             else if path == currentFile then Just (Nothing,0,[])
                             else Nothing
        let index' = Map.mapMaybeWithKey f index
        put dbm {index = index', latestGarbaged = Just $ maxMaybe latestGarbaged sl}

runCorruptionModel :: forall blockId m. MonadState (DBModel blockId) m
                   => Ord blockId
                   => (blockId -> Slot)
                   -> Corruptions
                   -> m ()
runCorruptionModel toSlot corrs = do
    dbm <- get
    let dbm' = foldr corrupt dbm corrs
    put $ recover dbm'
        where
            corrupt :: (FileCorruption, String) -> DBModel blockId -> DBModel blockId
            corrupt (corr, file) dbm = case corr of
                DeleteFile ->
                    dbm { mp = mp', index = index'}
                      where
                        (_, _, bids) = fromMaybe
                            (error "tried to corrupt a file which does not exist")
                            (Map.lookup file (index dbm))
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList bids)
                        index' = Map.delete file (index dbm)
                DropLastBytes n ->
                    -- this is how many bids we want to drop, not how many will actually be dropped.
                    let dropBids = (div n (fromIntegral binarySize)) +
                                   (if mod n (fromIntegral binarySize) == 0 then 0 else 1 )
                        (_mmax, size, bids) = fromMaybe
                            (error $ "tried to corrupt file " <> file <>  " which does not exist")
                            (Map.lookup file (index dbm))
                        -- we prepend on list of blockIds, so last bytes
                        -- are actually at the head of the list.
                        (droppedBids, newBids) = splitAt (fromIntegral dropBids) bids
                        newMmax = snd <$> maxSlotList toSlot newBids
                        index' = Map.insert file (newMmax, size - fromIntegral (length droppedBids), newBids) (index dbm)
                        mp' = Map.withoutKeys (mp dbm) (Set.fromList droppedBids)
                    in dbm {mp = mp', index = index'}
                AppendBytes _ -> dbm
                    -- Appending doesn't actually change anything, since additional bytes will be truncated.

recover :: DBModel blockId -> DBModel blockId
recover dbm@DBModel {..} = dbm{index = index', currentFile = cFile, nextFId = fid}
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

unsafeParseFd :: String -> FileId
unsafeParseFd file = fromMaybe
    (error $ "could not parse filename " <> file <> " of index")
    (parseFd file)

newFileInfo :: (Maybe a, Int, [b])
newFileInfo = (Nothing, 0, [])

getIsMemberModel :: MonadState (DBModel blockId) m
                 => Ord blockId
                 => ErrorHandling (VolatileDBError blockId) m
                 -> m (blockId -> Bool)
getIsMemberModel err = do
    DBModel {..} <- get
    if not open then EH.throwError err ClosedDBError
    else return (\bid -> Map.member bid mp)

maxMaybe :: Ord slot => Maybe slot -> slot -> slot
maxMaybe Nothing sl    = sl
maxMaybe (Just sl') sl = max sl' sl
