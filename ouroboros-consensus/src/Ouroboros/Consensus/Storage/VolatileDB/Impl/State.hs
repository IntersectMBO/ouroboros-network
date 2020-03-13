{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Ouroboros.Consensus.Storage.VolatileDB.Impl.State
  ( VolatileDBEnv (..)
  , OpenOrClosed (..)
  , volatileDbIsOpen
  , InternalState (..)
  , modifyState
  , mkInternalStateDB
  ) where

import           Control.Monad
import           Control.Tracer (Tracer, traceWith)
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Util (whenJust)
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB.API
import qualified Ouroboros.Consensus.Storage.VolatileDB.FileInfo as FileInfo
import           Ouroboros.Consensus.Storage.VolatileDB.Index (Index)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Util

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h e. VolatileDBEnv {
      hasFS            :: !(HasFS m h)
    , varInternalState :: !(StrictMVar m (OpenOrClosed blockId h))
    , maxBlocksPerFile :: !BlocksPerFile
    , parser           :: !(Parser e m blockId)
    , tracer           :: !(Tracer m (TraceEvent e blockId))
    }

data OpenOrClosed blockId h =
    VolatileDbOpen !(InternalState blockId h)
  | VolatileDbClosed
  deriving (Generic, NoUnexpectedThunks)

volatileDbIsOpen :: OpenOrClosed blockId h -> Bool
volatileDbIsOpen (VolatileDbOpen _) = True
volatileDbIsOpen VolatileDbClosed   = False

data InternalState blockId h = InternalState {
      currentWriteHandle :: !(Handle h)
      -- ^ The only open file we append blocks to.
    , currentWritePath   :: !FsPath
      -- ^ The path of the file above.
    , currentWriteId     :: !FileId
      -- ^ The 'FileId' of the same file.
    , currentWriteOffset :: !Word64
      -- ^ The offset of the same file.
    , currentMap         :: !(Index blockId)
      -- ^ The contents of each file.
    , currentRevMap      :: !(ReverseIndex blockId)
      -- ^ Where to find each block based on its slot number.
    , currentSuccMap     :: !(SuccessorsIndex blockId)
      -- ^ The successors for each block.
    , currentMaxSlotNo   :: !MaxSlotNo
      -- ^ Highest stored SlotNo.
      --
      -- INVARIANT: this is the cached value of:
      -- > FileInfo.maxSlotInFiles (Index.elems (currentMap st))
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyState :: forall blockId m r. (HasCallStack, IOLike m)
            => VolatileDBEnv m blockId
            -> (forall h
               .  HasFS m h
               -> InternalState blockId h
               -> m (InternalState blockId h, r)
               )
            -> m r
modifyState VolatileDBEnv {hasFS = hasFS :: HasFS m h, varInternalState} action = do
    (mr, ()) <- generalBracket open close (tryVolDB . mutation)
    case mr of
      Left  e      -> throwM e
      Right (_, r) -> return r
  where
    open :: m (OpenOrClosed blockId h)
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    open = uninterruptibleMask_ $ takeMVar varInternalState

    close
      :: OpenOrClosed blockId h
      -> ExitCase (Either VolatileDBError (InternalState blockId h, r))
      -> m ()
    close mst ec = do
        -- It is crucial to replace the TMVar.
        putMVar varInternalState mst'
        followUp
      where
        (mst', followUp) = case ec of
          -- If we were interrupted, restore the original state.
          ExitCaseAbort                         -> (mst, return ())
          ExitCaseException _ex                 -> (mst, return ())
          -- In case of success, update to the newest state.
          ExitCaseSuccess (Right (newState, _)) ->
            (VolatileDbOpen newState, return ())
          -- In case of an error (not an exception), close the DB for safety.
          ExitCaseSuccess (Left _)              ->
            (VolatileDbClosed, closeOpenHandle mst)

    mutation :: OpenOrClosed blockId h
             -> m (InternalState blockId h, r)
    mutation VolatileDbClosed          = throwM $ UserError ClosedDBError
    mutation (VolatileDbOpen oldState) = action hasFS oldState

    -- TODO what if this fails?
    closeOpenHandle :: OpenOrClosed blockId h -> m ()
    closeOpenHandle VolatileDbClosed = return ()
    closeOpenHandle (VolatileDbOpen InternalState { currentWriteHandle }) =
      wrapFsError $ hClose hasFS currentWriteHandle

mkInternalStateDB :: forall m blockId e h.
                     ( HasCallStack
                     , MonadThrow m
                     , MonadCatch m
                     , Ord blockId
                     )
                  => HasFS m h
                  -> Parser e m blockId
                  -> Tracer m (TraceEvent e blockId)
                  -> BlocksPerFile
                  -> m (InternalState blockId h)
mkInternalStateDB hasFS@HasFS{..} parser tracer maxBlocksPerFile =
    wrapFsError $ do
      createDirectoryIfMissing True dbDir
      allFiles <- map toFsPath . Set.toList <$> listDirectory dbDir
      filesWithIds <- logInvalidFiles $ parseAllFds allFiles
      mkInternalState hasFS parser tracer maxBlocksPerFile filesWithIds
  where
    -- | Logs about any invalid 'FsPath' and returns the valid ones.
    logInvalidFiles :: ([(FileId, FsPath)], [FsPath]) -> m [(FileId, FsPath)]
    logInvalidFiles (valid, invalid) = do
      unless (null invalid) $
        traceWith tracer $ InvalidFileNames invalid
      return valid

    dbDir = mkFsPath []

    toFsPath :: String -> FsPath
    toFsPath file = mkFsPath [file]

-- | Short-hand for all three index types
type Indices blockId =
  ( Index           blockId
  , ReverseIndex    blockId
  , SuccessorsIndex blockId
  )

-- | Makes the 'InternalState' by parsing all files.
--
-- It may create a new file to append new blocks to or use an existing one.
mkInternalState
  :: forall blockId m h e. (
       HasCallStack
     , MonadCatch m
     , Ord blockId
     )
  => HasFS m h
  -> Parser e m blockId
  -> Tracer m (TraceEvent e blockId)
  -> BlocksPerFile
  -> [(FileId, FsPath)]
  -> m (InternalState blockId h)
mkInternalState hasFS parser tracer maxBlocksPerFile files =
    wrapFsError $ do
      (currentMap', currentRevMap', currentSuccMap') <-
        foldM validateFile (Index.empty, Map.empty, Map.empty) files

      let (currentWriteId, currentMap'') = case Index.lastFile currentMap' of
            -- The DB is empty. Create a new file with 'FileId' 0
            Nothing
              -> (0, Index.insert 0 FileInfo.empty currentMap')
            Just (lastWriteId, lastFileInfo)
              | FileInfo.isFull maxBlocksPerFile lastFileInfo
              , let nextWriteId = lastWriteId + 1
                -- If the last file is full, we need to create a new one
              -> (nextWriteId, Index.insert nextWriteId FileInfo.empty currentMap')
              | otherwise
                -- If the last file is not full, then use that one
              -> (lastWriteId, currentMap')

      let currentWritePath = filePath currentWriteId

      currentWriteHandle <- hOpen hasFS currentWritePath (AppendMode AllowExisting)
      -- If 'hGetSize' fails, we should close the opened handle that didn't
      -- make it into the state, otherwise we'd leak it.
      currentWriteOffset <- onException
        (hGetSize hasFS currentWriteHandle)
        (hClose   hasFS currentWriteHandle)

      return InternalState {
          currentWriteHandle = currentWriteHandle
        , currentWritePath   = currentWritePath
        , currentWriteId     = currentWriteId
        , currentWriteOffset = currentWriteOffset
        , currentMap         = currentMap''
        , currentRevMap      = currentRevMap'
        , currentSuccMap     = currentSuccMap'
        , currentMaxSlotNo   = FileInfo.maxSlotInFiles
                                  (Index.elems currentMap')
        }
  where
    validateFile :: Indices blockId -> (FileId, FsPath) -> m (Indices blockId)
    validateFile (currentMap, currentRevMap, currentSuccMap) (fd, file) = do
      (parsedBlocks, mErr) <- parse parser file
      whenJust mErr $ \(e, offset) ->
        truncateError file e offset

      let (currentRevMap', acceptedBlocks, mErr') =
            addToReverseIndex file currentRevMap parsedBlocks
      -- We can find duplicate blocks when merging the parsed blocks with the
      -- 'ReverseIndex', so we might have to truncate at this point too.
      whenJust mErr' $ \(e, offset) ->
        truncateError file e offset

      let fileInfo        = FileInfo.fromParsedInfo acceptedBlocks
          currentMap'     = Index.insert fd fileInfo currentMap
          currentSuccMap' = foldl'
            (\succMap (_, (_, blockInfo)) ->
              insertMapSet succMap (bbid blockInfo, bpreBid blockInfo))
            currentSuccMap
            acceptedBlocks

      return (currentMap', currentRevMap', currentSuccMap')

    truncateError
      :: FsPath
      -> ParserError blockId e
      -> BlockOffset
      -> m ()
    truncateError file e offset = do
      traceWith tracer $ Truncate e file offset
      -- The handle of the parser is closed at this point. We need
      -- to reopen the file in 'AppendMode' now (parser opens with
      -- 'ReadMode').
      --
      -- Note that no file is open at this point, so we can safely
      -- open with 'AppendMode' any file, without the fear of opening
      -- multiple concurrent writers, which is not allowed, or concurrent
      -- read with truncate.
      withFile hasFS file (AppendMode AllowExisting) $ \hndl ->
        hTruncate hasFS hndl offset


-- | For each block found in a parsed file, we insert its 'InternalBlockInfo'
-- in the 'ReverseIndex'.
--
-- If a block is already present in the 'ReverseIndex' or occurs twice in the
-- same file, we stop with an error.
--
-- We return:
--
-- * A 'ReverseIndex' updated with the valid blocks
-- * A list of the valid blocks in the parsed file. This will be a prefix of
--   the given list, or most often, the original input list.
-- * In case of an error, the error and the offset to truncate to.
addToReverseIndex
  :: forall blockId e. Ord blockId
  => FsPath
  -> ReverseIndex blockId
  -> ParsedInfo blockId
  -> ( ReverseIndex blockId
     , ParsedInfo blockId
     , Maybe (ParserError blockId e, BlockOffset)
     )
addToReverseIndex file = \revMap -> go revMap []
  where
    go :: ReverseIndex blockId
       -> ParsedInfo blockId -- accumulator of the accepted blocks.
       -> ParsedInfo blockId
       -> ( ReverseIndex blockId
          , ParsedInfo blockId
          , Maybe (ParserError blockId e, BlockOffset)
          )
    go revMap acc = \case
      []               -> (revMap, reverse acc, Nothing)
      parsedBlock:rest -> case insertNew bbid internalBlockInfo revMap of
          Right revMap' -> go revMap' (parsedBlock:acc) rest
          Left InternalBlockInfo { ibFile = alreadyExistsHere } ->
              ( revMap
              , reverse acc
              , Just (DuplicatedBlock bbid alreadyExistsHere file, offset)
              )
        where
          (offset, (size, blockInfo@BlockInfo { bbid })) = parsedBlock
          internalBlockInfo = InternalBlockInfo {
              ibFile         = file
            , ibBlockOffset  = offset
            , ibBlockSize    = size
            , ibBlockInfo    = blockInfo
            }

    -- | Insert the value at the key returning the updated map, unless there
    -- already is a key at the same location, in which case we return the
    -- original value.
    --
    -- Should be more efficient than the combination of 'Map.lookup' and
    -- 'Map.insert'.
    insertNew :: forall k a. Ord k => k -> a -> Map k a -> Either a (Map k a)
    insertNew k a m =
      case Map.insertLookupWithKey (\_k new _old -> new) k a m of
        (Nothing, m') -> Right m'
        (Just a', _)  -> Left a'
