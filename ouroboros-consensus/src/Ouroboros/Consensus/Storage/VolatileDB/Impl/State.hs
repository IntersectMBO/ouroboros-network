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
  , InternalState (..)
  , dbIsOpen
  , OpenState (..)
  , ModifyOpenState
  , modifyOpenState
  , withOpenState
  , mkOpenState
  , closeOpenHandles
  ) where

import           Control.Monad
import           Control.Monad.State.Strict hiding (withState)
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
import           Ouroboros.Consensus.Util.ResourceRegistry (WithTempRegistry,
                     allocateTemp, modifyWithTempRegistry)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.VolatileDB.API
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo as FileInfo
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

data VolatileDBEnv m blockId = forall h e. Eq h => VolatileDBEnv {
      hasFS            :: !(HasFS m h)
    , varInternalState :: !(StrictMVar m (InternalState blockId h))
    , maxBlocksPerFile :: !BlocksPerFile
    , parser           :: !(Parser e m blockId)
    , tracer           :: !(Tracer m (TraceEvent e blockId))
    }

data InternalState blockId h =
    DbClosed
  | DbOpen !(OpenState blockId h)
  deriving (Generic, NoUnexpectedThunks)

dbIsOpen :: InternalState blockId h -> Bool
dbIsOpen (DbOpen _) = True
dbIsOpen DbClosed   = False

-- | Internal state when the database is open.
data OpenState blockId h = OpenState {
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

-- | Shorthand
type ModifyOpenState m blockId h =
  StateT (OpenState blockId h) (WithTempRegistry (OpenState blockId h) m)

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyOpenState
  :: forall blockId m a. (HasCallStack, IOLike m)
  => VolatileDBEnv m blockId
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m blockId h a)
  -> m a
modifyOpenState VolatileDBEnv {hasFS = hasFS :: HasFS m h, varInternalState} modSt = do
    wrapFsError $ modifyWithTempRegistry getSt putSt (modSt hasFS)
  where
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    getSt :: m (OpenState blockId h)
    getSt = uninterruptibleMask_ $ takeMVar varInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> do
        putMVar varInternalState DbClosed
        throwM $ UserError ClosedDBError

    putSt :: OpenState blockId h -> ExitCase (OpenState blockId h) -> m ()
    putSt ost ec = do
        -- It is crucial to replace the MVar.
        putMVar varInternalState st'
        unless (dbIsOpen st') $ closeOpenHandles hasFS ost
      where
        st' = case ec of
          ExitCaseSuccess ost' -> DbOpen ost'

          -- When something goes wrong, close the VolatileDB for safety.
          -- Except for user errors, because they stem from incorrect use of
          -- the VolatileDB.
          --
          -- NOTE: we only modify the VolatileDB in background threads of the
          -- ChainDB, not in per-connection threads that could be killed at
          -- any point. When an exception is encountered while modifying the
          -- VolatileDB in a background thread, or that background thread
          -- itself is killed with an async exception, we will shut down the
          -- node anway, so it is safe to close the VolatileDB here.
          ExitCaseAbort        -> DbClosed
          ExitCaseException ex
            | Just (UserError {}) <- fromException ex
            -> DbOpen ost
            | otherwise
            -> DbClosed

-- | Perform an action that accesses the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedError' is thrown while the action is being run, the
-- database is closed to prevent further appending to a database in a
-- potentially inconsistent state. All other exceptions will leave the
-- database open.
withOpenState
  :: forall blockId m r. (HasCallStack, IOLike m)
  => VolatileDBEnv m blockId
  -> (forall h. HasFS m h -> OpenState blockId h -> m r)
  -> m r
withOpenState VolatileDBEnv {hasFS = hasFS :: HasFS m h, varInternalState} action = do
    (mr, ()) <- generalBracket open (const close) (tryVolDB . access)
    case mr of
      Left  e -> throwM e
      Right r -> return r
  where
    open :: m (OpenState blockId h)
    open = readMVar varInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> throwM $ UserError ClosedDBError

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readMVar' here, not 'takeMVar'). So we need to get the most
    -- recent state anyway.
    close :: ExitCase (Either VolatileDBError r)
          -> m ()
    close ec = case ec of
      ExitCaseAbort                               -> return ()
      ExitCaseException _ex                       -> return ()
      ExitCaseSuccess (Right _)                   -> return ()
      -- In case of a VolatileDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedError {})) -> shutDown
      ExitCaseSuccess (Left (UserError {}))       -> return ()

    shutDown :: m ()
    shutDown = swapMVar varInternalState DbClosed >>= \case
      DbOpen ost -> wrapFsError $ closeOpenHandles hasFS ost
      DbClosed   -> return ()

    access :: OpenState blockId h -> m r
    access = action hasFS

-- | Close the handles in the 'OpenState'.
--
-- Idempotent, as closing a handle is idempotent.
--
-- NOTE: does not wrap 'FsError's and must be called within 'wrapFsError' or
-- 'tryVolDB'.
closeOpenHandles :: HasFS m h -> OpenState blockId h -> m ()
closeOpenHandles HasFS { hClose } OpenState { currentWriteHandle } =
    hClose currentWriteHandle

mkOpenState
  :: forall m blockId e h.
     ( HasCallStack
     , IOLike m
     , Ord blockId
     , Eq h
     )
  => HasFS m h
  -> Parser e m blockId
  -> Tracer m (TraceEvent e blockId)
  -> BlocksPerFile
  -> WithTempRegistry (OpenState blockId h) m (OpenState blockId h)
mkOpenState hasFS@HasFS{..} parser tracer maxBlocksPerFile = do
    lift $ createDirectoryIfMissing True dbDir
    allFiles <- map toFsPath . Set.toList <$> lift (listDirectory dbDir)
    filesWithIds <- lift $ logInvalidFiles $ parseAllFds allFiles
    mkOpenStateHelper hasFS parser tracer maxBlocksPerFile filesWithIds
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

-- | Make the 'OpenState' by parsing all files.
--
-- It may create a new file to append new blocks to or use an existing one.
mkOpenStateHelper
  :: forall blockId m h e. (
       HasCallStack
     , IOLike m
     , Ord blockId
     , Eq h
     )
  => HasFS m h
  -> Parser e m blockId
  -> Tracer m (TraceEvent e blockId)
  -> BlocksPerFile
  -> [(FileId, FsPath)]
  -> WithTempRegistry (OpenState blockId h) m (OpenState blockId h)
mkOpenStateHelper hasFS parser tracer maxBlocksPerFile files = do
    (currentMap', currentRevMap', currentSuccMap') <- lift $
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

    currentWriteHandle <-
      allocateTemp
        (hOpen   hasFS currentWritePath (AppendMode AllowExisting))
        (hClose' hasFS)
        ((==) . currentWriteHandle)
    currentWriteOffset <- lift $ hGetSize hasFS currentWriteHandle

    return OpenState {
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
