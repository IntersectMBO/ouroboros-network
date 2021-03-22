{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}

module Ouroboros.Consensus.Storage.VolatileDB.Impl.State (
    -- * Tracing
    TraceEvent (..)
    -- * State types
  , BlockOffset (..)
  , BlockSize (..)
  , FileId
  , InternalState (..)
  , OpenState (..)
  , ReverseIndex
  , SuccessorsIndex
  , VolatileDBEnv (..)
  , dbIsOpen
    -- * State helpers
  , ModifyOpenState
  , appendOpenState
  , closeOpenHandles
  , mkOpenState
  , withOpenState
  , writeOpenState
  ) where

import           Control.Monad
import           Control.Monad.State.Strict hiding (withState)
import           Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString.Lazy as Lazy
import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Ouroboros.Network.Block (MaxSlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (whenJust, (.:))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.MonadSTM.RAWLock (RAWLock)
import qualified Ouroboros.Consensus.Util.MonadSTM.RAWLock as RAWLock
import           Ouroboros.Consensus.Util.ResourceRegistry (WithTempRegistry,
                     allocateTemp, modifyWithTempRegistry)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Storage.VolatileDB.API
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.FileInfo as FileInfo
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Parser
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Types
import           Ouroboros.Consensus.Storage.VolatileDB.Impl.Util

{------------------------------------------------------------------------------
  State types
------------------------------------------------------------------------------}

data VolatileDBEnv m blk = forall h. Eq h => VolatileDBEnv {
      hasFS            :: !(HasFS m h)
    , varInternalState :: !(RAWLock m (InternalState blk h))
    , maxBlocksPerFile :: !BlocksPerFile
    , checkIntegrity   :: !(blk -> Bool)
    , codecConfig      :: !(CodecConfig blk)
    , tracer           :: !(Tracer m (TraceEvent blk))
    }

data InternalState blk h =
    DbClosed
  | DbOpen !(OpenState blk h)
  deriving (Generic, NoThunks)

dbIsOpen :: InternalState blk h -> Bool
dbIsOpen (DbOpen _) = True
dbIsOpen DbClosed   = False

-- | Internal state when the database is open.
data OpenState blk h = OpenState {
      currentWriteHandle :: !(Handle h)
      -- ^ The only open file we append blocks to.
    , currentWritePath   :: !FsPath
      -- ^ The path of the file above.
    , currentWriteId     :: !FileId
      -- ^ The 'FileId' of the same file.
    , currentWriteOffset :: !Word64
      -- ^ The offset of the same file.
    , currentMap         :: !(Index blk)
      -- ^ The contents of each file.
    , currentRevMap      :: !(ReverseIndex blk)
      -- ^ Where to find each block based on its slot number.
    , currentSuccMap     :: !(SuccessorsIndex blk)
      -- ^ The successors for each block.
    , currentMaxSlotNo   :: !MaxSlotNo
      -- ^ Highest stored SlotNo.
      --
      -- INVARIANT: this is the cached value of:
      -- > FileInfo.maxSlotNoInFiles (Index.elems (currentMap st))
    }
  deriving (Generic, NoThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | Shorthand
type ModifyOpenState m blk h =
  StateT (OpenState blk h) (WithTempRegistry (OpenState blk h) m)

data AppendOrWrite = Append | Write

-- | NOTE: This is safe in terms of throwing FsErrors.
modifyOpenState ::
     forall blk m a. (IOLike m, HasCallStack, StandardHash blk, Typeable blk)
  => AppendOrWrite
  -> VolatileDBEnv m blk
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m blk h a)
  -> m a
modifyOpenState appendOrWrite
                VolatileDBEnv {hasFS = hasFS :: HasFS m h, varInternalState}
                modSt =
    wrapFsError (Proxy @blk) $ modifyWithTempRegistry getSt putSt (modSt hasFS)
  where
    -- NOTE: we can't use the bracketed variants, as that's incompatible with
    -- 'modifyWithTempRegistry', which takes a function to put back the state,
    -- as that must have succeeded before the resources are released from the
    -- temporary registry.
    (acquire, release) = case appendOrWrite of
      Append ->
        (atomically .  RAWLock.unsafeAcquireAppendAccess,
         atomically .: RAWLock.unsafeReleaseAppendAccess)
      Write  ->
        (RAWLock.unsafeAcquireWriteAccess, RAWLock.unsafeReleaseWriteAccess)

    getSt :: m (OpenState blk h)
    getSt = acquire varInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> do
        release varInternalState DbClosed
        throwIO $ ApiMisuse @blk $ ClosedDBError Nothing

    putSt :: OpenState blk h -> ExitCase (OpenState blk h) -> m ()
    putSt ost ec = case closeOrRelease of
        -- We must close the VolatileDB
        Left ex -> do
          -- Poison the internal state lock with the exception that caused us
          -- to close the VolatileDB so the next time somebody accesses the
          -- VolatileDB, a 'ClosedDBError' containing the exception that
          -- caused it is thrown.
          --
          -- We don't care about the current state, as we were appending or
          -- writing, which means that the state couldn't have changed in the
          -- background.
          _mbCurState <-
            RAWLock.poison varInternalState $ \_st ->
              ApiMisuse @blk (ClosedDBError (Just ex))
          closeOpenHandles hasFS ost
        Right ost' -> release varInternalState (DbOpen ost')
      where
        closeOrRelease :: Either SomeException (OpenState blk h)
        closeOrRelease = case ec of
          ExitCaseSuccess ost'
            -> Right ost'
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
          ExitCaseAbort
            -- Only caused by 'throwE' or 'throwError' like functions, which
            -- we don't use, but we use @IOLike m => m@ here.
            -> error "impossible"
          ExitCaseException ex
            | Just (ApiMisuse {} :: VolatileDBError blk) <- fromException ex
            -> Right ost
            | otherwise
            -> Left ex

-- | Append to the open state. Reads can happen concurrently with this
-- operation.
--
-- NOTE: This is safe in terms of throwing FsErrors.
appendOpenState ::
     forall blk m a. (IOLike m, Typeable blk, StandardHash blk)
  => VolatileDBEnv m blk
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m blk h a)
  -> m a
appendOpenState = modifyOpenState Append

-- | Write to the open state. No reads or appends can concurrently with this
-- operation.
--
-- NOTE: This is safe in terms of throwing FsErrors.
writeOpenState ::
     forall blk m a. (IOLike m, Typeable blk, StandardHash blk)
  => VolatileDBEnv m blk
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m blk h a)
  -> m a
writeOpenState = modifyOpenState Write

-- | Perform an action that accesses the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedFailure' is thrown while the action is being run, the
-- database is closed to prevent further appending to a database in a
-- potentially inconsistent state. All other exceptions will leave the database
-- open.
withOpenState ::
     forall blk m r. (IOLike m, StandardHash blk, Typeable blk)
  => VolatileDBEnv m blk
  -> (forall h. HasFS m h -> OpenState blk h -> m r)
  -> m r
withOpenState VolatileDBEnv {hasFS = hasFS :: HasFS m h, varInternalState} action = do
    (mr, ()) <- generalBracket open close (tryVolatileDB (Proxy @blk). access)
    case mr of
      Left  e -> throwIO e
      Right r -> return r
  where
    open :: m (OpenState blk h)
    open =
      atomically (RAWLock.unsafeAcquireReadAccess varInternalState) >>= \case
        DbOpen ost -> return ost
        DbClosed   -> do
          atomically $ RAWLock.unsafeReleaseReadAccess varInternalState
          throwIO $ ApiMisuse @blk $ ClosedDBError Nothing

    close ::
         OpenState blk h
      -> ExitCase (Either (VolatileDBError blk) r)
      -> m ()
    close ost ec
        | Just ex <- shouldClose
        = do
            -- Poison the internal state lock with the exception that caused
            -- us to close the VolatileDB so the next time somebody accesses
            -- the VolatileDB, a 'ClosedDBError' containing the exception that
            -- caused it is thrown.
            mbCurState <-
              RAWLock.poison varInternalState $ \_st ->
                ApiMisuse @blk (ClosedDBError (Just ex))
            -- Close the open handles
            wrapFsError (Proxy @blk) $ case mbCurState of
              -- The handles in the most recent state
              Just (DbOpen ost') -> closeOpenHandles hasFS ost'
              -- The state was already closed, which is always followed by
              -- closing the open handles, so nothing to do.
              Just DbClosed      -> return ()
              -- No current value, e.g., we interrupted a thread in a middle
              -- of a write. Close the last open handles we know about. The
              -- interrupted thread will clean up its own resources that
              -- haven't yet made it into the state (thanks to
              -- 'modifyWithTempRegistry').
              Nothing            -> closeOpenHandles hasFS ost

        | otherwise
        = atomically $ RAWLock.unsafeReleaseReadAccess varInternalState
      where
        shouldClose :: Maybe SomeException
        shouldClose = case ec of
          ExitCaseAbort                                  -> Nothing
          ExitCaseException _ex                          -> Nothing
          ExitCaseSuccess (Right _)                      -> Nothing
          -- In case of a VolatileDBError, close when unexpected
          ExitCaseSuccess (Left ex@UnexpectedFailure {}) -> Just (toException ex)
          ExitCaseSuccess (Left ApiMisuse {})            -> Nothing

    access :: OpenState blk h -> m r
    access = action hasFS

-- | Close the handles in the 'OpenState'.
--
-- Idempotent, as closing a handle is idempotent.
--
-- NOTE: does not wrap 'FsError's and must be called within 'wrapFsError' or
-- 'tryVolatileDB'.
closeOpenHandles :: HasFS m h -> OpenState blk h -> m ()
closeOpenHandles HasFS { hClose } OpenState { currentWriteHandle } =
    hClose currentWriteHandle

mkOpenState ::
     forall m blk h.
     ( HasCallStack
     , IOLike m
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , HasNestedContent Header blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , Eq h
     )
  => CodecConfig blk
  -> HasFS m h
  -> (blk -> Bool)
  -> BlockValidationPolicy
  -> Tracer m (TraceEvent blk)
  -> BlocksPerFile
  -> WithTempRegistry (OpenState blk h) m (OpenState blk h)
mkOpenState ccfg hasFS@HasFS{..} checkInvariants validationPolicy tracer maxBlocksPerFile = do
    lift $ createDirectoryIfMissing True dbDir
    allFiles <- map toFsPath . Set.toList <$> lift (listDirectory dbDir)
    filesWithIds <- lift $ logInvalidFiles $ parseAllFds allFiles
    mkOpenStateHelper
      ccfg
      hasFS
      checkInvariants
      validationPolicy
      tracer
      maxBlocksPerFile
      filesWithIds
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
type Indices blk =
  ( Index           blk
  , ReverseIndex    blk
  , SuccessorsIndex blk
  )

-- | Make the 'OpenState' by parsing all files.
--
-- It may create a new file to append new blocks to or use an existing one.
mkOpenStateHelper ::
     forall blk m h.
     ( HasCallStack
     , IOLike m
     , HasHeader blk
     , GetPrevHash blk
     , HasBinaryBlockInfo blk
     , HasNestedContent Header blk
     , DecodeDisk blk (Lazy.ByteString -> blk)
     , Eq h
     )
  => CodecConfig blk
  -> HasFS m h
  -> (blk -> Bool)
  -> BlockValidationPolicy
  -> Tracer m (TraceEvent blk)
  -> BlocksPerFile
  -> [(FileId, FsPath)]
  -> WithTempRegistry (OpenState blk h) m (OpenState blk h)
mkOpenStateHelper ccfg hasFS checkIntegrity validationPolicy tracer maxBlocksPerFile files = do
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
      , currentMaxSlotNo   = FileInfo.maxSlotNoInFiles (Index.elems currentMap')
      }
  where
    validateFile :: Indices blk -> (FileId, FsPath) -> m (Indices blk)
    validateFile (currentMap, currentRevMap, currentSuccMap) (fd, file) = do
      (parsedBlocks, mErr) <-
        parseBlockFile ccfg hasFS checkIntegrity validationPolicy file
      whenJust mErr $ \(e, offset) ->
        truncateError file e offset

      let (currentRevMap', acceptedBlocks, mErr') =
            addToReverseIndex file currentRevMap parsedBlocks
      -- We can find duplicate blocks when merging the parsed blocks with the
      -- 'ReverseIndex', so we might have to truncate at this point too.
      whenJust mErr' $ \(e, offset) ->
        truncateError file e offset

      let fileInfo        = FileInfo.fromParsedBlockInfos acceptedBlocks
          currentMap'     = Index.insert fd fileInfo currentMap
          currentSuccMap' = foldl'
            (\succMap ParsedBlockInfo { pbiBlockInfo } ->
              insertMapSet (biPrevHash pbiBlockInfo) (biHash pbiBlockInfo) succMap)
            currentSuccMap
            acceptedBlocks

      return (currentMap', currentRevMap', currentSuccMap')

    truncateError :: FsPath -> ParseError blk -> BlockOffset -> m ()
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
        hTruncate hasFS hndl (unBlockOffset offset)

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
addToReverseIndex ::
     forall blk. HasHeader blk
  => FsPath
  -> ReverseIndex blk
  -> [ParsedBlockInfo blk]
  -> ( ReverseIndex blk
     , [ParsedBlockInfo blk]
     , Maybe (ParseError blk, BlockOffset)
     )
addToReverseIndex file = \revMap -> go revMap []
  where
    go ::
         ReverseIndex blk
      -> [ParsedBlockInfo blk] -- accumulator of the accepted blocks.
      -> [ParsedBlockInfo blk]
      -> ( ReverseIndex blk
         , [ParsedBlockInfo blk]
         , Maybe (ParseError blk, BlockOffset)
         )
    go revMap acc = \case
      []               -> (revMap, reverse acc, Nothing)
      parsedBlock:rest -> case insertNew biHash internalBlockInfo revMap of
          Right revMap' -> go revMap' (parsedBlock:acc) rest
          Left InternalBlockInfo { ibiFile = alreadyExistsHere } ->
              ( revMap
              , reverse acc
              , Just (DuplicatedBlock biHash alreadyExistsHere file, offset)
              )
        where
          ParsedBlockInfo {
              pbiBlockOffset = offset
            , pbiBlockSize   = size
            , pbiBlockInfo   = blockInfo@BlockInfo { biHash }
            , pbiNestedCtxt  = nestedCtxt
            } = parsedBlock
          internalBlockInfo = InternalBlockInfo {
              ibiFile         = file
            , ibiBlockOffset  = offset
            , ibiBlockSize    = size
            , ibiBlockInfo    = blockInfo
            , ibiNestedCtxt   = nestedCtxt
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
