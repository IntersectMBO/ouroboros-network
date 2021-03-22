{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Storage.ImmutableDB.Impl.State (
    -- * State types
    ImmutableDBEnv (..)
  , InternalState (..)
  , OpenState (..)
  , dbIsOpen
    -- * State helpers
  , ModifyOpenState
  , cleanUp
  , closeOpenHandles
  , getOpenState
  , mkOpenState
  , modifyOpenState
  , withOpenState
  ) where

import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     WithTempRegistry, allocateTemp, modifyWithTempRegistry)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.ImmutableDB.API
import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

-- | The environment used by the immutable database.
data ImmutableDBEnv m blk = forall h. Eq h => ImmutableDBEnv {
      hasFS            :: !(HasFS m h)
    , varInternalState :: !(StrictMVar m (InternalState m blk h))
    , checkIntegrity   :: !(blk -> Bool)
    , chunkInfo        :: !ChunkInfo
    , tracer           :: !(Tracer m (TraceEvent blk))
    , registry         :: !(ResourceRegistry m)
    , cacheConfig      :: !Index.CacheConfig
    , codecConfig      :: !(CodecConfig blk)
    }

data InternalState m blk h =
    DbClosed
  | DbOpen !(OpenState m blk h)
  deriving (Generic, NoThunks)

dbIsOpen :: InternalState m blk h -> Bool
dbIsOpen DbClosed   = False
dbIsOpen (DbOpen _) = True

-- | Internal state when the database is open.
data OpenState m blk h = OpenState {
      currentChunk           :: !ChunkNo
      -- ^ The current 'ChunkNo' the immutable store is writing to.
    , currentChunkOffset     :: !BlockOffset
      -- ^ The offset at which the next block will be written in the current
      -- chunk file.
    , currentSecondaryOffset :: !SecondaryOffset
      -- ^ The offset at which the next index entry will be written in the
      -- current secondary index.
    , currentChunkHandle     :: !(Handle h)
      -- ^ The write handle for the current chunk file.
    , currentPrimaryHandle   :: !(Handle h)
      -- ^ The write handle for the current primary index file.
    , currentSecondaryHandle :: !(Handle h)
      -- ^ The write handle for the current secondary index file.
    , currentTip             :: !(WithOrigin (Tip blk))
      -- ^ The current tip of the database.
    , currentIndex           :: !(Index m blk h)
      -- ^ An abstraction layer on top of the indices to allow for caching.
    }
  deriving (Generic, NoThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | Create the internal open state for the given chunk.
mkOpenState ::
     forall m blk h. (HasCallStack, IOLike m, Eq h)
  => HasFS m h
  -> Index m blk h
  -> ChunkNo
  -> WithOrigin (Tip blk)
  -> AllowExisting
  -> WithTempRegistry (OpenState m blk h) m (OpenState m blk h)
mkOpenState hasFS@HasFS{..} index chunk tip existing = do
    eHnd <- allocateHandle currentChunkHandle     $ hOpen (fsPathChunkFile          chunk) appendMode
    pHnd <- allocateHandle currentPrimaryHandle   $ Index.openPrimaryIndex index    chunk  existing
    sHnd <- allocateHandle currentSecondaryHandle $ hOpen (fsPathSecondaryIndexFile chunk) appendMode
    chunkOffset     <- lift $ hGetSize eHnd
    secondaryOffset <- lift $ hGetSize sHnd
    return OpenState
      { currentChunk           = chunk
      , currentChunkOffset     = BlockOffset chunkOffset
      , currentSecondaryOffset = fromIntegral secondaryOffset
      , currentChunkHandle     = eHnd
      , currentPrimaryHandle   = pHnd
      , currentSecondaryHandle = sHnd
      , currentTip             = tip
      , currentIndex           = index
      }
  where
    appendMode = AppendMode existing

    allocateHandle
      :: (OpenState m blk h -> Handle h)
      -> m (Handle h)
      -> WithTempRegistry (OpenState m blk h) m (Handle h)
    allocateHandle getHandle open =
      -- To check whether the handle made it in the final state, we check for
      -- equality.
      allocateTemp open (hClose' hasFS) ((==) . getHandle)

-- | Get the 'OpenState' of the given database, throw a 'ClosedDBError' in
-- case it is closed.
--
-- NOTE: Since the 'OpenState' is parameterized over a type parameter @h@ of
-- handles, which is not visible from the type of the @ImmutableDBEnv@,
-- we return a @SomePair@ here that returns the open state along with a 'HasFS'
-- instance for the /same/ type parameter @h@. Note that it would be impossible
-- to use an existing 'HasFS' instance already in scope otherwise, since the
-- @h@ parameters would not be known to match.
getOpenState ::
     forall m blk. (HasCallStack, IOLike m, StandardHash blk, Typeable blk)
  => ImmutableDBEnv m blk
  -> STM m (SomePair (HasFS m) (OpenState m blk))
getOpenState ImmutableDBEnv {..} = do
    -- We use 'readMVarSTM' to read a potentially stale internal state if
    -- somebody's appending to the ImmutableDB at the same time.
    internalState <- readMVarSTM varInternalState
    case internalState of
       DbClosed         -> throwApiMisuse $ ClosedDBError @blk
       DbOpen openState -> return (SomePair hasFS openState)

-- | Shorthand
type ModifyOpenState m blk h =
  StateT (OpenState m blk h) (WithTempRegistry (OpenState m blk h) m)

-- | Modify the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedFailure' is thrown, the database is closed to prevent
-- further appending to a database in a potentially inconsistent state.
--
-- The action is run in the 'ModifyOpenState' monad, which is a 'StateT'
-- transformer (of the 'OpenState') over the 'WithTempRegistry' monad. This
-- monad can be used to allocate resources in that will be transferred to the
-- returned 'OpenState' that is safely stored in the 'ImmutableDBEnv'. This
-- approach makes sure that no resources are leaked when an exception is
-- thrown while running the action modifying the state.
--
-- __Note__: This /takes/ the 'TMVar', /then/ runs the action (which might be
-- in 'IO'), and then puts the 'TMVar' back, just like
-- 'Control.Concurrent.MVar.modifyMVar' does. Consequently, it has the same
-- gotchas that @modifyMVar@ does; the effects are observable and it is
-- susceptible to deadlock.
modifyOpenState ::
     forall m blk a. (HasCallStack, IOLike m, StandardHash blk, Typeable blk)
  => ImmutableDBEnv m blk
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m blk h a)
  -> m a
modifyOpenState ImmutableDBEnv { hasFS = hasFS :: HasFS m h, .. } modSt =
    wrapFsError (Proxy @blk) $ modifyWithTempRegistry getSt putSt (modSt hasFS)
  where
    getSt :: m (OpenState m blk h)
    getSt = mask_ $ takeMVar varInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> do
        putMVar varInternalState DbClosed
        throwApiMisuse $ ClosedDBError @blk

    putSt :: OpenState m blk h -> ExitCase (OpenState m blk h) -> m ()
    putSt ost ec = do
        -- It is crucial to replace the MVar.
        putMVar varInternalState st'
        unless (dbIsOpen st') $ cleanUp hasFS ost
      where
        st' = case ec of
          ExitCaseSuccess ost'  -> DbOpen ost'

          -- When something goes wrong, close the ImmutableDB for safety.
          -- Except for user errors, because they stem from incorrect use of
          -- the ImmutableDB.
          --
          -- NOTE: we only modify the ImmutableDB in a background thread of
          -- the ChainDB, not in per-connection threads that could be killed
          -- at any point. When an exception is encountered while modifying
          -- the ImmutableDB in the background thread, or that background
          -- thread itself is killed with an async exception, we will shut
          -- down the node anway, so it is safe to close the ImmutableDB here.
          ExitCaseAbort         -> DbClosed
          ExitCaseException ex
            | Just (ApiMisuse {} :: ImmutableDBError blk) <- fromException ex
            -> DbOpen ost
            | otherwise
            -> DbClosed

-- | Perform an action that accesses the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedFailure' is thrown while the action is being run, the
-- database is closed to prevent further appending to a database in a
-- potentially inconsistent state.
withOpenState ::
     forall m blk r. (HasCallStack, IOLike m, StandardHash blk, Typeable blk)
  => ImmutableDBEnv m blk
  -> (forall h. HasFS m h -> OpenState m blk h -> m r)
  -> m r
withOpenState ImmutableDBEnv { hasFS = hasFS :: HasFS m h, .. } action = do
    (mr, ()) <-
      generalBracket open (const close) (tryImmutableDB (Proxy @blk) . access)
    case mr of
      Left  e -> throwIO e
      Right r -> return r
  where
    -- We use 'readMVarSTM' to read a potentially stale internal state if
    -- somebody's appending to the ImmutableDB at the same time. Reads can
    -- safely happen concurrently with appends, so this is fine and allows for
    -- some extra concurrency.
    open :: m (OpenState m blk h)
    open = atomically (readMVarSTM varInternalState) >>= \case
      DbOpen ost -> return ost
      DbClosed   -> throwApiMisuse $ ClosedDBError @blk

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readMVarSTM' here, not 'takeMVar'). So we need to get the most
    -- recent state anyway.
    close :: ExitCase (Either (ImmutableDBError blk) r)
          -> m ()
    close ec = case ec of
      ExitCaseAbort                                 -> return ()
      ExitCaseException _ex                         -> return ()
      ExitCaseSuccess (Right _)                     -> return ()
      -- In case of an ImmutableDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedFailure {})) -> shutDown
      ExitCaseSuccess (Left (ApiMisuse {}))         -> return ()

    shutDown :: m ()
    shutDown = swapMVar varInternalState DbClosed >>= \case
      DbOpen ost -> wrapFsError (Proxy @blk) $ cleanUp hasFS ost
      DbClosed   -> return ()

    access :: OpenState m blk h -> m r
    access = action hasFS

-- | Close the handles in the 'OpenState'.
--
-- Idempotent, as closing a handle is idempotent.
closeOpenHandles :: Monad m => HasFS m h -> OpenState m blk h -> m ()
closeOpenHandles HasFS { hClose } OpenState {..}  = do
    hClose currentChunkHandle
    hClose currentPrimaryHandle
    hClose currentSecondaryHandle

-- | Clean up the 'OpenState': 'closeOpenHandles' + close the index (i.e.,
-- shut down its background thread)
cleanUp :: Monad m => HasFS m h -> OpenState m blk h -> m ()
cleanUp hasFS ost@OpenState {..}  = do
    Index.close currentIndex
    closeOpenHandles hasFS ost
