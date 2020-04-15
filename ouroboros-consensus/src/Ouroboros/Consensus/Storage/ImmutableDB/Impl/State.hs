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
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
  ( -- * State types
    ImmutableDBEnv (..)
  , InternalState (..)
  , dbIsOpen
  , OpenState (..)
    -- * State helpers
  , mkOpenState
  , getOpenState
  , ModifyOpenState
  , modifyOpenState
  , withOpenState
  , closeOpenHandles
  , cleanUp
  ) where

import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     WithTempRegistry, allocateTemp, modifyWithTempRegistry)

import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types

import           Ouroboros.Consensus.Storage.ImmutableDB.Chunks
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Consensus.Storage.ImmutableDB.Parser (BlockSummary)
import           Ouroboros.Consensus.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

-- | The environment used by the immutable database.
data ImmutableDBEnv m hash = forall h e. Eq h => ImmutableDBEnv
    { hasFS            :: !(HasFS m h)
    , varInternalState :: !(StrictMVar m (InternalState m hash h))
    , chunkFileParser  :: !(ChunkFileParser e m (BlockSummary hash) hash)
    , chunkInfo        :: !ChunkInfo
    , hashInfo         :: !(HashInfo hash)
    , tracer           :: !(Tracer m (TraceEvent e hash))
    , registry         :: !(ResourceRegistry m)
    , cacheConfig      :: !Index.CacheConfig
    }

data InternalState m hash h =
    DbClosed
  | DbOpen !(OpenState m hash h)
  deriving (Generic, NoUnexpectedThunks)

dbIsOpen :: InternalState m hash h -> Bool
dbIsOpen DbClosed   = False
dbIsOpen (DbOpen _) = True

-- | Internal state when the database is open.
data OpenState m hash h = OpenState
    { currentChunk           :: !ChunkNo
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
    , currentTip             :: !(ImmTipWithInfo hash)
      -- ^ The current tip of the database.
    , currentIndex           :: !(Index m hash h)
      -- ^ An abstraction layer on top of the indices to allow for caching.
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | Create the internal open state for the given chunk.
mkOpenState
  :: forall m hash h. (HasCallStack, IOLike m, Eq h)
  => HasFS m h
  -> Index m hash h
  -> ChunkNo
  -> ImmTipWithInfo hash
  -> AllowExisting
  -> WithTempRegistry (OpenState m hash h) m (OpenState m hash h)
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
      :: (OpenState m hash h -> Handle h)
      -> m (Handle h)
      -> WithTempRegistry (OpenState m hash h) m (Handle h)
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
getOpenState :: (HasCallStack, IOLike m)
             => ImmutableDBEnv m hash
             -> m (SomePair (HasFS m) (OpenState m hash))
getOpenState ImmutableDBEnv {..} = do
    -- We use 'readMVarSTM' to read a potentially stale internal state if
    -- somebody's appending to the ImmutableDB at the same time.
    internalState <- atomically $ readMVarSTM varInternalState
    case internalState of
       DbClosed         -> throwUserError  ClosedDBError
       DbOpen openState -> return (SomePair hasFS openState)

-- | Shorthand
type ModifyOpenState m hash h =
  StateT (OpenState m hash h) (WithTempRegistry (OpenState m hash h) m)

-- | Modify the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedError' is thrown, the database is closed to prevent
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
modifyOpenState
  :: forall m hash a. (HasCallStack, IOLike m)
  => ImmutableDBEnv m hash
  -> (forall h. Eq h => HasFS m h -> ModifyOpenState m hash h a)
  -> m a
modifyOpenState ImmutableDBEnv { hasFS = hasFS :: HasFS m h, .. } modSt =
    wrapFsError $ modifyWithTempRegistry getSt putSt (modSt hasFS)
  where
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    getSt :: m (OpenState m hash h)
    getSt = uninterruptibleMask_ $ takeMVar varInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> do
        putMVar varInternalState DbClosed
        throwUserError ClosedDBError

    putSt :: OpenState m hash h -> ExitCase (OpenState m hash h) -> m ()
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
-- potentially inconsistent state.
withOpenState :: forall m hash r. (HasCallStack, IOLike m)
              => ImmutableDBEnv m hash
              -> (forall h. HasFS m h -> OpenState m hash h -> m r)
              -> m r
withOpenState ImmutableDBEnv { hasFS = hasFS :: HasFS m h, .. } action = do
    (mr, ()) <- generalBracket open (const close) (tryImmDB . access)
    case mr of
      Left  e -> throwM e
      Right r -> return r
  where
    HasFS{..} = hasFS

    -- We use 'readMVarSTM' to read a potentially stale internal state if
    -- somebody's appending to the ImmutableDB at the same time. Reads can
    -- safely happen concurrently with appends, so this is fine and allows for
    -- some extra concurrency.
    open :: m (OpenState m hash h)
    open = atomically (readMVarSTM varInternalState) >>= \case
      DbOpen ost -> return ost
      DbClosed   -> throwUserError ClosedDBError

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readMVarSTM' here, not 'takeMVar'). So we need to get the most
    -- recent state anyway.
    close :: ExitCase (Either ImmutableDBError r)
          -> m ()
    close ec = case ec of
      ExitCaseAbort                               -> return ()
      ExitCaseException _ex                       -> return ()
      ExitCaseSuccess (Right _)                   -> return ()
      -- In case of an ImmutableDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedError {})) -> shutDown
      ExitCaseSuccess (Left (UserError {}))       -> return ()

    shutDown :: m ()
    shutDown = swapMVar varInternalState DbClosed >>= \case
      DbOpen ost -> wrapFsError $ cleanUp hasFS ost
      DbClosed   -> return ()

    access :: OpenState m hash h -> m r
    access = action hasFS

-- | Close the handles in the 'OpenState'.
--
-- Idempotent, as closing a handle is idempotent.
closeOpenHandles :: Monad m => HasFS m h -> OpenState m hash h -> m ()
closeOpenHandles HasFS { hClose } OpenState {..}  = do
    hClose currentChunkHandle
    hClose currentPrimaryHandle
    hClose currentSecondaryHandle

-- | Clean up the 'OpenState': 'closeOpenHandles' + close the index (i.e.,
-- shut down its background thread)
cleanUp :: Monad m => HasFS m h -> OpenState m hash h -> m ()
cleanUp hasFS ost@OpenState {..}  = do
    Index.close currentIndex
    closeOpenHandles hasFS ost
