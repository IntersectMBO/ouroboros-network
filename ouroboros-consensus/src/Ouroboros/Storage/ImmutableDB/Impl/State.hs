{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Ouroboros.Storage.ImmutableDB.Impl.State
  ( -- * State types
    ImmutableDBEnv (..)
  , InternalState (..)
  , dbIsOpen
  , OpenState (..)
  , ClosedState (..)
    -- * State helpers
  , mkOpenState
  , getOpenState
  , modifyOpenState
  , withOpenState
  , closedStateFromInternalState
  , closeOpenStateHandles
  ) where

import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Control.Monad.Class.MonadThrow hiding (onException)

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocate, unsafeReleaseAll)

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Ouroboros.Storage.ImmutableDB.Impl.Index (Index)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index as Index
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import           Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary
                     (BlockOffset (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Secondary as Secondary
import           Ouroboros.Storage.ImmutableDB.Impl.Util
import           Ouroboros.Storage.ImmutableDB.Types

{------------------------------------------------------------------------------
  Main types
------------------------------------------------------------------------------}

-- | The environment used by the immutable database.
data ImmutableDBEnv m hash = forall h e. ImmutableDBEnv
    { _dbHasFS           :: !(HasFS m h)
    , _dbErr             :: !(ErrorHandling ImmutableDBError m)
    , _dbInternalState   :: !(StrictMVar m (InternalState m hash h))
    , _dbEpochFileParser :: !(EpochFileParser e m (Secondary.Entry hash) hash)
    , _dbEpochInfo       :: !(EpochInfo m)
    , _dbHashInfo        :: !(HashInfo hash)
    , _dbTracer          :: !(Tracer m (TraceEvent e hash))
    , _dbRegistry        :: !(ResourceRegistry m)
    , _dbCacheConfig     :: !Index.CacheConfig
    }

data InternalState m hash h =
    DbClosed !ClosedState
  | DbOpen !(OpenState m hash h)
  deriving (Generic, NoUnexpectedThunks)

dbIsOpen :: InternalState m hash h -> Bool
dbIsOpen (DbClosed _) = False
dbIsOpen (DbOpen _)   = True

-- | Internal state when the database is open.
data OpenState m hash h = OpenState
    { _currentEpoch           :: !EpochNo
      -- ^ The current 'EpochNo' the immutable store is writing to.
    , _currentEpochOffset     :: !BlockOffset
      -- ^ The offset at which the next block will be written in the current
      -- epoch file.
    , _currentSecondaryOffset :: !SecondaryOffset
      -- ^ The offset at which the next index entry will be written in the
      -- current secondary index.
    , _currentEpochHandle     :: !(Handle h)
      -- ^ The write handle for the current epoch file.
    , _currentPrimaryHandle   :: !(Handle h)
      -- ^ The write handle for the current primary index file.
    , _currentSecondaryHandle :: !(Handle h)
      -- ^ The write handle for the current secondary index file.
    , _currentTip             :: !(ImmTipWithHash hash)
      -- ^ The current tip of the database.
    , _nextIteratorID         :: !BaseIteratorID
      -- ^ The ID of the next iterator that will be created.
    , _index                  :: !(Index m hash h)
      -- ^ An abstraction layer on top of the indices to allow for caching.
    }
  deriving (Generic, NoUnexpectedThunks)

-- | Internal state when the database is closed. This contains data that
-- should be restored when the database is reopened. Data not present here
-- will be recovered when reopening.
data ClosedState = ClosedState
    { _closedNextIteratorID :: !BaseIteratorID
      -- ^ See '_nextIteratorID'.
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | Create the internal open state for the given epoch.
mkOpenState
  :: forall m hash h. (HasCallStack, IOLike m)
  => ResourceRegistry m
  -> HasFS m h
  -> ErrorHandling ImmutableDBError m
  -> Index m hash h
  -> EpochNo
  -> BaseIteratorID
  -> ImmTipWithHash hash
  -> AllowExisting
  -> m (OpenState m hash h)
mkOpenState registry HasFS{..} _err index epoch nextIteratorID tip existing = do
    eHnd <- allocateHandle $ hOpen (renderFile "epoch" epoch)     appendMode
    pHnd <- allocateHandle $ Index.openPrimaryIndex index epoch existing
    sHnd <- allocateHandle $ hOpen (renderFile "secondary" epoch) appendMode
    epochOffset     <- hGetSize eHnd
    secondaryOffset <- hGetSize sHnd
    return OpenState
      { _currentEpoch           = epoch
      , _currentEpochOffset     = BlockOffset epochOffset
      , _currentSecondaryOffset = fromIntegral secondaryOffset
      , _currentEpochHandle     = eHnd
      , _currentPrimaryHandle   = pHnd
      , _currentSecondaryHandle = sHnd
      , _currentTip             = tip
      , _nextIteratorID         = nextIteratorID
      , _index                  = index
      }
  where
    appendMode = AppendMode existing

    allocateHandle :: m (Handle h) -> m (Handle h)
    allocateHandle open = snd <$> allocate registry (const open) hClose

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
    internalState <- readMVar _dbInternalState
    case internalState of
       DbClosed _       -> throwUserError _dbErr ClosedDBError
       DbOpen openState -> return (SomePair _dbHasFS openState)

-- | Modify the internal state of an open database.
--
-- In case the database is closed, a 'ClosedDBError' is thrown.
--
-- In case an 'UnexpectedError' is thrown, the database is closed to prevent
-- further appending to a database in a potentially inconsistent state.
--
-- __Note__: This /takes/ the 'TMVar', /then/ runs the action (which might be
-- in 'IO'), and then puts the 'TMVar' back, just like
-- 'Control.Concurrent.MVar.modifyMVar' does. Consequently, it has the same
-- gotchas that @modifyMVar@ does; the effects are observable and it is
-- susceptible to deadlock.
modifyOpenState :: forall m hash r. (HasCallStack, IOLike m)
                => ImmutableDBEnv m hash
                -> (forall h. HasFS m h -> StateT (OpenState m hash h) m r)
                -> m r
modifyOpenState ImmutableDBEnv { _dbHasFS = hasFS :: HasFS m h, .. } action = do
    (mr, ()) <- generalBracket open close (tryImmDB hasFsErr _dbErr . mutation)
    case mr of
      Left  e      -> throwError e
      Right (r, _) -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    -- We use @m (Either e a)@ instead of @EitherT e m a@ for 'generalBracket'
    -- so that 'close' knows which error is thrown (@Either e (s, r)@ vs. @(s,
    -- r)@).

    open :: m (InternalState m hash h)
    open = takeMVar _dbInternalState

    close :: InternalState m hash h
          -> ExitCase (Either ImmutableDBError (r, OpenState m hash h))
          -> m ()
    close !st ec = do
        -- It is crucial to replace the TMVar.
        putMVar _dbInternalState st'
        followUp
      where
        (st', followUp) = case ec of
          -- If we were interrupted, restore the original state.
          ExitCaseAbort                               -> (st, return ())
          ExitCaseException _ex                       -> (st, return ())
          -- In case of success, update to the newest state
          ExitCaseSuccess (Right (_, ost))            ->
              (DbOpen ost, return ())
          -- In case of an unexpected error (not an exception), close the DB
          -- for safety
          ExitCaseSuccess (Left (UnexpectedError {})) ->
              -- This function ('modifyOpenState') can be called from all
              -- threads, not necessarily the one that opened the ImmutableDB
              -- and the registry, so we must use 'unsafeReleaseAll' instead of
              -- 'releaseAll'.
              ( DbClosed (closedStateFromInternalState st)
              , unsafeReleaseAll _dbRegistry )
          -- In case a user error, just restore the previous state
          ExitCaseSuccess (Left (UserError {}))       -> (st, return ())

    mutation :: HasCallStack
             => InternalState m hash h
             -> m (r, OpenState m hash h)
    mutation (DbClosed _) = throwUserError _dbErr ClosedDBError
    mutation (DbOpen ost) = runStateT (action hasFS) ost

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
withOpenState ImmutableDBEnv { _dbHasFS = hasFS :: HasFS m h, .. } action = do
    (mr, ()) <- generalBracket open (const close) (tryImmDB hasFsErr _dbErr . access)
    case mr of
      Left  e -> throwError e
      Right r -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    open :: m (InternalState m hash h)
    open = readMVar _dbInternalState

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readTMVar' here, 'takeTMVar'). So we need to get the most recent
    -- state anyway.
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
    shutDown = do
      -- We need to get the most recent state because it might have changed
      -- behind our back
      updateMVar_ _dbInternalState $ DbClosed . closedStateFromInternalState
      -- This function ('withOpenState') can be called from all threads, not
      -- necessarily the one that opened the ImmutableDB and the registry, so
      -- we must use 'unsafeReleaseAll' instead of 'releaseAll'.
      unsafeReleaseAll _dbRegistry

    access :: HasCallStack
           => InternalState m hash h
           -> m r
    access (DbClosed _) = throwUserError _dbErr ClosedDBError
    access (DbOpen ost) = action hasFS ost

closeOpenStateHandles :: Monad m => HasFS m h -> OpenState m hash h -> m ()
closeOpenStateHandles HasFS { hClose } OpenState {..}  = do
    -- If one of the 'hClose' calls fails, the error will bubble up to the
    -- bracketed call to 'withRegistry', which will close the
    -- 'ResourceRegistry' and thus all the remaining handles in it.
    hClose _currentEpochHandle
    hClose _currentPrimaryHandle
    hClose _currentSecondaryHandle

-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: InternalState m hash h -> ClosedState
closedStateFromInternalState (DbClosed cst) = cst
closedStateFromInternalState (DbOpen OpenState {..}) = ClosedState
    { _closedNextIteratorID  = _nextIteratorID
    }
