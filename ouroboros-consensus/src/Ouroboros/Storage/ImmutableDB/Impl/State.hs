{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Ouroboros.Storage.ImmutableDB.Impl.State
  ( -- * Main types
    ImmutableDBEnv (..)
  , InternalState (..)
  , dbIsOpen
  , OpenState (..)
  , ClosedState (..)
    -- * State helpers
  , mkOpenState
  , mkOpenStateNewEpoch
  , getOpenState
  , modifyOpenState
  , withOpenState
  , closedStateFromInternalState
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.CBOR.Encoding (Encoding)
import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Control.Monad.Class.MonadThrow

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))

import           Ouroboros.Storage.ImmutableDB.Impl.Index
import           Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets
                     (SlotOffsets (..))
import qualified Ouroboros.Storage.ImmutableDB.Impl.SlotOffsets as SlotOffsets
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
    , _dbEpochFileParser :: !(EpochFileParser e hash m (Word64, SlotNo))
    , _dbHashDecoder     :: !(forall s . Decoder s hash)
    , _dbHashEncoder     :: !(hash -> Encoding)
    , _dbTracer          :: !(Tracer m (TraceEvent e))
    }

data InternalState m hash h =
    DbClosed !(ClosedState m)
  | DbOpen !(OpenState m hash h)
  deriving (Generic, NoUnexpectedThunks)

dbIsOpen :: InternalState m hash h -> Bool
dbIsOpen (DbClosed _) = False
dbIsOpen (DbOpen _)   = True

-- | Internal state when the database is open.
data OpenState m hash h = OpenState
    { _currentEpoch            :: !EpochNo
    -- ^ The current 'EpochNo' the immutable store is writing to.
    , _currentEpochWriteHandle :: !(Handle h)
    -- ^ The write handle for the current epoch file.
    , _currentEpochOffsets     :: !SlotOffsets
    -- ^ The offsets to which blobs have been written in the current epoch
    -- file, stored from last to first.
    , _currentEBBHash          :: !(CurrentEBB hash)
    -- ^ The hash of the EBB of the current epoch that must be appended to the
    -- index file when finalising the current epoch.
    , _currentTip              :: !ImmTip
    -- ^ The current tip of the database.
    , _epochInfo               :: !(EpochInfo m)
    -- ^ Function to get the size of an epoch.
    , _nextIteratorID          :: !BaseIteratorID
    -- ^ The ID of the next iterator that will be created.
    }
  deriving (Generic, NoUnexpectedThunks)

-- | Internal state when the database is closed. This contains data that
-- should be restored when the database is reopened. Data not present here
-- will be recovered when reopening.
data ClosedState m = ClosedState
    { _closedEpochInfo      :: !(EpochInfo m)
    -- ^ See '_epochInfo'.
    , _closedNextIteratorID :: !BaseIteratorID
    -- ^ See '_nextIteratorID'.
    }
  deriving (Generic, NoUnexpectedThunks)

{------------------------------------------------------------------------------
  State helpers
------------------------------------------------------------------------------}

-- | Create the internal open state based on an epoch with the given 'Index'.
--
-- Open the epoch file for appending.
mkOpenState :: (HasCallStack, MonadThrow m)
            => HasFS m h
            -> EpochNo
            -> EpochInfo m
            -> BaseIteratorID
            -> ImmTip
            -> Index hash
            -> m (OpenState m hash h)
mkOpenState HasFS{..} epoch epochInfo nextIteratorID tip index = do
    let epochFile     = renderFile "epoch" epoch
        epochOffsets  = indexToSlotOffsets index

    eHnd <- hOpen epochFile (AppendMode AllowExisting)

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _currentEBBHash          = getEBBHash index
      , _currentTip              = tip
      , _epochInfo               = epochInfo
      , _nextIteratorID          = nextIteratorID
      }

-- | Create the internal open state for a new empty epoch.
--
-- Create the epoch file for appending.
mkOpenStateNewEpoch :: (HasCallStack, MonadThrow m)
                    => HasFS m h
                    -> EpochNo
                    -> EpochInfo m
                    -> BaseIteratorID
                    -> ImmTip
                    -> m (OpenState m hash h)
mkOpenStateNewEpoch HasFS{..} epoch epochInfo nextIteratorID tip = do
    let epochFile    = renderFile "epoch" epoch
        epochOffsets = SlotOffsets.empty

    eHnd <- hOpen epochFile (AppendMode MustBeNew)

    return OpenState
      { _currentEpoch            = epoch
      , _currentEpochWriteHandle = eHnd
      , _currentEpochOffsets     = epochOffsets
      , _currentEBBHash          = NoCurrentEBB
      , _currentTip              = tip
      , _epochInfo               = epochInfo
      , _nextIteratorID          = nextIteratorID
      }

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
--
-- TODO(adn): we should really just use 'Control.Concurrent.MVar.MVar' rather
-- than 'TMVar', but we currently don't have a simulator for code using
-- @MVar@.
modifyOpenState :: forall m hash r. (HasCallStack, IOLike m)
                => ImmutableDBEnv m hash
                -> (forall h. HasFS m h -> StateT (OpenState m hash h) m r)
                -> m r
modifyOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
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
    close !st ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> putMVar _dbInternalState st
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        let !cst = closedStateFromInternalState st
        putMVar _dbInternalState (DbClosed cst)
        closeOpenHandles st
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (_, ost)) ->
        putMVar _dbInternalState (DbOpen ost)
      -- In case of an error (not an exception)
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- When unexpected, close the DB for safety
        let !cst = closedStateFromInternalState st
        putMVar _dbInternalState (DbClosed cst)
        closeOpenHandles st
      ExitCaseSuccess (Left (UserError {})) ->
        -- When a user error, just restore the previous state
        putMVar _dbInternalState st

    mutation :: HasCallStack
             => InternalState m hash h
             -> m (r, OpenState m hash h)
    mutation (DbClosed _) = throwUserError _dbErr ClosedDBError
    mutation (DbOpen ost) = runStateT (action hasFS) ost

    -- TODO what if this fails?
    closeOpenHandles :: InternalState m hash h -> m ()
    closeOpenHandles (DbClosed _)            = return ()
    closeOpenHandles (DbOpen OpenState {..}) = hClose _currentEpochWriteHandle

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
withOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
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
      ExitCaseAbort         -> return ()
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        st <- updateMVar _dbInternalState $ \st ->
                (DbClosed (closedStateFromInternalState st), st)
        closeOpenHandles st
      ExitCaseSuccess (Right _) -> return ()
      -- In case of an ImmutableDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- We need to get the most recent state because it might have changed
        -- behind our back
        st <- updateMVar _dbInternalState $ \st ->
                (DbClosed (closedStateFromInternalState st), st)
        closeOpenHandles st
      ExitCaseSuccess (Left (UserError {})) -> return ()

    access :: HasCallStack
           => InternalState m hash h
           -> m r
    access (DbClosed _) = throwUserError _dbErr ClosedDBError
    access (DbOpen ost) = action hasFS ost

    -- TODO what if this fails?
    closeOpenHandles :: InternalState m hash h -> m ()
    closeOpenHandles (DbClosed _)            = return ()
    closeOpenHandles (DbOpen OpenState {..}) = hClose _currentEpochWriteHandle


-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: InternalState m hash h -> ClosedState m
closedStateFromInternalState (DbClosed cst) = cst
closedStateFromInternalState (DbOpen OpenState {..}) = ClosedState
  { _closedEpochInfo       = _epochInfo
  , _closedNextIteratorID  = _nextIteratorID
  }
