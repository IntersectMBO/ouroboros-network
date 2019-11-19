{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
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
  , closeOpenHandles
  , closeOpenStateHandles
  ) where

import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Control.Monad.Class.MonadThrow hiding (onException)

import           Ouroboros.Network.Point (WithOrigin)

import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike

import           Ouroboros.Storage.Common
import           Ouroboros.Storage.EpochInfo
import           Ouroboros.Storage.FS.API
import           Ouroboros.Storage.FS.API.Types
import           Ouroboros.Storage.Util.ErrorHandling (ErrorHandling (..))
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Ouroboros.Storage.ImmutableDB.Impl.Index.Primary
                     (SecondaryOffset)
import qualified Ouroboros.Storage.ImmutableDB.Impl.Index.Primary as Primary
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
    , _dbInternalState   :: !(StrictMVar m (InternalState hash h))
    , _dbEpochFileParser :: !(EpochFileParser e m (Secondary.Entry hash, WithOrigin hash))
    , _dbEpochInfo       :: !(EpochInfo m)
    , _dbHashInfo        :: !(HashInfo hash)
    , _dbTracer          :: !(Tracer m (TraceEvent e hash))
    }

data InternalState hash h =
    DbClosed !ClosedState
  | DbOpen !(OpenState hash h)
  deriving (Generic, NoUnexpectedThunks)

dbIsOpen :: InternalState hash h -> Bool
dbIsOpen (DbClosed _) = False
dbIsOpen (DbOpen _)   = True

-- | Internal state when the database is open.
data OpenState hash h = OpenState
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
  :: forall m hash h. (HasCallStack, MonadThrow m)
  => HasFS m h
  -> EpochNo
  -> BaseIteratorID
  -> ImmTipWithHash hash
  -> AllowExisting
  -> m (OpenState hash h)
mkOpenState hasFS@HasFS{..} epoch nextIteratorID tip existing =
    -- TODO use a ResourceRegistry for this
    closeOnException     (hOpen (renderFile "epoch" epoch)     appendMode) $ \eHnd ->
      closeOnException   (Primary.open hasFS epoch existing)               $ \pHnd ->
        closeOnException (hOpen (renderFile "secondary" epoch) appendMode) $ \sHnd -> do
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
            }
  where
    appendMode = AppendMode existing

    -- | Open the handle and run a function using that handle, but if it
    -- throws an 'FsError', close the handle.
    closeOnException :: m (Handle h) -> (Handle h -> m a) -> m a
    closeOnException open k = open >>= \h ->
      EH.onException hasFsErr (k h) (hClose h)

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
             -> m (SomePair (HasFS m) (OpenState hash))
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
                -> (forall h. HasFS m h -> StateT (OpenState hash h) m r)
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

    open :: m (InternalState hash h)
    open = takeMVar _dbInternalState

    close :: InternalState hash h
          -> ExitCase (Either ImmutableDBError (r, OpenState hash h))
          -> m ()
    close !st ec = case ec of
      -- Restore the original state in case of an abort
      ExitCaseAbort         -> putMVar _dbInternalState st
      -- In case of an exception, close the DB for safety.
      ExitCaseException _ex -> do
        let !cst = closedStateFromInternalState st
        putMVar _dbInternalState (DbClosed cst)
        closeOpenHandles hasFS st
      -- In case of success, update to the newest state
      ExitCaseSuccess (Right (_, ost)) ->
        putMVar _dbInternalState (DbOpen ost)
      -- In case of an error (not an exception)
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- When unexpected, close the DB for safety
        let !cst = closedStateFromInternalState st
        putMVar _dbInternalState (DbClosed cst)
        closeOpenHandles hasFS st
      ExitCaseSuccess (Left (UserError {})) ->
        -- When a user error, just restore the previous state
        putMVar _dbInternalState st

    mutation :: HasCallStack
             => InternalState hash h
             -> m (r, OpenState hash h)
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
              -> (forall h. HasFS m h -> OpenState hash h -> m r)
              -> m r
withOpenState ImmutableDBEnv {_dbHasFS = hasFS :: HasFS m h, ..} action = do
    (mr, ()) <- generalBracket open (const close) (tryImmDB hasFsErr _dbErr . access)
    case mr of
      Left  e -> throwError e
      Right r -> return r
  where
    HasFS{..}         = hasFS
    ErrorHandling{..} = _dbErr

    open :: m (InternalState hash h)
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
        closeOpenHandles hasFS st
      ExitCaseSuccess (Right _) -> return ()
      -- In case of an ImmutableDBError, close when unexpected
      ExitCaseSuccess (Left (UnexpectedError {})) -> do
        -- We need to get the most recent state because it might have changed
        -- behind our back
        st <- updateMVar _dbInternalState $ \st ->
                (DbClosed (closedStateFromInternalState st), st)
        closeOpenHandles hasFS st
      ExitCaseSuccess (Left (UserError {})) -> return ()

    access :: HasCallStack
           => InternalState hash h
           -> m r
    access (DbClosed _) = throwUserError _dbErr ClosedDBError
    access (DbOpen ost) = action hasFS ost

closeOpenHandles :: Monad m => HasFS m h -> InternalState hash h -> m ()
closeOpenHandles hasFS = \case
    DbClosed _       -> return ()
    DbOpen openState -> closeOpenStateHandles hasFS openState

-- TODO what if this fails? Use a 'ResourceRegistry'?
closeOpenStateHandles :: Monad m => HasFS m h -> OpenState hash h -> m ()
closeOpenStateHandles HasFS { hClose } OpenState {..}  = do
    hClose _currentEpochHandle
    hClose _currentPrimaryHandle
    hClose _currentSecondaryHandle

-- | Create a 'ClosedState' from an internal state, open or closed.
closedStateFromInternalState :: InternalState hash h -> ClosedState
closedStateFromInternalState (DbClosed cst) = cst
closedStateFromInternalState (DbOpen OpenState {..}) = ClosedState
    { _closedNextIteratorID  = _nextIteratorID
    }
