{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Ouroboros.Consensus.Storage.ImmutableDB.Impl.State
  ( -- * State types
    ImmutableDBEnv (..)
  , InternalState (..)
  , dbIsOpen
  , OpenState (..)
    -- * State helpers
  , mkOpenState
  , getOpenState
  , modifyOpenState
  , withOpenState
  , cleanUp
  ) where

import           Control.Monad.State.Strict
import           Control.Tracer (Tracer)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)

import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Consensus.BlockchainTime (BlockchainTime)
import           Ouroboros.Consensus.Util (SomePair (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry (ResourceRegistry,
                     allocate)

import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.FS.API
import           Ouroboros.Consensus.Storage.FS.API.Types
import           Ouroboros.Consensus.Storage.Util.ErrorHandling
                     (ErrorHandling (..))

import           Ouroboros.Consensus.Storage.ImmutableDB.ChunkSize
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
data ImmutableDBEnv m hash = forall h e. ImmutableDBEnv
    { _dbHasFS           :: !(HasFS m h)
    , _dbErr             :: !(ErrorHandling ImmutableDBError m)
    , _dbInternalState   :: !(StrictMVar m (InternalState m hash h))
    , _dbEpochFileParser :: !(EpochFileParser e m (BlockSummary hash) hash)
    , _dbChunkSize       :: !ChunkSize
    , _dbHashInfo        :: !(HashInfo hash)
    , _dbTracer          :: !(Tracer m (TraceEvent e hash))
    , _dbRegistry        :: !(ResourceRegistry m)
    , _dbCacheConfig     :: !Index.CacheConfig
    , _dbBlockchainTime  :: !(BlockchainTime m)
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
    , _currentTip             :: !(ImmTipWithInfo hash)
      -- ^ The current tip of the database.
    , _index                  :: !(Index m hash h)
      -- ^ An abstraction layer on top of the indices to allow for caching.
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
  -> ImmTipWithInfo hash
  -> AllowExisting
  -> m (OpenState m hash h)
mkOpenState registry HasFS{..} _err index epoch tip existing = do
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
       DbClosed         -> throwUserError _dbErr ClosedDBError
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

    open :: m (OpenState m hash h)
    -- TODO Is uninterruptibleMask_ absolutely necessary here?
    open = uninterruptibleMask_ $ takeMVar _dbInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> do
        putMVar _dbInternalState DbClosed
        throwUserError _dbErr ClosedDBError

    close :: OpenState m hash h
          -> ExitCase (Either ImmutableDBError (r, OpenState m hash h))
          -> m ()
    close !ost ec = do
        -- It is crucial to replace the MVar.
        putMVar _dbInternalState st'
        followUp
      where
        (st', followUp) = case ec of
          -- If we were interrupted, restore the original state.
          ExitCaseAbort                               -> (DbOpen ost, return ())
          ExitCaseException _ex                       -> (DbOpen ost, return ())
          -- In case of success, update to the newest state
          ExitCaseSuccess (Right (_, ost'))           -> (DbOpen ost', return ())
          -- In case of an unexpected error (not an exception), close the DB
          -- for safety
          ExitCaseSuccess (Left (UnexpectedError {})) -> (DbClosed, cleanUp hasFS ost)
          -- In case a user error, just restore the previous state
          ExitCaseSuccess (Left (UserError {}))       -> (DbOpen ost, return ())

    mutation :: OpenState m hash h -> m (r, OpenState m hash h)
    mutation = runStateT (action hasFS)

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

    open :: m (OpenState m hash h)
    open = readMVar _dbInternalState >>= \case
      DbOpen ost -> return ost
      DbClosed   -> throwUserError _dbErr ClosedDBError

    -- close doesn't take the state that @open@ returned, because the state
    -- may have been updated by someone else since we got it (remember we're
    -- using 'readMVar' here, 'takeMVar'). So we need to get the most recent
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
    shutDown = swapMVar _dbInternalState DbClosed >>= \case
      DbOpen ost -> cleanUp hasFS ost
      DbClosed   -> return ()

    access :: OpenState m hash h -> m r
    access = action hasFS

cleanUp :: Monad m => HasFS m h -> OpenState m hash h -> m ()
cleanUp HasFS { hClose } OpenState {..}  = do
    Index.close _index
    -- If one of the 'hClose' calls fails, the error will bubble up to the
    -- bracketed call to 'withRegistry', which will close the
    -- 'ResourceRegistry' and thus all the remaining handles in it.
    hClose _currentEpochHandle
    hClose _currentPrimaryHandle
    hClose _currentSecondaryHandle
