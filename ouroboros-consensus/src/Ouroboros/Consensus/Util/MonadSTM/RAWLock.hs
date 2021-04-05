{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- | A Read-Append-Write (RAW) lock
--
-- Intended for qualified import
module Ouroboros.Consensus.Util.MonadSTM.RAWLock (
    -- * Public API
    RAWLock
  , new
  , poison
  , read
  , withAppendAccess
  , withReadAccess
  , withWriteAccess
    -- * Exposed internals: non-bracketed acquire & release
  , unsafeAcquireAppendAccess
  , unsafeAcquireReadAccess
  , unsafeAcquireWriteAccess
  , unsafeReleaseAppendAccess
  , unsafeReleaseReadAccess
  , unsafeReleaseWriteAccess
  ) where

import           Prelude hiding (read)

import           Control.Monad.Except
import           Data.Functor (($>))
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack)
import           NoThunks.Class (AllowThunk (..))

import           Ouroboros.Consensus.Util.IOLike

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | A Read-Append-Write (RAW) lock
--
-- A RAW lock allows multiple concurrent readers, at most one appender, which
-- is allowed to run concurrently with the readers, and at most one writer,
-- which has exclusive access to the lock.
--
-- The following table summarises which roles are allowed to concurrently
-- access the RAW lock:
--
-- >          │ Reader │ Appender │ Writer │
-- > ─────────┼────────┼──────────┼────────┤
-- > Reader   │   V    │     V    │    X   │
-- > Appender │░░░░░░░░│     X    │    X   │
-- > Writer   │░░░░░░░░│░░░░░░░░░░│    X   │
--
-- It is important to realise that a RAW lock is intended to control access to
-- a piece of in-memory state that should remain in sync with some other state
-- that can only be modified using side-effects, e.g., the file system. If,
-- for example, you're only maintaining a counter shared by threads, then
-- simply use a 'TVar' or an 'MVar'.
--
-- = Example use case: log files
--
-- A RAW lock is useful, for example, to maintain an in-memory index of log
-- files stored on disk.
--
-- * To read data from a log file, you need \"read\" access to the index to
--   find out the file and offset where the requested piece of data is stored.
--   While holding the RAW lock as a reader, you can perform the IO operation
--   to read the data from the right log file. This can safely happen
--   concurrently with other read operations.
--
-- * To append data to the current log file, you need \"append\" access to the
--   index so you can append an entry to the index and even to add a new log
--   file to the index when necessary. While holding the RAW lock as an
--   appender, you can perform the IO operation to append the piece of data to
--   the current log file and, if necessary start a new log file. Only one
--   append can happen concurrently. However, reads can safely happen
--   concurrently with appends. Note that the in-memory index is only updated
--   /after/ writing to disk.
--
-- * To remove the oldest log files, you need \"write\" access to the index,
--   so you can remove files from the index. While holding the RAW lock as a
--   writer, you can perform the IO operations to delete the oldest log files.
--   No other operations can run concurrently with this operation: concurrent
--   reads might try to read from deleted files and a concurrent append could
--   try to append to a deleted file.
--
-- = Analogy: Chicken coop
--
-- Think of readers as chickens, the appender as the rooster, and the writer
-- as the fox. All of them want access to the chicken coop, i.e., the state
-- protected by the RAW lock.
--
-- We can allow multiple chickens (readers) together in the chicken coop, they
-- get along (reasonably) fine. We can also let one rooster (appender) in, but
-- not more than one, otherwise he would start fighting with the other rooster
-- (conflict with the other appender). We can only let the fox in when all
-- chickens and the rooster (if present) have left the chicken coop, otherwise
-- the fox would eat them (conflict with the appender and invalidate the
-- results of readers, e.g, closing resources readers try to access).
--
-- = Usage
--
-- To use the lock, use any of the three following operations:
--
-- * 'withReadAccess'
-- * 'withAppendAccess'
-- * 'withWriteAccess'
--
-- If the standard bracketing the above three operations use doesn't suffice,
-- use the following three acquire-release pairs:
--
-- * 'unsafeAcquireReadAccess'   & 'unsafeReleaseReadAccess'
-- * 'unsafeAcquireAppendAccess' & 'unsafeReleaseAppendAccess'
-- * 'unsafeAcquireWriteAccess'  & 'unsafeReleaseWriteAccess'
--
-- NOTE: an acquire __must__ be followed by the corresponding release,
-- otherwise the correctness of the lock is not guaranteed and a dead-lock can
-- happen.
--
-- NOTE: nested locking of the same lock is not allowed, as you might be
-- blocked on yourself.
--
-- = Notes
--
-- * Only use a RAW lock when it is safe to concurrently read and append.
--
-- * We do not guarantee fairness for appenders and writers. They will race
--   for access each time the RAW lock changes.
--
-- * When you have many writers and/or very frequent writes, readers and
--   appenders will starve. You could say we have \"unfairness\", as writers
--   win over readers and appenders. A RAW lock will not be the best fit in
--   such a scenario.
--
-- * When you have no writers and you only need a read-append lock, consider
--   using a @StrictMVar@ instead. The \"stale\" state can be used by the
--   readers.
--
-- * The state @st@ is always evaluated to WHNF and is subject to the
--   'NoThunks' check when enabled.
--
-- * All public functions are exception-safe.
--
newtype RAWLock m st = RAWLock (StrictTVar m (RAWState st))

-- | Create a new 'RAWLock'
new :: (IOLike m, NoThunks st) => st -> m (RAWLock m st)
new st = RAWLock <$> newTVarIO (emptyRAWState st)

-- | Access the state stored in the 'RAWLock' as a reader.
--
-- Will block when there is a writer or when a writer is waiting to take the
-- lock.
withReadAccess :: forall m st a. IOLike m => RAWLock m st -> (st -> m a) -> m a
withReadAccess rawLock =
    bracket
      (atomically $ unsafeAcquireReadAccess rawLock)
      (const (atomically $ unsafeReleaseReadAccess rawLock))

-- | Access the state stored in the 'RAWLock' as an appender.
--
-- NOTE: it must be safe to run the given append action concurrently with
-- readers.
--
-- Will block when there is another appender, a writer, or when a writer is
-- waiting to take the lock.
withAppendAccess
  :: forall m st a. IOLike m => RAWLock m st -> (st -> m (st, a)) -> m a
withAppendAccess rawLock k = snd . fst <$>
    generalBracket
      (atomically $ unsafeAcquireAppendAccess rawLock)
      (\acquiredSt exitCase ->
        atomically $ unsafeReleaseAppendAccess
          rawLock
          (stateToPutBack acquiredSt exitCase))
      k

-- | Access the state stored in the 'RAWLock' as a writer.
--
-- Will block when there is another writer or while there are readers and/or
-- an appender.
withWriteAccess
  :: forall m st a. IOLike m => RAWLock m st -> (st -> m (st, a)) -> m a
withWriteAccess rawLock k = snd . fst <$>
    generalBracket
      (unsafeAcquireWriteAccess rawLock)
      (\acquiredSt exitCase ->
        unsafeReleaseWriteAccess
          rawLock
          (stateToPutBack acquiredSt exitCase))
      k

-- | Internal helper
stateToPutBack
  :: st  -- ^ Acquired state
  -> ExitCase (st, a)
     -- ^ Result of 'generalBracket', containing the modified state in case of
     -- success
  -> st
stateToPutBack acquiredSt = \case
    ExitCaseSuccess (modifiedSt, _a) -> modifiedSt
    ExitCaseException _ex            -> acquiredSt
    ExitCaseAbort                    -> acquiredSt

-- | Read the contents of the 'RAWLock' in an STM transaction.
--
-- Will retry when there is a writer.
--
-- In contrast to 'withReadAccess', this transaction will succeed when there
-- is a writer waiting to write, as there is no IO-operation during which the
-- lock must be held.
read :: IOLike m => RAWLock m st -> STM m st
read (RAWLock var) = readTVar var >>= \case
    ReadAppend     _readers _appender st -> return st
    WaitingToWrite _readers _appender st -> return st
    Writing                              -> retry
    Poisoned       (AllowThunk ex)       -> throwSTM ex

-- | Poison the lock with the given exception. All subsequent access to the
-- lock will result in the given exception being thrown.
--
-- Unless the lock has already been poisoned, in which case the original
-- exception with which the lock was poisoned will be thrown.
poison
  :: (IOLike m, Exception e, HasCallStack)
  => RAWLock m st -> (CallStack -> e) -> m (Maybe st)
poison (RAWLock var) mkEx = atomically $ do
    rawSt <- readTVar var
    (rawSt', mbSt) <-
      withPoisoned (poisonPure (toException (mkEx callStack)) rawSt)
    writeTVar var rawSt'
    return mbSt

{-------------------------------------------------------------------------------
  Exposed internals: non-bracketed acquire & release
-------------------------------------------------------------------------------}

withPoisoned :: MonadThrow m => Except SomeException a -> m a
withPoisoned = either throwIO return . runExcept

-- | Acquire the 'RAWLock' as a reader.
--
-- Will block when there is a writer or when a writer is waiting to take the
-- lock.
--
-- Composable with other 'STM' transactions.
--
-- NOTE: __must__ be followed by a call to 'unsafeReleaseReadAccess'.
unsafeAcquireReadAccess :: IOLike m => RAWLock m st -> STM m st
unsafeAcquireReadAccess (RAWLock var) = do
    rawSt <- readTVar var
    withPoisoned (acquireReadAccessPure rawSt) >>= \case
      Nothing           -> retry
      Just (rawSt', st) -> writeTVar var rawSt' $> st

-- | Release the 'RAWLock' as a reader.
--
-- Doesn't block.
--
-- Composable with other 'STM' transactions.
--
-- NOTE: __must__ be preceded by a call to 'unsafeAcquireReadAccess'.
unsafeReleaseReadAccess :: IOLike m => RAWLock m st -> STM m ()
unsafeReleaseReadAccess (RAWLock var) = do
    rawSt <- readTVar var
    withPoisoned (releaseReadAccessPure rawSt) >>= writeTVar var

-- | Access the state stored in the 'RAWLock' as an appender.
--
-- Will block when there is another appender, a writer, or when a writer is
-- waiting to take the lock.
--
-- Composable with other 'STM' transactions.
--
-- NOTE: __must__ be followed by a call to 'unsafeReleaseAppendAccess'.
unsafeAcquireAppendAccess :: IOLike m => RAWLock m st -> STM m st
unsafeAcquireAppendAccess (RAWLock var) = do
    rawSt <- readTVar var
    withPoisoned (acquireAppendAccessPure rawSt) >>= \case
      Nothing           -> retry
      Just (rawSt', st) -> writeTVar var rawSt' $> st

-- | Release the 'RAWLock' as an appender.
--
-- Doesn't block.
--
-- Composable with other 'STM' transactions.
--
-- NOTE: __must__ be preceded by a call to 'unsafeAcquireAppendAccess'.
unsafeReleaseAppendAccess
  :: IOLike m
  => RAWLock m st
  -> st  -- ^ State to store in the lock
  -> STM m ()
unsafeReleaseAppendAccess (RAWLock var) st = do
    rawSt <- readTVar var
    withPoisoned (releaseAppendAccessPure st rawSt) >>= writeTVar var

-- | Access the state stored in the 'RAWLock' as a writer.
--
-- Will block when there is another writer or while there are readers and\/or
-- an appender.
--
-- Does /not/ compose with other 'STM' transactions.
--
-- NOTE: __must__ be followed by a call to 'unsafeReleaseWriteAccess'.
unsafeAcquireWriteAccess :: IOLike m => RAWLock m st -> m st
unsafeAcquireWriteAccess rawLock@(RAWLock var) = join $ atomically $ do
    rawSt <- readTVar var
    withPoisoned (acquireWriteAccessPure rawSt) >>= \case
      Nothing             -> retry
      Just (rawSt', mbSt) -> do
        writeTVar var rawSt'
        -- We must update the value in the var, but we may or may not have
        -- obtained the @st@ in it. We must commit the write either way.
        case mbSt of
          Just st -> return $ return st
          -- Return a continuation that tries to acquire again
          Nothing -> return $ unsafeAcquireWriteAccess rawLock

-- | Release the 'RAWLock' as a writer.
--
-- Doesn't block.
--
-- Does /not/ compose with other 'STM' transactions.
--
-- NOTE: __must__ be preceded by a call to 'unsafeAcquireWriteAccess'.
unsafeReleaseWriteAccess
  :: IOLike m
  => RAWLock m st
  -> st  -- ^ State to store in the lock
  -> m ()
unsafeReleaseWriteAccess (RAWLock var) st = atomically $ do
    rawSt <- readTVar var
    withPoisoned (releaseWriteAccessPure st rawSt) >>= writeTVar var

{-------------------------------------------------------------------------------
  Pure internals
-------------------------------------------------------------------------------}

-- | Any non-negative number of readers
newtype Readers = Readers Word
  deriving newtype (Eq, Ord, Enum, Num, NoThunks)

-- | At most one appender
data Appender = NoAppender | Appender
  deriving (Generic, NoThunks)

-- | The lock is implemented by a single 'StrictTVar', which stores a
-- 'RAWState'.
data RAWState st =
    -- | Reading and/or appending is happening.
    ReadAppend     !Readers !Appender !st

    -- | A writer (or more than one) has arrived. No new readers or a new
    -- appender are allowed, they can only release, not acquire.
    --
    -- When the number of readers is 0 and there is no more appender, a writer
    -- (multiple writers can race for this) will be able to get exclusive
    -- access and will change the state to 'Writing'.
  | WaitingToWrite !Readers !Appender !st

    -- | No (more) readers or appender, the writer has exclusive access.
  | Writing

    -- | The lock has been poisoned: all subsequent acquires or releases will
    -- throw the stored exception.
  | Poisoned       !(AllowThunk SomeException)
  deriving (Generic, NoThunks)

-- | Create an initial, empty, unlocked 'RAWState': no readers, no appender,
-- no writer (waiting).
emptyRAWState :: st -> RAWState st
emptyRAWState = ReadAppend (Readers 0) NoAppender

{-------------------------------------------------------------------------------
  Pure internals: transitions between the 'RAWState's
-------------------------------------------------------------------------------}

acquireReadAccessPure
  :: RAWState st -> Except SomeException (Maybe (RAWState st, st))
acquireReadAccessPure = \case
    ReadAppend readers appender st
      -> return $ Just (ReadAppend (succ readers) appender st, st)
    WaitingToWrite {}
      -> return Nothing
    Writing
      -> return Nothing
    Poisoned (AllowThunk ex)
      -> throwError ex

releaseReadAccessPure
  :: RAWState st -> Except SomeException (RAWState st)
releaseReadAccessPure = \case
    ReadAppend readers appender st
      | 0 <- readers
      -> error "releasing a reader without outstanding readers in ReadAppend"
      | otherwise
      -> return $ ReadAppend (pred readers) appender st
    WaitingToWrite readers appender st
      | 0 <- readers
      -> error "releasing a reader without outstanding readers in WaitingToWrite"
      | otherwise
      -> return $ WaitingToWrite (pred readers) appender st
    Writing
      -> error "releasing a reader without outstanding readers in Writing"
    Poisoned (AllowThunk ex)
      -> throwError ex

acquireAppendAccessPure
  :: RAWState st -> Except SomeException (Maybe (RAWState st, st))
acquireAppendAccessPure = \case
    ReadAppend readers appender st
      | NoAppender <- appender
      -> return $ Just (ReadAppend readers Appender st, st)
      | otherwise
      -> return Nothing
    WaitingToWrite {}
      -> return Nothing
    Writing
      -> return Nothing
    Poisoned (AllowThunk ex)
      -> throwError ex

releaseAppendAccessPure
  :: st -> RAWState st -> Except SomeException (RAWState st)
releaseAppendAccessPure st' = \case
    ReadAppend readers appender _st
      | NoAppender <- appender
      -> error "releasing an appender without an outstanding appender in ReadAppend"
      | otherwise
      -> return $ ReadAppend readers NoAppender st'
    WaitingToWrite readers appender _st
      | NoAppender <- appender
      -> error "releasing an appender without an outstanding appender in WaitingToWrite"
      | otherwise
      -> return $ WaitingToWrite readers NoAppender st'
    Writing
      -> error "releasing an appender without an outstanding appender in Writing"
    Poisoned (AllowThunk ex)
      -> throwError ex

acquireWriteAccessPure
  :: RAWState st -> Except SomeException (Maybe (RAWState st, Maybe st))
acquireWriteAccessPure = \case
    -- When there are no readers or appender in the 'ReadAppend' we can
    -- directly go to the 'Writing' state, if not, we'll go to the
    -- intermediary 'WaitingToWrite' state until they have all released.
    ReadAppend readers appender st
      | 0 <- readers
      , NoAppender <- appender
      -> return $ Just (Writing, Just st)
      | otherwise
      -> return $ Just (WaitingToWrite readers appender st, Nothing)
    WaitingToWrite readers appender st
      | 0 <- readers
      , NoAppender <- appender
      -> return $ Just (Writing, Just st)
      | otherwise
      -> return Nothing
    Writing
      -> return Nothing
    Poisoned (AllowThunk ex)
      -> throwError ex

releaseWriteAccessPure
  :: st -> RAWState st -> Except SomeException (RAWState st)
releaseWriteAccessPure st' = \case
    ReadAppend _readers _appender _st
      -> error "releasing a writer in ReadAppend"
    WaitingToWrite _readers _appender _st
      -> error "releasing a writer in WaitingToWrite"
    Writing
      -> return $ emptyRAWState st'
    Poisoned (AllowThunk ex)
      -> throwError ex

poisonPure
  :: SomeException -> RAWState st -> Except SomeException (RAWState st, Maybe st)
poisonPure ex = \case
    ReadAppend _readers _appender st
      -> return (Poisoned (AllowThunk ex), Just st)
    WaitingToWrite _readers _appender st
      -> return (Poisoned (AllowThunk ex), Just st)
    Writing
      -> return (Poisoned (AllowThunk ex), Nothing)
    Poisoned (AllowThunk prevEx)
      -> throwError prevEx
