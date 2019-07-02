{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wredundant-constraints #-}

-- | A thread registry can be used to spawn threads. You could compare it to a
-- non-scoped version of 'withAsync' augmented with subregistries.
module Ouroboros.Consensus.Util.ThreadRegistry
  ( ThreadRegistry
  , withThreadRegistry
  , fork
  , forkLinked
  , forkNonTerminating
  , withSubregistry
  , ExceptionInForkedThread (..)
  ) where

import           Control.Exception (asyncExceptionFromException,
                     asyncExceptionToException)
import           Control.Monad (forM, forM_, liftM)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           Data.Word (Word64)

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork hiding (fork)
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow hiding (handle)

{-------------------------------------------------------------------------------
  Public API
-------------------------------------------------------------------------------}

-- | A registry of threads, see 'withThreadRegistry'.
data ThreadRegistry m = ThreadRegistry
  { _registered       :: !(TVar m (Map RegisteredID (Registered m)))
    -- ^ The registered threads and subregistries.
    --
    -- The 'RegisteredID' is used to remove a terminated thread or a
    -- subregistry from this map.
  , _nextRegisteredID :: !(TVar m RegisteredID)
    -- ^ The value to use for the next 'RegisteredID' that will be added to
    -- '_registered'.
  }

-- | Create a new 'ThreadRegistry' and pass it to the supplied function.
--
-- Using the 'ThreadRegistry' you can:
--
-- * 'fork': spawn a new thread and get an 'Async' handle for the thread.
-- * 'forkLinked': same as 'fork' but any exception thrown in the thread will
--    be thrown to the thread that forked it.
-- * 'forkNonTerminating': same as 'forkLinked' but meant for non-terminating
--    actions.
-- * 'withSubregistry': create subregistries.
--
-- When the supplied continuation function returns or throws an exception, all
-- threads created using the 'ThreadRegistry', as well as all threads created
-- using subregistries created using the 'ThreadRegistry' will be terminated
-- using 'uninterruptibleCancel', similar to 'withAsync'.
withThreadRegistry :: (MonadAsync m, MonadThrow m)
                   => (ThreadRegistry m -> m a)
                   -> m a
withThreadRegistry = bracket (atomically new) cancelAll

-- | Fork a new thread.
--
-- You can use the returned 'Async' handle to wait on it and to obtain the
-- result or exception.
--
-- The thread registry will cancel the thread when the thread registry is shut
-- down.
fork :: (MonadAsync m, MonadMask m)
     => ThreadRegistry m
     -> m a  -- ^ The action to run in the thread
     -> m (Async m a)
fork threadRegistry action = mask $ \restore -> do
    -- We obtain the registeredID in a separate transaction, before the one
    -- that stores the handle in the map, because we need to know the
    -- registeredID in the action, which returns the actual handle.
    registeredID <- atomically $ getNextRegisteredID threadRegistry

    handle <- async $
      -- When the action terminates successfully or not, we deregister it as
      -- we will no longer need to cancel it.
      restore action `finally`
        atomically (deregister registeredID threadRegistry)

    atomically $ register registeredID (AsyncThread handle) threadRegistry

    return handle

-- | Same as 'fork', but any exception thrown in the forked thread will be
-- rethrown wrapped in an 'ExceptionInForkedThread' to the thread that called
-- this function, like 'link'.
forkLinked :: forall m a. (MonadAsync m, MonadFork m, MonadMask m)
           => ThreadRegistry m
           -> m a  -- ^ The action to run in the thread
           -> m (Async m a)
forkLinked threadRegistry action = do
    me <- myThreadId
    fork threadRegistry $
      action `catch` \e -> do
        case fromException e of
          Just AsyncCancelled{} -> return ()
          Nothing               -> throwTo me $ ExceptionInForkedThread e
        throwM e

-- | Variant of 'forkLinked' intented to fork a new thread that will run a
-- non-terminating action.
forkNonTerminating :: (MonadAsync m, MonadFork m, MonadMask m)
                   => ThreadRegistry m
                   -> m Void  -- ^ The non-terminating action to run in the
                              -- thread
                   -> m (Async m Void)
forkNonTerminating = forkLinked

-- | Create a subregistry.
--
-- Similar to 'withThreadRegistry'. The subregistry and all the threads it
-- forked will be terminated when the parent (or any ancestor) registry is
-- terminated.
withSubregistry :: forall m a. (MonadAsync m, MonadThrow m)
                => ThreadRegistry m
                -> (ThreadRegistry m -> m a)
                -> m a
withSubregistry parent k =
    bracket open close (\(_registeredID, child) -> k child)
  where
    open :: m (RegisteredID, ThreadRegistry m)
    open = atomically $ do
      registeredID <- getNextRegisteredID parent
      subregistry  <- new
      register registeredID (Subregistry subregistry) parent
      return (registeredID, subregistry)

    close :: (RegisteredID, ThreadRegistry m) -> m ()
    close (registeredID, child) = do
      cancelAll child
      atomically $ deregister registeredID parent

-- | An exception thrown by a thread forked using 'forkLinked' or
-- 'forkNonTerminating'.
data ExceptionInForkedThread = ExceptionInForkedThread SomeException
    deriving (Show)

instance Exception ExceptionInForkedThread where
  fromException = asyncExceptionFromException
  toException   = asyncExceptionToException

{-------------------------------------------------------------------------------
  Internals
-------------------------------------------------------------------------------}

type RegisteredID = Word64

data Registered m where
  AsyncThread :: Async m a        -> Registered m
  Subregistry :: ThreadRegistry m -> Registered m

data SomeAsync m where
  SomeAsync :: Async m a -> SomeAsync m

-- | Create a new 'ThreadRegistry'
new :: MonadSTM m => STM m (ThreadRegistry m)
new = ThreadRegistry <$> newTVar Map.empty <*> newTVar 0

-- | Get the value of '_nextRegisteredID' and increment it.
getNextRegisteredID :: MonadSTM m => ThreadRegistry m -> STM m RegisteredID
getNextRegisteredID ThreadRegistry { _nextRegisteredID } = do
    nextRegisteredID <- readTVar _nextRegisteredID
    modifyTVar' _nextRegisteredID succ
    return nextRegisteredID

register :: MonadSTM m
         => RegisteredID -> Registered m -> ThreadRegistry m -> STM m ()
register registeredID reg ThreadRegistry { _registered } =
    modifyTVar' _registered (Map.insertWith checkAbsence registeredID reg)
  where
    -- Only called when there is already an entry for this key in this map,
    -- which should never happen, unless 'RegisteredID' (a 'Word64')
    -- overflows, which is very unlikely.
    checkAbsence :: a -> a -> a
    checkAbsence _new _old = error $
      "impossible: RegisteredID " <> show registeredID <> " already registered"

deregister :: MonadSTM m => RegisteredID -> ThreadRegistry m -> STM m ()
deregister registeredID ThreadRegistry { _registered } =
    modifyTVar' _registered (Map.alter checkPresence registeredID)
  where
    checkPresence :: Maybe a -> Maybe a
    checkPresence (Just _) = Nothing
    checkPresence Nothing  = error $
      "impossible: RegisteredID " <> show registeredID <> " not registered"

-- | Cancel all threads in the given 'ThreadRegistry' and its subregistries
-- using 'uninterruptibleCancel'.
cancelAll :: MonadAsync m => ThreadRegistry m -> m ()
cancelAll threadRegistry = do
    as <- atomically $ getAllThreads threadRegistry
    forM_ as $ \(SomeAsync a) -> uninterruptibleCancel a

-- | Return all registered threads, including threads registered using
-- subregistries.
getAllThreads :: forall m. MonadSTM m
              => ThreadRegistry m -> STM m [SomeAsync m]
getAllThreads = go
  where
    go :: ThreadRegistry m -> STM m [SomeAsync m]
    go (ThreadRegistry { _registered }) = do
      registered <- readTVar _registered
      liftM concat $ forM registered $ \case
        AsyncThread a -> return [SomeAsync a]
        Subregistry r -> go r
