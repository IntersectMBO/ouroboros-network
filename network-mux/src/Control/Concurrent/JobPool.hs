{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeData            #-}


-- | This module allows the management of a multiple Async jobs which
-- are grouped by an 'Ord group => group' type.
--
module Control.Concurrent.JobPool
  ( JobPool
  , HasQueue (..)
  , Job (..)
  , withJobPool
  , withJobPool_
  , forkJob
  , forkJobOn
  , readSize
  , readGroupSize
  , waitForJob
  , cancelGroup
  ) where

import Data.Functor (($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Control.Concurrent.Class.MonadSTM
import Control.Exception (SomeAsyncException (..))
import Control.Monad (void, when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (MonadThread (..))
import Control.Monad.Class.MonadThrow


-- | Whether a 'JobPool' was created with a completion queue.  Only a
-- 'WithQueue' pool can be passed to 'waitForJob'.
--
type data HasQueue = WithQueue | WithoutQueue

-- | A completion queue, indexed by whether it exists at all.  Matching on
-- this GADT is what makes 'waitForJob' total: at index 'WithQueue' the only
-- constructor is 'CompletionQueue'.
--
data CompletionQueue (q :: HasQueue) m a where
  CompletionQueue   :: !(TQueue m a) -> CompletionQueue WithQueue    m a
  NoCompletionQueue ::                  CompletionQueue WithoutQueue m a

-- | JobPool allows to submit asynchronous jobs, wait for their completion or
-- cancel.  Jobs are grouped, each group can be cancelled separately.
--
data JobPool (q :: HasQueue) group m a = JobPool {
       jobsVar         :: !(TVar m (Map (group, ThreadId m) (Async m ()))),
       completionQueue :: !(CompletionQueue q m a)
     }

-- | An asynchronous job which belongs to some group and its exception handler.
--
data Job group m a =
    Job (m a)                  -- ^ job
        (SomeException -> m a) -- ^ error handler
        group                  -- ^ job group
        String                 -- ^ thread label

withJobPool :: forall group m a b.
               (MonadAsync m, MonadThrow m, MonadLabelledSTM m)
            => (JobPool WithQueue group m a -> m b) -> m b
withJobPool =
    bracket create closeJobPool
  where
    create :: m (JobPool WithQueue group m a)
    create =
      atomically $
        JobPool <$> (newTVar Map.empty >>= \v -> labelTVar v "job-pool" $> v)
                <*> (CompletionQueue <$> newTQueue)

-- | Like 'withJobPool', but for a pool whose jobs' results nobody ever
-- inspects: no 'TQueue' is created, so there is nothing for 'forkJob'\/
-- 'forkJobOn' to write a finished job's result into, and nothing to
-- remember to drain.  'waitForJob' does not typecheck against a pool
-- created this way.
--
withJobPool_ :: forall group m a b.
                (MonadAsync m, MonadThrow m, MonadLabelledSTM m)
             => (JobPool WithoutQueue group m a -> m b) -> m b
withJobPool_ =
    bracket create closeJobPool
  where
    create :: m (JobPool WithoutQueue group m a)
    create =
      atomically $
        JobPool <$> (newTVar Map.empty >>= \v -> labelTVar v "job-pool" $> v)
                <*> pure NoCompletionQueue

-- 'bracket' requires that this callback is uninterruptible.  Note also that
-- 'async' library is using 'uninterruptibleCancel' in 'withAsync' combinator.
-- This can only deadlock if the threads in 'JobPool' got deadlocked so that
-- the asynchronous exception cannot be delivered, e.g. deadlock in an ffi
-- call or a tight loop which does not allocate (which is not a deadlock per
-- se, but rather a rare unfortunate condition).
closeJobPool :: (MonadAsync m, MonadThrow m)
             => JobPool q group m a -> m ()
closeJobPool JobPool{jobsVar} = do
  jobs <- readTVarIO jobsVar
  mapM_ uninterruptibleCancel jobs


forkJob' :: forall q group m a.
            ( MonadAsync m, MonadMask m
            , Ord group
            )
         => (((forall x. m x -> m x) -> m ()) -> m (Async m ()))
         -- ^ how to fork a thread, e.g. `async`, `asyncOn`.
         -> JobPool q group m a
         -> Job     group m a
         -> m ()
forkJob' doFork JobPool{jobsVar, completionQueue} (Job action handler group label) =
    mask_ do
      jobAsync <- doFork \restore -> do
        tid <- myThreadId
        io tid restore
          `onException`
          deregister tid
        deregister tid

      let !tid = asyncThreadId jobAsync
      atomically $ modifyTVar' jobsVar (Map.insert (group, tid) $! jobAsync)
      return ()
  where
    -- | Remove this job's own @(group, tid)@ entry from 'jobsVar', once it is
    -- actually there.  This can only block before the parent's insert, which
    -- is guaranteed to follow, since the parent is masked.
    --
    deregister :: ThreadId m -> m ()
    deregister tid =
      atomically $ do
        registered <- Map.member (group, tid) <$> readTVar jobsVar
        if registered
          then modifyTVar' jobsVar (Map.delete (group, tid))
          else retry

    notAsyncExceptions :: SomeException -> Maybe SomeException
    notAsyncExceptions e
      | Just (SomeAsyncException _) <- fromException e
                  = Nothing
      | otherwise = Just e

    io :: ThreadId m
       -> (forall x. m x -> m x)
       -> m ()
    io tid restore = do
      labelThread tid label
      -- NOTE: `network-mux` relies on not passing asynchronous exceptions to
      -- the exception handler, see `Network.Mux.miniProtocolJob`.
      !res <- handleJust notAsyncExceptions handler $
              restore action
      case completionQueue of
        NoCompletionQueue  -> pure ()
        CompletionQueue cq -> atomically $ writeTQueue cq res



-- | Fork a `Job` using `async`.
--
forkJob :: forall q group m a.
           ( MonadAsync m, MonadMask m
           , Ord group
           )
        => JobPool q group m a
        -> Job     group m a
        -> m ()
forkJob = forkJob' asyncWithUnmask


-- | Fork a `Job` using `asyncOn`.
--
forkJobOn :: forall q group m a.
             ( MonadAsync m, MonadMask m
             , Ord group
             )
          => Int
          -> JobPool q group m a
          -> Job     group m a
          -> m ()
forkJobOn cap = forkJob' (asyncOnWithUnmask cap)


readSize :: MonadSTM m => JobPool q group m a -> STM m Int
readSize JobPool{jobsVar} = Map.size <$> readTVar jobsVar

readGroupSize :: ( MonadSTM m
                 , Eq group
                 )
              => JobPool q group m a -> group -> STM m Int
readGroupSize JobPool{jobsVar} group =
      Map.size
    . Map.filterWithKey (\(group', _) _ -> group' == group)
  <$> readTVar jobsVar

-- | Wait for next successfully completed job.  Unlike 'wait' it will not throw
-- if a job errors.
--
waitForJob :: MonadSTM m => JobPool WithQueue group m a -> STM m a
waitForJob JobPool{completionQueue = CompletionQueue cq} = readTQueue cq

-- | Cancel all threads in a given group.  Blocks until all threads terminated.
--
cancelGroup :: ( MonadAsync m
               , Eq group
               )
            => JobPool q group m a -> group -> m ()
cancelGroup JobPool { jobsVar } group = do
    jobs <- readTVarIO jobsVar
    void $ Map.traverseWithKey
             (\(group', _) thread ->
                when (group' == group) $
                  cancel thread
             )
             jobs
