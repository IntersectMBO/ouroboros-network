{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | This module allows the management of a multiple Async jobs which
-- are grouped by an 'Ord group => group' type.
--
module Control.Concurrent.JobPool
  ( JobPool
  , Job (..)
  , withJobPool
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

import Control.Concurrent (getNumCapabilities)
import System.IO.Unsafe (unsafePerformIO)

-- | JobPool allows to submit asynchronous jobs, wait for their completion or
-- cancel.  Jobs are grouped, each group can be cancelled separately.
--
data JobPool group m a = JobPool {
       jobsVar         :: !(TVar m (Map (group, ThreadId m) (Async m ()))),
       completionQueue :: !(TQueue m a)
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
            => (JobPool group m a -> m b) -> m b
withJobPool =
    bracket create close
  where
    create :: m (JobPool group m a)
    create =
      atomically $
        JobPool <$> (newTVar Map.empty >>= \v -> labelTVar v "job-pool" $> v)
                <*> newTQueue

    -- 'bracket' requires that the 'close' callback is uninterruptible.  Note
    -- also that 'async' library is using 'uninterruptibleClose' in
    -- 'withAsync' combinator.  This can only deadlock if the threads in
    -- 'JobPool' got deadlocked so that the asynchronous exception cannot be
    -- delivered, e.g. deadlock in an ffi call or a tight loop which does not
    -- allocate (which is not a deadlock per se, but rather a rare unfortunate
    -- condition).
    close :: JobPool group m a -> m ()
    close JobPool{jobsVar} = do
      jobs <- readTVarIO jobsVar
      mapM_ uninterruptibleCancel jobs


forkJob' :: forall group m a.
            ( MonadAsync m, MonadMask m
            , Ord group
            )
         => (m () -> m (Async m ()))
         -> JobPool group m a
         -> Job     group m a
         -> m ()
forkJob' doFork JobPool{jobsVar, completionQueue} (Job action handler group label) =
    mask $ \restore -> do
      jobAsync <- doFork $ do
        tid <- myThreadId
        io tid restore
          `onException`
          atomically (modifyTVar' jobsVar (Map.delete (group, tid)))
        atomically (modifyTVar' jobsVar (Map.delete (group, tid)))

      let !tid = asyncThreadId jobAsync
      atomically $ modifyTVar' jobsVar (Map.insert (group, tid) $! jobAsync)
      return ()
  where
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
      !res <- handleJust notAsyncExceptions handler $
              restore action
      atomically $ writeTQueue completionQueue res



forkJob :: forall group m a.
           ( MonadAsync m, MonadMask m
           , Ord group
           )
        => JobPool group m a
        -> Job     group m a
        -> m ()
forkJob = forkJob' async

forkJobOn :: forall group m a.
             ( MonadAsync m, MonadMask m
             , Ord group
             )
          => Int
          -> JobPool group m a
          -> Job     group m a
          -> m ()
forkJobOn c = forkJob' (asyncOn limitCapability)
  where
    limitCapability :: Int
    limitCapability =
      let sysCap = unsafePerformIO getNumCapabilities in
      c `mod` (max 1 $ sysCap - 2)


readSize :: MonadSTM m => JobPool group m a -> STM m Int
readSize JobPool{jobsVar} = Map.size <$> readTVar jobsVar

readGroupSize :: ( MonadSTM m
                 , Eq group
                 )
              => JobPool group m a -> group -> STM m Int
readGroupSize JobPool{jobsVar} group =
      Map.size
    . Map.filterWithKey (\(group', _) _ -> group' == group)
  <$> readTVar jobsVar

-- | Wait for next successfully completed job.  Unlike 'wait' it will not throw
-- if a job errors.
--
waitForJob :: MonadSTM m => JobPool group m a -> STM m a
waitForJob JobPool{completionQueue} = readTQueue completionQueue

-- | Cancel all threads in a given group.  Blocks until all threads terminated.
--
cancelGroup :: ( MonadAsync m
               , Eq group
               )
            => JobPool group m a -> group -> m ()
cancelGroup JobPool { jobsVar } group = do
    jobs <- readTVarIO jobsVar
    void $ Map.traverseWithKey
             (\(group', _) thread ->
                when (group' == group) $
                  cancel thread
             )
             jobs


