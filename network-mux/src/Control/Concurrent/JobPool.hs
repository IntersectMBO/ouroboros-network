{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Control.Concurrent.JobPool (
    JobPool,
    Job(..),
    withJobPool,
    forkJob,
    readSize,
    readGroupSize,
    collect,
    cancelGroup
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))

import           Control.Exception (SomeAsyncException (..))
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadThread (..))
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad (when)


data JobPool group m a = JobPool {
       jobsVar         :: !(TVar m (Map (group, ThreadId m) (Async m ()))),
       completionQueue :: !(TQueue m a)
     }

data Job group m a = Job (m a) (SomeException -> m a) group String

withJobPool :: forall group m a b.
               (MonadAsync m, MonadThrow m)
            => (JobPool group m a -> m b) -> m b
withJobPool =
    bracket create close
  where
    create :: m (JobPool group m a)
    create =
      atomically $
        JobPool <$> newTVar Map.empty
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
      jobs <- atomically (readTVar jobsVar)
      mapM_ uninterruptibleCancel jobs

forkJob :: forall group m a.
           ( MonadAsync m, MonadMask m
           , Ord group
           )
        => JobPool group m a
        -> Job     group m a
        -> m ()
forkJob JobPool{jobsVar, completionQueue} (Job action handler group label) =
    mask $ \restore -> do
      jobAsync <- async $ do
        tid <- myThreadId
        labelThread tid label
        !res <- handleJust notAsyncExceptions handler $
                 restore action
        atomically $ do
          writeTQueue completionQueue res
          modifyTVar' jobsVar (Map.delete (group, tid))

      let !tid = asyncThreadId (Proxy :: Proxy m) jobAsync
      atomically $ modifyTVar' jobsVar (Map.insert (group, tid) jobAsync)
      return ()
  where
    notAsyncExceptions :: SomeException -> Maybe SomeException
    notAsyncExceptions e
      | Just (SomeAsyncException _) <- fromException e
                  = Nothing
      | otherwise = Just e

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

collect :: MonadSTM m => JobPool group m a -> STM m a
collect JobPool{completionQueue} = readTQueue completionQueue

cancelGroup :: ( MonadAsync m
               , Eq group
               )
            => JobPool group m a -> group -> m ()
cancelGroup JobPool { jobsVar } group = do
    jobs <- atomically (readTVar jobsVar)
    _ <- Map.traverseWithKey (\(group', _) thread ->
                               when (group' == group) $
                                 cancel thread
                             )
                             jobs
    return ()


