{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}


module Network.Mux.JobPool (
    JobPool,
    Job(..),
    withJobPool,
    forkJob,
    readSize,
    collect
  ) where

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy(..))

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork (MonadThread(..))
import           Control.Monad.Class.MonadThrow
import           Control.Exception (SomeException, SomeAsyncException(..))


data JobPool m a = JobPool {
       jobsVar         :: !(TVar m (Map (ThreadId m) (Async m ()))),
       completionQueue :: !(TQueue m a)
     }

data Job m a = Job (m a) (SomeException -> a) String

withJobPool :: forall m a b.
               (MonadAsync m, MonadThrow m)
            => (JobPool m a -> m b) -> m b
withJobPool =
    bracket create close
  where
    create :: m (JobPool m a)
    create =
      atomically $
        JobPool <$> newTVar Map.empty
                <*> newTQueue

    close :: JobPool m a -> m ()
    close JobPool{jobsVar} = do
      jobs <- atomically (readTVar jobsVar)
      mapM_ cancel jobs

forkJob :: forall m a.
           (MonadAsync m, MonadMask m)
        => JobPool m a
        -> Job     m a
        -> m ()
forkJob JobPool{jobsVar, completionQueue} (Job action handler label) =
    mask $ \restore -> do
      jobAsync <- async $ do
        tid <- myThreadId
        labelThread tid label
        !res <- handleJust notAsyncExceptions (return . handler) $
                 restore action
        atomically $ do
          writeTQueue completionQueue res
          modifyTVar' jobsVar (Map.delete tid)

      let !tid = asyncThreadId (Proxy :: Proxy m) jobAsync
      atomically $ modifyTVar' jobsVar (Map.insert tid jobAsync)
  where
    notAsyncExceptions :: SomeException -> Maybe SomeException
    notAsyncExceptions e
      | Just (SomeAsyncException _) <- fromException e
                  = Nothing
      | otherwise = Just e

readSize :: MonadSTM m => JobPool m a -> STM m Int
readSize JobPool{jobsVar} = Map.size <$> readTVar jobsVar

collect :: MonadSTM m => JobPool m a -> STM m a
collect JobPool{completionQueue} = readTQueue completionQueue

