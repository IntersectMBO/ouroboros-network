{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Class.MonadFork
  ( MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception
import           Control.Monad (void)
import           Control.Monad.Except
import           Control.Monad.Reader
import           System.IO (hPutStrLn, stderr)

class Monad m => MonadFork m where
  fork    :: m () -> m ()

instance MonadFork IO where
  fork a =
    let handleException :: Either SomeException () -> IO ()
        handleException (Left e) =
            hPutStrLn stderr $ "Uncaught exception in thread:" ++ displayException e
        handleException (Right x) = return x
    in void (IO.forkFinally a handleException)

instance MonadFork m => MonadFork (ReaderT e m) where
  fork (ReaderT f) = ReaderT $ \e -> fork (f e)

-- NOTE(adn): Is this a sensible instance?
instance (Show e, MonadFork m) => MonadFork (ExceptT e m) where
  fork (ExceptT m) = ExceptT $ Right <$> fork (either (error . show) id <$> m)

