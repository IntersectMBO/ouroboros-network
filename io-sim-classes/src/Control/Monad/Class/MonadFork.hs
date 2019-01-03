{-# LANGUAGE ScopedTypeVariables #-}
module Control.Monad.Class.MonadFork
  ( MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception
import           Control.Monad (void)
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
