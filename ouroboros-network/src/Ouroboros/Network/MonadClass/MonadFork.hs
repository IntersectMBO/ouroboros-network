{-# LANGUAGE ScopedTypeVariables #-}
module Ouroboros.Network.MonadClass.MonadFork
  ( MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Exception (SomeException, catch, displayException)
import           Control.Monad (void)
import           System.IO (hPutStrLn, stderr)

class Monad m => MonadFork m where
  fork    :: m () -> m ()

instance MonadFork IO where
  fork a = void (IO.forkIO a)
      `catch` (\(e :: SomeException) ->
           hPutStrLn stderr $ "Uncaught exception in thread:" ++ displayException e)
