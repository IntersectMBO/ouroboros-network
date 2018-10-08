module MonadClass.MonadFork
  ( MonadFork (..)
  ) where

import qualified Control.Concurrent as IO
import           Control.Monad (void)

class Monad m => MonadFork m where
  fork    :: m () -> m ()

instance MonadFork IO where
  fork a = void $ IO.forkIO a
