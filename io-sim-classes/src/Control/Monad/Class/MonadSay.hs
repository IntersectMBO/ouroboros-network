module Control.Monad.Class.MonadSay where

import           Control.Monad.State

class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = print

instance MonadSay m => MonadSay (StateT s m) where
  say = lift . say
