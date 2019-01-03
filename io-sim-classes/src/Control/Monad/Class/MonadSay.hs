module Control.Monad.Class.MonadSay where

class Monad m => MonadSay m where
  say :: String -> m ()

instance MonadSay IO where
  say = print
