module Control.Monad.Class.MonadTest ( MonadTest (..) ) where

import           Control.Monad.Reader

class Monad m => MonadTest m where
  exploreRaces :: m ()
  exploreRaces = return ()

instance MonadTest IO

instance MonadTest m => MonadTest (ReaderT e m) where
  exploreRaces = lift exploreRaces

