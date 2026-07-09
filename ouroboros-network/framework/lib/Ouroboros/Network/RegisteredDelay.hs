-- Register delay, API around `GHC.Conc.registerDelay`
--
-- The module should be imported qualified.
module Ouroboros.Network.RegisteredDelay where

import Prelude hiding (read)
import Control.Monad ((>=>))
import Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Monad.Class.MonadTimer.SI

newtype RegisteredDelay m = RegisteredDelay { registeredDelayVar :: LazySTM.TVar m Bool }

new :: MonadTimer m
    => DiffTime
    -> m (RegisteredDelay m)
new = fmap RegisteredDelay . registerDelay

read :: MonadSTM m
     => RegisteredDelay m
     -> STM m Bool
read = LazySTM.readTVar . registeredDelayVar

await :: MonadSTM m
      => RegisteredDelay m
      -> STM m ()
await = read >=> check
