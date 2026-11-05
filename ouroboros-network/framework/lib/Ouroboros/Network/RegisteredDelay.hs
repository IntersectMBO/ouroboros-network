{-# LANGUAGE NamedFieldPuns #-}

-- | A registered delay: an API around the timer library's cancellable
-- delay, 'registerDelayCancellable'.  For any delay an 'Int' of
-- microseconds holds, which on a 64-bit machine is every delay there is,
-- the library's IO instance registers one timeout with the timer manager
-- and a cancel unregisters it; beyond that bound it forks a thread, and a
-- cancel only stops that thread's write.
--
-- The module should be imported qualified.
module Ouroboros.Network.RegisteredDelay where

import Control.Concurrent.Class.MonadSTM
import Control.Monad ((>=>))
import Control.Monad.Class.MonadTimer.SI
import Prelude hiding (read)

-- | A delay registered with the timer manager: its state, and its
-- cancellation.
data RegisteredDelay m = RegisteredDelay {
    registeredDelayState :: STM m TimeoutState,
    cancelRegistered     :: m ()
  }

-- | Register a delay.  A non-positive one is fired at birth and registers
-- nothing.
new :: MonadTimer m
    => DiffTime
    -> m (RegisteredDelay m)
new d
  | d <= 0    = pure RegisteredDelay { registeredDelayState = pure TimeoutFired
                                     , cancelRegistered     = pure () }
  | otherwise = do
      (state, cancelIt) <- registerDelayCancellable d
      pure RegisteredDelay { registeredDelayState = state, cancelRegistered = cancelIt }

-- | Whether the delay has fired; a cancelled one never has.
read :: MonadSTM m
     => RegisteredDelay m
     -> STM m Bool
read = fmap (== TimeoutFired) . registeredDelayState

await :: MonadSTM m
      => RegisteredDelay m
      -> STM m ()
await = read >=> check

-- | Cancel the delay while it is pending; one fired or cancelled already
-- is left as it is.
cancel :: MonadSTM m
       => RegisteredDelay m
       -> m ()
cancel RegisteredDelay { registeredDelayState, cancelRegistered } = do
    pending <- atomically ((== TimeoutPending) <$> registeredDelayState)
    if pending then cancelRegistered else pure ()
