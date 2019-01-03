{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module Control.Monad.Class.MonadProbe
  ( MonadProbe (..)
  , MonadRunProbe (..)
  , withProbe
  ) where

import qualified Control.Concurrent.STM.TVar as STM
import           System.Clock (Clock (Monotonic), TimeSpec, getTime, toNanoSecs)

import           Control.Monad.Class.MonadSTM (atomically)
import           Control.Monad.Class.MonadTimer (Time)


type ProbeTrace m a = [(Time m, a)]

class MonadProbe m where
  type Probe m :: * -> *
  probeOutput :: Probe m a -> a -> m ()

class (MonadProbe m, Monad n) => MonadRunProbe m n | m -> n, n -> m where
  newProbe    :: n (Probe m a)
  readProbe   :: Probe m a -> n (ProbeTrace m a)
  runM        :: m () -> n ()

withProbe :: MonadRunProbe m n
          => (Probe m a -> m ())
          -> n (ProbeTrace m a)
withProbe f = do
  p <- newProbe
  runM (f p)
  readProbe p

--
-- Instances
--

newtype ProbeIO a = ProbeIO (STM.TVar [(Int, a)])

instance MonadProbe IO where
  type Probe IO = ProbeIO
  probeOutput (ProbeIO p) a = do
    t <- toMicroseconds <$> getTime Monotonic
    -- the user is not exposed to the inner TVar, so it should never block for
    -- too long.
    atomically $ STM.modifyTVar' p ((t,a):)
    where
      toMicroseconds :: TimeSpec -> Int
      toMicroseconds = fromIntegral . (div 1000) . toNanoSecs

  -- In the above the starting state is pending, there is only one transaction
  -- that goes from pending to fired, and only one that goes from pending to
  -- cancelled. We can see from this that the state changes at most once, even
  -- if there were multiple uses of cancel or update, or even if the ghc timer
  -- manager ran the action multiple times. For example, there can be a race
  -- between firing and cancelling, but one will win and the state will change
  -- only once, so the final state is stable.

instance MonadRunProbe IO IO where
  newProbe  = ProbeIO <$> STM.newTVarIO []
  readProbe (ProbeIO p) = STM.readTVarIO p
  runM = id

