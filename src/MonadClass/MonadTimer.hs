{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module MonadClass.MonadTimer
  ( TimeMeasure (..)
  , MonadTimer (..)
  , mult
  , fromStart
  ) where

import qualified Control.Concurrent as IO
import           Control.Monad (void)

class (Ord t, Ord (Duration t), Num (Duration t)) => TimeMeasure t where
  type Duration t :: *

  diffTime :: t -> t -> Duration t
  addTime  :: Duration t -> t -> t
  zero :: t

-- | Helper function to multiply `Duration t` by an `Int`.
--
mult :: Num d => Int -> d -> d
mult n = sum . replicate n

-- | Count time since @'zero'@.
--
fromStart :: TimeMeasure t => Duration t -> t
fromStart = flip addTime zero

class (Monad m, TimeMeasure (Time m)) => MonadTimer m where
  type Time m :: *
  timer :: Duration (Time m) -> m () -> m ()

--
-- Instances
--

instance TimeMeasure Int where
  type Duration Int = Int -- microseconds

  diffTime t t' = t-t'
  addTime  d t  = t+d
  zero = 0

instance MonadTimer IO where
  type Time IO = Int -- microseconds

  timer t a = void $ IO.forkIO (IO.threadDelay t >> a)
