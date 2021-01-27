module Ouroboros.Network.Counter (
    Counter
  , nullCounter
  ) where

import Control.Tracer

-- | Type alias to provide type information to indicate counters should
-- be handled differently from tracers.
type Counter m a = Tracer m a

-- | this 'Tracer' ignores all counter messages
nullCounter :: Applicative m => Counter m a
nullCounter = nullTracer
