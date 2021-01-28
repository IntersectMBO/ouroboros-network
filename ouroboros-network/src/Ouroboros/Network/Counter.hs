module Ouroboros.Network.Counter (
    Counter
  ) where

import Control.Tracer

-- | Type alias to 
type Counter m a = Tracer m a
