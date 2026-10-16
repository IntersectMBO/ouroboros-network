-- | What every benchmark fixture goes through before it is timed.
module Bench.Env (prepareEnv) where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import System.Mem (performMajorGC)

-- | Evaluate a fixture in full and collect, so neither its construction nor
-- the garbage of it is timed.
prepareEnv :: NFData a => a -> IO a
prepareEnv a = do
  _ <- evaluate (rnf a)
  performMajorGC
  pure a
