{-# LANGUAGE NamedFieldPuns #-}

module Test.Dynamic.Util.Expectations
    ( NumBlocks (..)
    , determineForkLength
    ) where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..))

newtype NumBlocks = NumBlocks Word64

-- | Internal accumulator of 'determineForkLength'
data Acc = Acc
  { maxForkLength  :: !Word64
    -- ^ Upper bound on length of the longest fork in the network, excluding
    -- the common prefix
    --
    -- Note that @0@ corresponds to /consensus/.
  }

-- | Compute a bound for the length of the final forks
--
-- At the end of the test, the nodes' final chains will share a common prefix
-- (\"intersection\"), though it may be empty. Each current chain is a fork off
-- that prefix. No fork will be longer than the result of this function.
determineForkLength ::
     SecurityParam
  -> LeaderSchedule
  -> NumBlocks
determineForkLength k (LeaderSchedule m) =
    prj $ foldl' step initial (Map.toAscList m)
  where
    prj Acc{maxForkLength} = NumBlocks maxForkLength

    -- assume the network begins in consensus (eg all nodes start at Genesis)
    initial = Acc
      { maxForkLength = 0
      }

    -- this logic focuses on the new chains made in this slot that are longer
    -- than the longest chains from the previous slot
    step Acc{maxForkLength} (_slot, leaders) =
        Acc
          { maxForkLength = update maxForkLength
          }
      where
        update
            -- too late to reach consensus, so further diverge
          | maxForkLength > maxRollbacks k = (+ 1)

            -- assume (common) worst-case: each leader creates a unique longer
            -- chain
          | pullingAhead >  1              = (+ 1)

            -- the sole leader creates the sole longer chain, bringing the
            -- network into consensus
          | pullingAhead == 1              = const 0

            -- no leaders, so the longest chains are the same as in the
            -- previous slot
          | otherwise                      = id

        -- how many leaders are making a new chain longer than any from the
        -- previous slot
        pullingAhead = length leaders
