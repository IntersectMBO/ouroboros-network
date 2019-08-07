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
  deriving (Eq, Show)

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
-- (\"intersection\"), though it may be empty. Each node's current chain is a
-- fork off that prefix. No such fork will be longer than the result of this
-- function.
--
-- ASSUMPTION: The network connectivity is such that any block forged in a slot
-- will be fetched and possibly selected by every other node before the end of
-- the slot.
--
-- *How 'LeaderSchedule' affects this function*
--
-- A round-robin 'LeaderSchedule' will always reach consensus, so the fork
-- length will be @0@. For other 'LeaderSchedule's -- whether known /a priori/
-- (see "Test.Dynamic.LeaderSchedule") or /a posteriori/ (see
-- "Test.Dynamic.Praos") -- we often will not expect consensus. The key
-- difference is that a round-robin schedule always has exactly one leader per
-- slot. Arbitrary schedules instead may have 0 or multiple leaders.
--
-- A sequence of slots in which no slot has exactly one leader can drive a
-- network away from consensus. This function bounds how far from consensus the
-- network could be after the last simulated slot. It determines a conservative
-- upper bound on the length of the contended suffix of the nodes' final
-- chains. The bound may be less than, equal, or greater than @k@; regardless
-- of which, it should be used to test the nodes' final chains.
--
-- If such a sequence is long enough, it might even prevent the network from
-- every reaching consensus again: the nodes at the end of the sequence may
-- have chains that disagree by more than @k@ blocks, thereby exceeding the
-- security parameter @k@. In particular, if the bound determined by a
-- 'LeaderSchedule' is greater than @k@, then no extension of that
-- 'LeaderSchedule' can determine a lesser bound.
--
-- Multiple leaders create divergent chains as follows. Assume that every
-- leader of @s + 1@ begins the slot with a chain of length @n@, and that no
-- chain in the network has a greater length. Each leader forges a unique
-- block. A race condition between ChainSync/BlockFetch and forging makes it
-- possible, though relatively unlikely, that a leader would have received
-- another leader's new block before it forges its own. In that case, the new
-- leader will forged a block but then not select it, since it already selected
-- the other leader's new block. This function assumes the \"worst-case\" in
-- which both leaders make separate chains, thereby breaking consensus. Hence
-- it computes an upper bound. Each leader thus selects a new @n+1@-length
-- chain, and each non-leader will adopt one of them because they previously
-- had a chain of length @<= n@. Thus the network is likely not in consensus at
-- the end of a multi-leader slot.
--
-- The network will reestablish consensus at the end of a single-leader slot,
-- because the leader creates the only chain of length @n+1@ and all other
-- nodes thus select it.
--
-- In a slot with no leaders, all nodes will simply retain their current
-- chains.
--
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
          | numLeaders >  1                = (+ 1)

            -- the sole leader creates the sole longer chain, bringing the
            -- network into consensus
          | numLeaders == 1                = const 0

            -- no leaders, so the longest chains are the same as in the
            -- previous slot
          | otherwise                      = id

        -- ASSUMPTION: each leader is making a unique new chain that is longer
        -- than any from the previous slot
        numLeaders = length leaders
