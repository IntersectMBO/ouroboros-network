{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.Util.Expectations (
    NumBlocks (..)
  , determineForkLength
  ) where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import           Data.Word (Word64)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config.SecurityParam
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule

import           Test.ThreadNet.Util.NodeJoinPlan

newtype NumBlocks = NumBlocks Word64
  deriving (Eq, Show)

-- | Internal accumulator of 'determineForkLength'
data Acc = Acc
  { maxChainLength :: !Word64
    -- ^ Upper bound on length of the longest chain in the network
  , maxForkLength  :: !Word64
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
-- ASSUMPTION: The network connectivity is such that -- barring other
-- non-network considerations -- any block forged at the start of a slot will
-- be fetched and possibly selected by every other node before the end of the
-- slot.
--
-- *How 'LeaderSchedule' affects this function*
--
-- A round-robin 'LeaderSchedule' will always reach consensus, so the fork
-- length will be @0@. For other 'LeaderSchedule's -- whether known /a priori/
-- (see "Test.ThreadNet.LeaderSchedule") or /a posteriori/ (see
-- "Test.ThreadNet.Praos") -- we often will not expect consensus. The key
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
-- *How 'NodeJoinPlan' affects this function*
--
-- Because the race condition between forging and ChainSync/BlockFetch is
-- consistently won by forging, a node that leads in the same slot it joins
-- always attempts to make a chain of length 1 (ASSUMPTION: new nodes start
-- with an empty chain). Except for the edge cases when the longest chains in
-- the network are of length 0 or 1, this means that leaders who are joining
-- can be disregarded.
--
determineForkLength ::
     SecurityParam
  -> NodeJoinPlan
  -> LeaderSchedule
  -> NumBlocks
determineForkLength k (NodeJoinPlan joinPlan) (LeaderSchedule sched) =
    prj $ foldl' step initial (Map.toAscList sched)
  where
    prj Acc{maxForkLength} = NumBlocks maxForkLength

    -- assume the network begins in consensus (eg all nodes start at Genesis)
    initial = Acc
      { maxChainLength = 0
      , maxForkLength = 0
      }

    -- this logic focuses on the new chains made in this slot that are longer
    -- than the longest chains from the previous slot
    step Acc{maxChainLength, maxForkLength} (slot, leaders) =
        Acc
          { maxChainLength = grow    maxChainLength
          , maxForkLength  = update  maxForkLength
          }
      where
        grow = if 0 == pullingAhead then id else (+ 1)

        update
            -- too late to reach consensus, so further diverge
          | maxForkLength > maxRollbacks k = grow

            -- assume (common) worst-case: each leader creates a unique longer
            -- chain
          | pullingAhead >  1              = (+ 1)

            -- the sole leader creates the sole longer chain, bringing the
            -- network into consensus
          | pullingAhead == 1              = const 0

            -- there will be multiple longest chains that disagree on at least
            -- the latest block
            --
            -- Note that pullingAhead == 0 by the preceding guards
          | pullingEven  >  0              = max 1

            -- no nodes are extending their chain, so the longest chains are
            -- the same as in the previous slot
          | otherwise                      = id

        -- how many leaders are forging a block onto a @maxForkLength@-chain
        pullingAhead = nlOld + nlNew (maxChainLength == 0)
        -- how many leaders are forging a block onto a @maxForkLength - 1@-chain
        pullingEven  = nlNew (maxChainLength == 1)

        -- how many leaders joined before this slot
        nlOld = length $ filter ((< slot) . joinSlot) leaders
        nlNew b
          | not b     = 0
            -- how many leaders are joining during this slot; these might not
            -- be relevant
            --
            -- A node leading the same slot it joins always forges a 1-block
            -- chain; there's actually a race-condition between block forging
            -- and BlockFetch/ChainSync, but forging always wins in the current
            -- test framework implementation.
          | otherwise = length $ filter ((== slot) . joinSlot) leaders

        -- slot in which the node joins the network
        joinSlot :: CoreNodeId -> SlotNo
        joinSlot nid = case Map.lookup nid joinPlan of
            Nothing    -> error "determineForkLength: incomplete node join plan"
            Just slot' -> slot'
