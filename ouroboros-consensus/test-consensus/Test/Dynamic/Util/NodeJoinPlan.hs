{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.Util.NodeJoinPlan
  ( -- * Node Join Plan
    NodeJoinPlan (..)
  , genNodeJoinPlan
  , shrinkNodeJoinPlan
  , trivialNodeJoinPlan
  ) where

import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Node Join Plans
-------------------------------------------------------------------------------}

-- | In which slot each node joins the network
--
newtype NodeJoinPlan = NodeJoinPlan (Map CoreNodeId SlotNo)
  deriving (Show)

instance Condense NodeJoinPlan where
  condense (NodeJoinPlan m) = condense
      [ (fromCoreNodeId nid, slot) | (nid, slot) <- Map.toAscList m ]

-- | All nodes join immediately
--
trivialNodeJoinPlan :: NumCoreNodes -> NodeJoinPlan
trivialNodeJoinPlan numCoreNodes =
    NodeJoinPlan $
    Map.fromList $
    [ (nid, SlotNo 0) | nid <- enumCoreNodes numCoreNodes ]

-- | Generate a 'NodeJoinPlan' consistent with the given properties
--
-- INVARIANT: Nodes with higher Ids will not join before nodes with lower Ids.
-- This eliminates some uninteresting symmetry and makes the counter-examples
-- easier for humans to interpret.
--
genNodeJoinPlan ::
     NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> NumSlots
     -- ^ PRECONDITION: positive
  -> Gen NodeJoinPlan
genNodeJoinPlan numCoreNodes@(NumCoreNodes n) numSlots@(NumSlots t)
  | n < 0 || t < 1 = error $ "Cannot generate TestConfig: "
    ++ show (numCoreNodes, numSlots)

  | otherwise = do
    let genNodeSchedule = do
            let lastSlot = t - 1
            (SlotNo . toEnum) <$> choose (0, lastSlot)

    let nids = enumCoreNodes numCoreNodes :: [CoreNodeId]
    schedules <- vectorOf n genNodeSchedule
    -- without loss of generality, the nodes start initializing in order of
    -- their Ids; this merely makes it easer to interpret the counterexamples
    pure $ NodeJoinPlan $ Map.fromList $ zip nids $ List.sort schedules

-- | Shrink a node join plan
--
-- The new plans must be usable with the same number of nodes and slots as the
-- old plan
--
shrinkNodeJoinPlan :: NodeJoinPlan -> [NodeJoinPlan]
shrinkNodeJoinPlan (NodeJoinPlan m) =
    map (NodeJoinPlan . Map.fromList) $
    reverse $   -- favor eliminating more delays
    zeroedInits id (Map.toAscList m)
  where
    -- TODO more sophisticated shrinks. I anticipate that they'll need to use
    -- 'Test.QuickCheck.Shrinking' or else risk very slow responses

    -- For example, @zeroedInits id [(c0, 0), (c1, 1), (c2, 2), (c3, 3)]@ is
    -- > [ [(c0, 0), (c1, 0), (c2, 2), (c3, 3)]
    -- > , [(c0, 0), (c1, 0), (c2, 0), (c3, 3)]
    -- > , [(c0, 0), (c1, 0), (c2, 0), (c3, 0)]
    -- > ]
    --
    -- (And note that we elsewhere reverse the order of those results.)
    zeroedInits ::
        forall cnid plan.
        ([(cnid, SlotNo)] -> plan) -> [(cnid, SlotNo)] -> [plan]
    zeroedInits fromTail = \case
        [] -> []
        (cnid, slot) : pairs ->
            fromTail' pairs `cons` zeroedInits fromTail' pairs
          where
            fromTail' = fromTail . (:) (cnid, SlotNo 0)
            cons x xs = if SlotNo 0 == slot then xs else x:xs
