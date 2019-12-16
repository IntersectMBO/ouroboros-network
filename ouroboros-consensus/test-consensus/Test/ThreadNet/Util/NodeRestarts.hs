module  Test.ThreadNet.Util.NodeRestarts (
  NodeRestarts (..),
  noRestarts,
  genNodeRestarts,
  shrinkNodeRestarts,
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Traversable (forM)
import           Test.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense

import           Test.ThreadNet.Util.NodeJoinPlan


-- | Which nodes are scheduled to restart in each slot
--
-- INVARIANT no 'Set' is empty
--
newtype NodeRestarts = NodeRestarts (Map SlotNo (Set CoreNodeId))
  deriving (Eq, Show)

instance Condense NodeRestarts where
  condense (NodeRestarts m) = condense
      [ (,) slot $
        [ fromCoreNodeId cid
        | cid <- Set.toAscList cids
        ]
      | (slot, cids) <- Map.toAscList m
      ]

noRestarts :: NodeRestarts
noRestarts = NodeRestarts Map.empty

-- | Generate a valid 'NodeRestarts'
--
-- POSTCONDITION will not restart a node before it joins
--
-- POSTCONDITION will not restart a node when it's scheduled to lead
-- *according* *to* *round-robin*
--
-- POSTCONDITION will not simultaneously restart all nodes that have previously
-- joined
--
genNodeRestarts :: NodeJoinPlan -> NumSlots -> Gen NodeRestarts
genNodeRestarts (NodeJoinPlan m) (NumSlots t)
  | t < 1     = pure noRestarts
  | otherwise =
  fmap (NodeRestarts . Map.filter (not . Set.null) . Map.fromList) $ do
    ss <- sublistOf [0 .. SlotNo (fromIntegral t - 1)]
    forM ss $ \s ->
      fmap ((,) s) $
      let alreadyJoined = Map.keysSet $ Map.filter (< s) m
          keepSome
            | Set.null alreadyJoined = id
            | otherwise              =
              (`suchThat` \x -> not $ alreadyJoined `Set.isSubsetOf` x)
          candidates = Map.filterWithKey (canRestartIn s) m
      in
      keepSome $
      if Map.null candidates
      then pure Set.empty
      else (fmap Set.fromList $ sublistOf $ Map.keys $ candidates)
  where
    isLeading (CoreNodeId i) s = fromIntegral i /= unSlotNo s `mod` n
      where
        n = fromIntegral $ Map.size m

    canRestartIn s nid joinSlot =
        -- must be present
        joinSlot <= s &&
        -- must not be leading (TODO relax this somehow?)
        not (isLeading nid s)

shrinkNodeRestarts :: NodeRestarts -> [NodeRestarts]
shrinkNodeRestarts (NodeRestarts m)
  | Map.null m = []  -- TODO better shrink
  | otherwise  = [noRestarts]
