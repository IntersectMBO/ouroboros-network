module Test.ThreadNet.Util.NodeRestarts (
    NodeRestart (..)
  , NodeRestarts (..)
  , genNodeRestarts
  , noRestarts
  , shrinkNodeRestarts
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Traversable (forM)
import           Test.QuickCheck

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense

import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.Util.Slots (NumSlots (..))

data NodeRestart
  = NodeRekey
    -- ^ restart the node with a fresh operational key and immediately emit a
    -- delegation certificate transaction
  | NodeRestart
    -- ^ restart the node without rekeying
  deriving (Eq, Ord, Show)

instance Condense NodeRestart where
  condense = show

-- | Which nodes are scheduled to restart in each slot
--
-- INVARIANT no element 'Map' is empty
--
newtype NodeRestarts = NodeRestarts (Map SlotNo (Map CoreNodeId NodeRestart))
  deriving (Eq, Show)

instance Condense NodeRestarts where
  condense (NodeRestarts m) = condense
      [ (,) slot $
        [ (fromCoreNodeId cid, r)
        | (cid, r) <- Map.toAscList m'
        ]
      | (slot, m') <- Map.toAscList m
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
  fmap (NodeRestarts . Map.filter (not . Map.null) . Map.fromList) $ do
    ss <- sublistOf [0 .. SlotNo (t - 1)]
    forM ss $ \s ->
      fmap ((,) s) $
      let alreadyJoined = Map.keysSet $ Map.filter (< s) m
          keepSome
            | Set.null alreadyJoined = id
            | otherwise              =
              (`suchThat` \x -> not $ alreadyJoined `Set.isSubsetOf` Map.keysSet x)
          candidates = Map.filterWithKey (canRestartIn s) m
      in
      keepSome $
      if Map.null candidates
      then pure Map.empty
      else fmap (Map.fromList . map (flip (,) NodeRestart)) $
           sublistOf $ Map.keys $ candidates
  where
    isLeading (CoreNodeId i) s = i /= unSlotNo s `mod` n
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
