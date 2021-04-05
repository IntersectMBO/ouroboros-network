{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ThreadNet.Util.NodeTopology (
    -- * Node Topology
    NodeTopology (..)
  , coreNodeIdNeighbors
  , edgesNodeTopology
  , genNodeTopology
  , mapNodeTopology
  , meshNodeTopology
  , minimumDegreeNodeTopology
  , shrinkNodeTopology
  , unionNodeTopology
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Quiet (Quiet (..))
import           Test.QuickCheck

import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Node Topologies
-------------------------------------------------------------------------------}

-- | Which /lesser/ nodes each node connects to
--
-- INVARIANT: for each mapping @n -> ms@, @n > m@ for each @m@ in @ms@
--
-- INVARIANT: only the mapping for @n = CoreNodeId 0@ is empty
--
-- INVARIANT: there is a mapping for each @CoreNodeId@ in the test, @0 .. n -
-- 1@
--
-- Note that every node is connected to every other but not necessarily
-- directly. In other words, the topology is always a single network of nodes,
-- which is realistic. With other test configuration components, such as
-- network partitions, the network may split (temporarily or not) into separate
-- connected components during the execution, but the base topology is
-- connected.
--
newtype NodeTopology =
    NodeTopology {unNodeTopology :: Map CoreNodeId (Set CoreNodeId)}
  deriving (Eq, Generic)
  deriving (Show) via Quiet NodeTopology

instance Condense NodeTopology where
  condense top@(NodeTopology m)
    | top == mesh = "meshNodeTopology (NumCoreNodes " ++ show (Map.size m) ++ ")"
    | otherwise = condense
      [ (fromCoreNodeId nid, Set.map fromCoreNodeId nids)
      | (nid, nids) <- Map.toAscList m ]
    where
      mesh = meshNodeTopology (NumCoreNodes (fromIntegral (Map.size m)))

-- | Connect every pair of nodes
--
meshNodeTopology ::
     NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> NodeTopology
meshNodeTopology numCoreNodes =
    NodeTopology $
    Map.fromList $
    [ (nid, Set.fromList $ enumCoreNodes (NumCoreNodes i))
    | nid@(CoreNodeId i) <- enumCoreNodes numCoreNodes ]

-- | Generate a 'NodeTopology' consistent with the given properties
--
genNodeTopology ::
     HasCallStack
  => NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> Gen NodeTopology
genNodeTopology numCoreNodes@(NumCoreNodes n)
  | n < 0 = error $ "Unsatisfiable parameters: "
    ++ show numCoreNodes

  | otherwise = do
    let genNeighbors me@(CoreNodeId i) = case i of
          0 -> pure (me, Set.empty)
          _ ->
            fmap ((,) me . Set.fromList) $
            flip suchThat (not . null) $
            sublistOf (enumCoreNodes (NumCoreNodes i))

    fmap (NodeTopology . Map.fromList) $
      mapM genNeighbors (enumCoreNodes numCoreNodes)

-- | Shrink a node topology
--
-- The new topologies must be usable with the same number of nodes and slots as
-- the old topology
--
shrinkNodeTopology :: NodeTopology -> [NodeTopology]
shrinkNodeTopology top@(NodeTopology m)
    | top == mesh = []
    | otherwise = [mesh]
  where
    mesh = meshNodeTopology (NumCoreNodes (fromIntegral (Map.size m)))
    -- TODO more sophisticated shrinks. I anticipate that they'll need to use
    -- 'Test.QuickCheck.Shrinking' or else risk very slow responses

-- | The neighbors of this node
--
coreNodeIdNeighbors ::
     HasCallStack
  => NodeTopology -> CoreNodeId -> [CoreNodeId]
coreNodeIdNeighbors (NodeTopology m) nid =
    case hit of
      Nothing      ->
          error $
          "invariant violated: " <>
          "could not find " <> condense (nid, Map.toList m)
      Just lessers -> Set.toList lessers ++ greaters
  where
    (_, hit, greaters0) = Map.splitLookup nid m
    greaters = Map.keys (Map.filter (nid `Set.member`) greaters0)

-- | The edges in this topology
--
edgesNodeTopology :: NodeTopology -> [(CoreNodeId, CoreNodeId)]
edgesNodeTopology (NodeTopology m) =
    flip foldMap (Map.toList m) $ \(greater, lessers) ->
        map (flip (,) greater) (Set.toList lessers)

-- | The neighbor count of the node with the fewest neighbors, unless there are
-- zero nodes
--
minimumDegreeNodeTopology :: NodeTopology -> Maybe Int
minimumDegreeNodeTopology top@(NodeTopology m) =
    check [ length (coreNodeIdNeighbors top nid) | nid <- Map.keys m ]
  where
    check = \case
        []   -> Nothing
        x:xs -> Just $ foldl min x xs

unionNodeTopology :: NodeTopology -> NodeTopology -> NodeTopology
unionNodeTopology (NodeTopology l) (NodeTopology r) =
    NodeTopology $ Map.unionWith Set.union l r

mapNodeTopology :: (CoreNodeId -> CoreNodeId) -> NodeTopology -> NodeTopology
mapNodeTopology f topo =
    NodeTopology $ Map.fromListWith Set.union $
    [ f l `sortedSingleton` f r
    | (l, r) <- edgesNodeTopology topo
    ]
  where
    sortedSingleton l r =
        if l > r then (l, Set.singleton r) else (r, Set.singleton l)
