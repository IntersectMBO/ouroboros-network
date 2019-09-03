{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.Util.NodeTopology
  ( -- * Node Topology
    NodeTopology (..)
  , coreNodeIdNeighbors
  , edgesNodeTopology
  , genNodeTopology
  , shrinkNodeTopology
  , meshNodeTopology
  , bottleneckSizeNodeTopology
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Node Topologies
-------------------------------------------------------------------------------}

-- | Which /lesser/ nodes each node connects to
--
-- INVARIANT: for each mapping @n -> ms@, @n > m@ for each @m@.
--
-- INVARIANT: each mapping is non-empty, except for @CoreNodeId 0@
--
-- INVARIANT: each mapping is sorted strictly ascending
--
-- INVARIANT: there is a mapping for each @CoreNodeId@ in the test, @0 .. n -
-- 1@
--
newtype NodeTopology = NodeTopology (Map CoreNodeId [CoreNodeId])
  deriving (Eq, Show)

instance Condense NodeTopology where
  condense top@(NodeTopology m)
    | top == mesh = "meshNodeTopology (NumCoreNodes " ++ show (Map.size m) ++ ")"
    | otherwise = condense
      [ (fromCoreNodeId nid, map fromCoreNodeId nids)
      | (nid, nids) <- Map.toAscList m ]
    where
      mesh = meshNodeTopology (NumCoreNodes (Map.size m))

-- | Connect every pair of nodes
--
meshNodeTopology ::
     NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> NodeTopology
meshNodeTopology numCoreNodes =
    NodeTopology $
    Map.fromList $
    [ (nid, enumCoreNodes (NumCoreNodes i))
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
          0 -> pure (me, [])
          _ ->
            fmap ((,) me) $
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
    mesh = meshNodeTopology (NumCoreNodes (Map.size m))
    -- TODO more sophisticated shrinks. I anticipate that they'll need to use
    -- 'Test.QuickCheck.Shrinking' or else risk very slow responses

-- | Partial; @error@ for a node not in the plan
--
coreNodeIdNeighbors ::
     HasCallStack
  => NodeTopology -> CoreNodeId -> [CoreNodeId]
coreNodeIdNeighbors (NodeTopology m) nid =
    case hit of
      Nothing      -> error $ "not found: " <> condense (nid, Map.toList m)
      Just lessers -> lessers ++ greaters
  where
    (_, hit, greaters0) = Map.splitLookup nid m
    greaters = Map.keys (Map.filter (nid `elem`) greaters0)

-- | The edges in this topology
edgesNodeTopology ::
     HasCallStack
  => NodeTopology -> [(CoreNodeId, CoreNodeId)]
edgesNodeTopology (NodeTopology m) =
    flip foldMap (Map.toList m) $ \(greater, lessers) ->
       map (flip (,) greater) lessers

-- | The neighbor count of the node with the fewest neighbors
--
bottleneckSizeNodeTopology :: HasCallStack => NodeTopology -> Int
bottleneckSizeNodeTopology top@(NodeTopology m) =
    check [ length (coreNodeIdNeighbors top nid) | nid <- Map.keys m ]
  where
    check = \case
        []   -> error "empty topology"
        x:xs -> foldl min x xs
