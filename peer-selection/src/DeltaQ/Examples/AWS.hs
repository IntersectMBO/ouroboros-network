{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTSyntax #-}

module DeltaQ.Examples.AWS where

import Algebra.Graph.Labelled.AdjacencyMap hiding (edge)
import qualified Algebra.Graph.Labelled.AdjacencyMap as Graph
import Algebra.Graph.Labelled.AdjacencyMap.ShortestPath
import qualified Data.List as List (sortBy, filter, foldl', head, null)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Last (..))
import qualified Data.Set as Set
import Data.Time.Clock
import Numeric.Natural

import DeltaQ.SimpleGS
import DeltaQ.Topography
import DeltaQ.LinkRestriction
import DeltaQ.TCP

data NetNode
  = NVirginiaAWS
  | OhioAWS
  | NCaliforniaAWS
  | OregonAWS
  | CanadaAWS
  | IrelandAWS
  | LondonAWS
  | FrankfurtAWS
  | TokyoAWS
  | SeoulAWS
  | SingaporeAWS
  | SydneyAWS
  | MumbaiAWS
  | SaoPauloAWS
  | GL10
  deriving (Eq, Ord, Bounded, Enum, Show)

-- | Graph of AWS nodes with G/S edge annotations.
aws :: Topography SimpleGS NetNode
aws = overlays $ 
  [ awsTestData
  , transpose awsTestData
  , GL10       -< edge (mkGS 9.6e-3 1.2e-5 ) >- IrelandAWS
  , IrelandAWS -< edge (mkGS 9.6e-3 6.1e-7 ) >- GL10
  ]

-- | Probably not the true minimal-cost cycle.
awsMinCycle :: Natural -> NetNode -> Topography SimpleGS NetNode
awsMinCycle bytes = awsCycle (compareAt bytes)

-- | Probably not the true maximal-cost cycle.
awsMaxCycle :: Natural -> NetNode -> Topography SimpleGS NetNode
awsMaxCycle bytes = awsCycle (flipOrder (compareAt bytes))
  where
  flipOrder compare a b = case compare a b of
    EQ -> EQ
    LT -> GT
    GT -> LT

awsMinCycle' :: Natural -> NetNode -> Topography BearerCharacteristics NetNode
awsMinCycle' bytes nodes = updateLinkAnnotation (setRestriction (ethernetR 1e9 1500)) $ overlays
  [ awsMinCycle bytes nodes
  , transpose (awsMinCycle bytes nodes)
  ]

awsMaxCycle' :: Natural -> NetNode -> Topography BearerCharacteristics NetNode
awsMaxCycle' bytes nodes = updateLinkAnnotation (setRestriction (ethernetR 1e9 1500)) $ overlays
  [ awsMaxCycle bytes nodes
  , transpose (awsMaxCycle bytes nodes)
  ]

-- | The 2-edge graph 'restrictions' is used to add restrictions to
-- the big AWS G/S graph. Whenever there is an edge in 'restrictions', its
-- link restriction is added. Otherwise, the default one given here is used
-- @ethernetR 1e9 1500@
aws' :: Topography BearerCharacteristics NetNode
aws' = updateLinkAnnotation f aws
  where
    f = addRestriction (ethernetR 1e9 1500) restrictions

-- | Restrictions to add to the AWS graph. 
-- Note the edge type: there can be _at most one_ link restriction between
-- any two nodes, and the last one takes precedence in case of duplicates.
restrictions :: AdjacencyMap (Last LinkRestriction) NetNode
restrictions = overlays
  [ GL10       -< Last (mkRestriction  7e6 1492) >- IrelandAWS
  , IrelandAWS -< Last (mkRestriction 39e6 1492) >- GL10
  ]

setRestriction :: LinkRestriction
               -> NetNode
               -> NetNode
               -> SimpleGS
               -> BearerCharacteristics
setRestriction lr _ _ gs = Bearer gs lr

addRestriction :: LinkRestriction -- ^ default link restriction.
               -> AdjacencyMap (Last LinkRestriction) NetNode
               -> NetNode
               -> NetNode
               -> SimpleGS
               -> BearerCharacteristics
addRestriction dft tr a b gs = case mlr of
    -- There's no edge in the link restriction map; use a default.
    Nothing        -> Bearer gs dft
    Just (Last lr) -> Bearer gs lr
  where
    mlr = edgeLabel a b tr

simpleBearerTest :: SimpleGS
                 -> [(DiffTime, Natural)]
                 -> [(DiffTime, Natural)]
simpleBearerTest dq
  = bearerTransitDelay b
  where
    e = ethernetR 10e6 1500
    b = mkBearer (linkRestrictionAsSimpleGS e) e

-- | An example giving, for each pair of nodes in a subset of the aws graph,
bbfExample :: [(NetNode, NetNode, [DiffTime])]
bbfExample = [(a, b, map (minTTC aws' a b) sizes) | (a,b) <- nodes]
  where
    sizes :: [Natural]
    sizes = [1024, 10 * 1024, 100 * 1024, 500 * 1024, 1000 * 1024, 2000 * 1024]
    nodes = [ (LondonAWS, FrankfurtAWS)
            , (LondonAWS, IrelandAWS)
            , (IrelandAWS, FrankfurtAWS)
            , (LondonAWS, OhioAWS)
            , (SaoPauloAWS, SingaporeAWS)
            ]

-- FIXME this only works if the nodes are directly connected; no path finding
-- is done.
minTTC
  :: Topography BearerCharacteristics NetNode
  -> NetNode
  -> NetNode
  -> Natural
  -> DiffTime
minTTC gr a b size = 
  ttc'
  where
    iw  = 10
    lp   = tcpRPCLoadPattern a2b b2a 60 iw  Nothing 400 size
    ttc' = ttc lp
    --rate :: Double
    --rate = fromRational $ 8 * (fromIntegral size ) / (toRational ttc')
    -- FIXME partial match; what if there is no edge?
    Just (Last a2b) = edgeLabel a b gr
    Just (Last b2a) = edgeLabel b a gr

sbt1 = simpleBearerTest (mkGS' 0.1 8e6) [(x, 1500) | x <- [0,0.1..1]]
sbt2 = simpleBearerTest (mkGS' 0.1 1e6) $ replicate 10 (0,1500)

-- time to complete
-- FIXME is partial.
ttc :: [(DiffTime, b)] -> DiffTime
ttc = fst . last

-- simple time to complete
stt1 iw size = (ttc', rate, lp)
  where
    -- load pattern, 60 octet PDH, 400 octet request - no congestion
    -- window limit
    lp   = tcpRPCLoadPattern a2b b2a 60 iw  Nothing 400 size
    ttc' = ttc lp
    rate :: Double
    rate = fromRational $ 8 * (fromIntegral size ) / (toRational ttc')
    a2b = mkBearer (mkGS 10e-3 1e-5) (mkRestriction 2e6 1500)
    b2a = mkBearer (mkGS 10e-3 1e-6) (mkRestriction 10e6 1500)

-- | Construct a cycle in the 'awsTestData' graph by selecting one edge
-- for each vertex according to an ordering: the lowest one.
--
-- Can be used to construct approximations of longest and shortest AWS cycles
-- for a given number of bytes, by comparing the GS at that number of bytes and
-- taking the dual order if desired. Actually computing the maximal/minimal
-- such cycle would be quite hard; it's travelling salesman on 15 nodes.
--
awsCycle :: (SimpleGS -> SimpleGS -> Ordering) -> NetNode -> Topography SimpleGS NetNode
awsCycle ordering start = case finalEdges of
  [] -> newGraph
  -- Add the final edge, to get a cycle.
  edgesFrom@(_:_) -> Graph.overlay
    newGraph
    (Graph.edge (snd (List.head (List.sortBy ordering' edgesFrom))) finalNode start)
  where
  finalEdges = List.filter (\(v, _) -> v == start) (Map.toList (postSetEdges finalNode unidirectional))
  (finalNode, newGraph, oldGraph) = construct (Graph.empty, unidirectional) start
  construct :: (Topography SimpleGS NetNode, Topography SimpleGS NetNode)
            -> NetNode
            -> (NetNode, Topography SimpleGS NetNode, Topography SimpleGS NetNode)
  construct (newGraph, oldGraph) node = case Map.toList (postSetEdges node oldGraph) of
    -- Cycle ends here.
    []              -> (node, newGraph, oldGraph)
    edgesFrom@(_:_) -> construct (newGraph', oldGraph') node'
      where
        (node', minimale) = List.head (List.sortBy ordering' edgesFrom)
        !newGraph'          = Graph.overlay newGraph (Graph.edge minimale node node')
        !oldGraph'          = removeVertex node oldGraph
  -- Must do the fold on the graph overlayed with its own transpose, otherwise
  -- weird results are possible due to the way in which the awsTestData graph
  -- is defined and the order of the nodes given to awsCycle. It's possible
  -- to get an empty graph, for instance, if @reverse [minBound..maxBound]@ is
  -- used.
  unidirectional = Graph.overlay (Graph.transpose awsTestData) awsTestData
  ordering' :: (NetNode, Last SimpleGS) -> (NetNode, Last SimpleGS) -> Ordering
  ordering' (_, Last a) (_, Last b) = ordering a b

awsTestData :: Topography SimpleGS NetNode
awsTestData = overlays 
  [ NVirginiaAWS -< edge (mkGS 0.006 3.27e-05 ) >- OhioAWS
  , NVirginiaAWS -< edge (mkGS 0.037 3.57e-05 ) >- NCaliforniaAWS
  , NVirginiaAWS -< edge (mkGS 0.037 4.84e-05 ) >- OregonAWS
  , NVirginiaAWS -< edge (mkGS 0.007 3.98e-05 ) >- CanadaAWS
  , NVirginiaAWS -< edge (mkGS 0.038 1.45e-05 ) >- IrelandAWS
  , NVirginiaAWS -< edge (mkGS 0.038 2.46e-05 ) >- LondonAWS
  , NVirginiaAWS -< edge (mkGS 0.044 1.34e-05 ) >- FrankfurtAWS
  , NVirginiaAWS -< edge (mkGS 0.083 3.68e-05 ) >- TokyoAWS
  , NVirginiaAWS -< edge (mkGS 0.097 3.65e-05 ) >- SeoulAWS
  , NVirginiaAWS -< edge (mkGS 0.12 4.43e-05 ) >- SingaporeAWS
  , NVirginiaAWS -< edge (mkGS 0.103 2.16e-05 ) >- SydneyAWS
  , NVirginiaAWS -< edge (mkGS 0.091 1.97e-05 ) >- MumbaiAWS
  , NVirginiaAWS -< edge (mkGS 0.06 3.13e-05 ) >- SaoPauloAWS

  , OhioAWS -< edge (mkGS 0.025 3.16e-05 ) >- NCaliforniaAWS
  , OhioAWS -< edge (mkGS 0.035 2.9e-05 ) >- OregonAWS
  , OhioAWS -< edge (mkGS 0.013 3.5e-05 ) >- CanadaAWS
  , OhioAWS -< edge (mkGS 0.04 4.65e-05 ) >- IrelandAWS
  , OhioAWS -< edge (mkGS 0.043 3.42e-05 ) >- LondonAWS
  , OhioAWS -< edge (mkGS 0.048 4.13e-05 ) >- FrankfurtAWS
  , OhioAWS -< edge (mkGS 0.083 3.05e-05 ) >- TokyoAWS
  , OhioAWS -< edge (mkGS 0.096 4.84e-05 ) >- SeoulAWS
  , OhioAWS -< edge (mkGS 0.112 3.46e-05 ) >- SingaporeAWS
  , OhioAWS -< edge (mkGS 0.097 2.12e-05 ) >- SydneyAWS
  , OhioAWS -< edge (mkGS 0.095 4.24e-05 ) >- MumbaiAWS
  , OhioAWS -< edge (mkGS 0.065 4.02e-05 ) >- SaoPauloAWS

  , NCaliforniaAWS -< edge (mkGS 0.011 6.66e-05 ) >- OregonAWS
  , NCaliforniaAWS -< edge (mkGS 0.038 5.77e-05 ) >- CanadaAWS
  , NCaliforniaAWS -< edge (mkGS 0.071 5.84e-05 ) >- IrelandAWS
  , NCaliforniaAWS -< edge (mkGS 0.069 2.72e-05 ) >- LondonAWS
  , NCaliforniaAWS -< edge (mkGS 0.073 3.79e-05 ) >- FrankfurtAWS
  , NCaliforniaAWS -< edge (mkGS 0.059 3.35e-05 ) >- TokyoAWS
  , NCaliforniaAWS -< edge (mkGS 0.071 2.83e-05 ) >- SeoulAWS
  , NCaliforniaAWS -< edge (mkGS 0.09 4.87e-05 ) >- SingaporeAWS
  , NCaliforniaAWS -< edge (mkGS 0.074 6.51e-05 ) >- SydneyAWS
  , NCaliforniaAWS -< edge (mkGS 0.123 4.99e-05 ) >- MumbaiAWS
  , NCaliforniaAWS -< edge (mkGS 0.096 4.65e-05 ) >- SaoPauloAWS

  , OregonAWS -< edge (mkGS 0.034 3.91e-05 ) >- CanadaAWS
  , OregonAWS -< edge (mkGS 0.064 4.24e-05 ) >- IrelandAWS
  , OregonAWS -< edge (mkGS 0.079 2.42e-05 ) >- LondonAWS
  , OregonAWS -< edge (mkGS 0.079 3.72e-05 ) >- FrankfurtAWS
  , OregonAWS -< edge (mkGS 0.052 4.32e-05 ) >- TokyoAWS
  , OregonAWS -< edge (mkGS 0.067 2.01e-05 ) >- SeoulAWS
  , OregonAWS -< edge (mkGS 0.089 5.77e-05 ) >- SingaporeAWS
  , OregonAWS -< edge (mkGS 0.081 4.43e-05 ) >- SydneyAWS
  , OregonAWS -< edge (mkGS 0.109 6.44e-05 ) >- MumbaiAWS
  , OregonAWS -< edge (mkGS 0.091 2.94e-05 ) >- SaoPauloAWS

  , CanadaAWS -< edge (mkGS 0.039 3.24e-05 ) >- IrelandAWS
  , CanadaAWS -< edge (mkGS 0.044 7.03e-05 ) >- LondonAWS
  , CanadaAWS -< edge (mkGS 0.051 3.27e-05 ) >- FrankfurtAWS
  , CanadaAWS -< edge (mkGS 0.084 6.14e-05 ) >- TokyoAWS
  , CanadaAWS -< edge (mkGS 0.096 4.09e-05 ) >- SeoulAWS
  , CanadaAWS -< edge (mkGS 0.11 2.34e-05 ) >- SingaporeAWS
  , CanadaAWS -< edge (mkGS 0.106 5.88e-05 ) >- SydneyAWS
  , CanadaAWS -< edge (mkGS 0.097 5.06e-05 ) >- MumbaiAWS
  , CanadaAWS -< edge (mkGS 0.065 3.61e-05 ) >- SaoPauloAWS

  , IrelandAWS -< edge (mkGS 0.005 3.31e-05 ) >- LondonAWS
  , IrelandAWS -< edge (mkGS 0.013 3.31e-05 ) >- FrankfurtAWS
  , IrelandAWS -< edge (mkGS 0.121 2.27e-05 ) >- TokyoAWS
  , IrelandAWS -< edge (mkGS 0.138 5.32e-05 ) >- SeoulAWS
  , IrelandAWS -< edge (mkGS 0.087 4.32e-05 ) >- SingaporeAWS
  , IrelandAWS -< edge (mkGS 0.142 3.72e-05 ) >- SydneyAWS
  , IrelandAWS -< edge (mkGS 0.061 1.67e-05 ) >- MumbaiAWS
  , IrelandAWS -< edge (mkGS 0.092 2.9e-05 ) >- SaoPauloAWS

  , LondonAWS -< edge (mkGS 0.008 3.83e-05 ) >- FrankfurtAWS
  , LondonAWS -< edge (mkGS 0.126 1.86e-05 ) >- TokyoAWS
  , LondonAWS -< edge (mkGS 0.147 3.2e-05 ) >- SeoulAWS
  , LondonAWS -< edge (mkGS 0.083 5.25e-05 ) >- SingaporeAWS
  , LondonAWS -< edge (mkGS 0.14 5.25e-05 ) >- SydneyAWS
  , LondonAWS -< edge (mkGS 0.055 4.76e-05 ) >- MumbaiAWS
  , LondonAWS -< edge (mkGS 0.098 4.24e-05 ) >- SaoPauloAWS

  , FrankfurtAWS -< edge (mkGS 0.132 5.92e-05) >- TokyoAWS
  , FrankfurtAWS -< edge (mkGS 0.155 3.83e-05) >- SeoulAWS
  , FrankfurtAWS -< edge (mkGS 0.083 3.68e-05) >- SingaporeAWS
  , FrankfurtAWS -< edge (mkGS 0.145 3.65e-05) >- SydneyAWS
  , FrankfurtAWS -< edge (mkGS 0.055 3.01e-05) >- MumbaiAWS
  , FrankfurtAWS -< edge (mkGS 0.104 3.76e-05) >- SaoPauloAWS

  , TokyoAWS -< edge (mkGS 0.016 5.1e-05) >- SeoulAWS
  , TokyoAWS -< edge (mkGS 0.034 8e-05) >- SingaporeAWS
  , TokyoAWS -< edge (mkGS 0.052 2.83e-05) >- SydneyAWS
  , TokyoAWS -< edge (mkGS 0.061 2.75e-05) >- MumbaiAWS
  , TokyoAWS -< edge (mkGS 0.154 3.53e-05) >- SaoPauloAWS

  , SeoulAWS -< edge (mkGS 0.049 2.94e-05) >- SingaporeAWS
  , SeoulAWS -< edge (mkGS 0.067 4.54e-05) >- SydneyAWS
  , SeoulAWS -< edge (mkGS 0.077 4.46e-05) >- MumbaiAWS
  , SeoulAWS -< edge (mkGS 0.17 4.58e-05) >- SaoPauloAWS

  , SingaporeAWS -< edge (mkGS 0.085 3.5e-05) >- SydneyAWS
  , SingaporeAWS -< edge (mkGS 0.028 2.23e-05) >- MumbaiAWS
  , SingaporeAWS -< edge (mkGS 0.187 6.14e-05) >- SaoPauloAWS

  , SydneyAWS -< edge (mkGS 0.111 4.91e-05) >- MumbaiAWS
  , SydneyAWS -< edge (mkGS 0.16 5.32e-05) >- SaoPauloAWS
  
  , MumbaiAWS -< edge (mkGS 0.153 6.25e-05) >- SaoPauloAWS
  ]
