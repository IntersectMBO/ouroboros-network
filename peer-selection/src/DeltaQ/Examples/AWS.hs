{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTSyntax #-}

module DeltaQ.Examples.AWS where

import Algebra.Graph.Labelled.AdjacencyMap hiding (edge)
import Algebra.Graph.Labelled.AdjacencyMap.ShortestPath
import Data.Monoid (Last (..))
import qualified Data.Semigroup as Semigroup (Last (..))
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

-- | The 2-edge graph 'restrictions' is used to add restrictions to
-- the big AWS G/S graph. Whenever there is an edge in 'restrictions', its
-- link restriction is added. Otherwise, the default one given here is used
-- @ethernetR 1e9 1500@
aws' :: Topography BearerCharacteristics NetNode
aws' = updateLinkAnnotation f aws
  where
    f = addRestriction (ethernetR 1e9 1500) restrictions

data Latency where
  Loss :: Latency
  TX   :: !Rational -> Latency

deriving instance Show Latency

instance Eq Latency where
  Loss == Loss = True
  TX a == TX b = a == b
  _    == _    = False

instance Ord Latency where
  Loss `compare` Loss = EQ
  Loss `compare` _    = GT
  _    `compare` Loss = LT
  TX a `compare` TX b = a `compare` b

instance Semigroup Latency where
  Loss <> _    = Loss
  _    <> Loss = Loss
  TX a <> TX b = TX (a + b)

instance Monoid Latency where
  mappend = (<>)
  mempty = TX 0

to_difftime :: Latency -> Maybe DiffTime
to_difftime Loss   = Nothing
to_difftime (TX a) = Just (fromRational a)

-- Must know the edges in _both directions_!
-- Instead of messing with the shortest path algorithm, we could construct
-- another graph with edge type
--
--   [(BearerCharacteristics, BearerCharacteristics)]
--
-- that just pairs edges in either direction.
--
-- So, on aws' we map the edges to "Send"
-- Then we transpose aws' and map the edges to "Recv"
-- Then we overlay them, with a monoid instance that pairs Sends and Recvs
-- arbitrarily, leaving extras...
-- It'll be easiest if we ditch the multiple edges as well, favouring
--
--   Data.Monoid.Last BearerCharacteristics

data UEdge e where
  Out :: e -> UEdge e
  In  :: e -> UEdge e
  Uni :: e -> e -> UEdge e
  deriving (Eq, Show)

instance Semigroup e => Semigroup (UEdge e) where

  Out o <> Out o'   = Out (o <> o')
  Out o <> In i     = Uni o i
  Out o <> Uni o' i = Uni (o <> o') i

  In i  <> In i'    = In (i <> i')
  In i  <> Out o    = Uni o i
  In i  <> Uni o i' = Uni o (i <> i')

  Uni i o <> Uni i' o' = Uni (i <> i') (o <> o')

-- So the type we want for edges is
--
--   Maybe (UEdge (Last BearerCharacteristics))
--
-- Kinda annoying but that's the library we're dealing with...

aws_shortest_paths
  :: Natural
  -> AllShortestPaths NetNode (Maybe (UEdge (Semigroup.Last BearerCharacteristics))) Latency
aws_shortest_paths response_size = all_pairs_sp mkWeight aws_graph
  where
  mkWeight :: NetNode -> NetNode -> Maybe (UEdge (Semigroup.Last BearerCharacteristics)) -> Latency
  mkWeight _ _ Nothing          = Loss
  mkWeight _ _ (Just (Out _))   = Loss
  mkWeight _ _ (Just (In  _))   = Loss
  mkWeight _ _ (Just (Uni (Semigroup.Last o) (Semigroup.Last i))) =
    let pattern = tcpRPCLoadPattern i o pdu_overhead initial_window Nothing request_size response_size
    in  TX $ foldr (+) 0 (fmap (toRational . fst) pattern)
    -- If we wanted to ignore TCP and just use the delta Q
    --TX $ toRational (dqG (linkDeltaQ o)) + toRational (dqS (linkDeltaQ i) request_size)
    --   + toRational (dqG (linkDeltaQ i)) + toRational (dqS (linkDeltaQ i) response_size)

  request_size :: Natural
  request_size = 256

  pdu_overhead :: Natural
  pdu_overhead = 20

  initial_window :: Natural
  initial_window = 14000

  aws_graph :: AdjacencyMap (Maybe (UEdge (Semigroup.Last BearerCharacteristics))) NetNode
  aws_graph = overlay
    (emap mkOut aws')
    (emap mkIn  (transpose aws'))

  mkOut :: Edges BearerCharacteristics -> Maybe (UEdge (Semigroup.Last BearerCharacteristics))
  mkOut (Edges [])    = Nothing
  mkOut (Edges (e:_)) = Just (Out (Semigroup.Last e))

  mkIn :: Edges BearerCharacteristics -> Maybe (UEdge (Semigroup.Last BearerCharacteristics))
  mkIn (Edges [])    = Nothing
  mkIn (Edges (e:_)) = Just (In (Semigroup.Last e))

-- | Restrictions to add to the AWS graph. 
-- Note the edge type: there can be _at most one_ link restriction between
-- any two nodes, and the last one takes precedence in case of duplicates.
restrictions :: AdjacencyMap (Last LinkRestriction) NetNode
restrictions = overlays
  [ GL10       -< Last (Just (mkRestriction  7e6 1492)) >- IrelandAWS
  , IrelandAWS -< Last (Just (mkRestriction 39e6 1492)) >- GL10
  ]

addRestriction :: LinkRestriction -- ^ default link restriction.
               -> AdjacencyMap (Last LinkRestriction) NetNode
               -> NetNode
               -> NetNode
               -> SimpleGS
               -> BearerCharacteristics
addRestriction dft tr a b gs = case mlr of
    Last Nothing   -> Bearer gs dft
    Last (Just lr) -> Bearer gs lr
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
    -- FIXME non-exhaustive patterns; there can in fact be more than one
    -- edge from a to b or b to a; tcpRPCLoadPatterns should be run on
    -- each of them? What if there is no edge?
    Edges [a2b] = edgeLabel a b gr
    Edges [b2a] = edgeLabel b a gr

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
