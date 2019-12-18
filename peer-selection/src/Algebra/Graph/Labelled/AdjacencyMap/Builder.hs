{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Algebra.Graph.Labelled.AdjacencyMap.Builder
  ( Builder
  , buildIO
  , buildFromSeed
  , build
  , Vertex (..)
  , VertexRep
  , Directed (..)
  , directed
  , directedEdge
  , Undirected (..)
  , undirected
  , undirectedEdge
  , mirror
  , component
  , overlay
  , freshVertex
  , freshVertices
  , setLabel
  , getLabel
  , outEdges
  , edge
  , pathOn
  , cycleOn
  , cycleWithShortcutsOn
  , kregular
  , degrees
  , isRegular
  , cylinder

  , Random (..)
  , randomST
  , random
  , uniform
  , normal
  , exponential
  , randomGS
  , shuffle
  , pickN
  , pickNPairs
  ) where

import Algebra.Graph.Labelled.AdjacencyMap (AdjacencyMap (..))
import qualified Algebra.Graph.Labelled.AdjacencyMap as GR
import Control.Monad (ap, forM, forM_, when)
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Foldable (foldlM)
import qualified Data.List as List (head, last, splitAt)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup (Last (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (picosecondsToDiffTime)
import qualified Data.Vector as Vector (fromList, toList)
import Data.Word (Word32)
import Numeric.Natural (Natural)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC
import qualified Control.Monad.Primitive as MWC (PrimMonad, PrimState)

import DeltaQ.SimpleGS

-- | A monad for pseudorandom values.
newtype Random t = Random
  { runRandom :: forall m . MWC.PrimMonad m => MWC.Gen (MWC.PrimState m) -> m t }

instance Functor Random where
  fmap f rand = Random $ \gen -> fmap f (runRandom rand gen)

instance Applicative Random where
  pure x = Random (const (pure x))
  (<*>) = ap

instance Monad Random where
  return = pure
  rand >>= k = Random $ \gen -> do
    t <- runRandom rand gen
    runRandom (k t) gen

randomST :: MWC.GenST s -> Random t -> ST s t
randomST = flip runRandom

-- | Uniformly-distributed pseudorandom value within the given bounds.
uniform :: MWC.Variate t => t -> t -> Random t
uniform low high = Random $ MWC.uniformR (low, high)

-- | Normally-distributed pseudorandom double with given mean and standard
-- deviation (in that order).
normal :: Double -> Double -> Random Double
normal mean stdDev = Random $ MWC.normal mean stdDev

-- | Exponentially-distributed pseudorandom double with given parameter.
exponential :: Double -> Random Double
exponential lambda = Random $ MWC.exponential lambda

-- | Shuffle a list randomly. Useful for getting a random order of vertices, for
-- example.
shuffle :: [v] -> Random [v]
shuffle vs = Random $ \gen ->
  fmap Vector.toList (MWC.uniformShuffle (Vector.fromList vs) gen)

-- | Pick n elements from a set without replacement.
-- If the set has fewer than the number, then you get the whole set as a list.
pickN :: Natural -> Set v -> Random [v]
pickN n set = go n (Set.size set) [] (Set.toList set)
  where
  go :: Natural -> Int -> [v] -> [v] -> Random [v]
  go n m vs lst | n == 0    = pure vs
                | m == 0    = pure vs
                | otherwise = do
                    idx <- Random $ MWC.uniformR (0, m-1)
                    let (prefix, v:suffix) = List.splitAt idx lst
                    go (n-1) (m-1) (v:vs) (prefix ++ suffix)

-- | Pick n pairs randomly from a set. If the set does not have at least 2*n
-- elements, then fewer than n pairs are returned.
pickNPairs :: Natural -> Set v -> Random [(v, v)]
pickNPairs n set = fmap collate (pickN (2 * n) set)
  where
  collate :: [v] -> [(v,v)]
  collate []       = []
  collate [_]      = []
  collate (v:w:vs) = (v,w):collate vs

-- | Use random picoseconds for G and S to get a random G/S.
randomGS :: Random Integer -> Random Integer -> Random SimpleGS
randomGS rg rs = mk <$> rg <*> rs
  where
  mk g s = mkGS (picosecondsToDiffTime g) (picosecondsToDiffTime s)

type MakeEdge f e v = f e -> Vertex v -> Vertex v -> AdjacencyMap (Last e) (Vertex v)

type VertexRep = Word32

-- | The @v@ plays a role similar to that of the phantom type in @STRef@.
data Vertex v = Vertex { getVertexRep :: !Word32 }
  deriving (Eq, Ord)

data BuilderState s r e v = BuilderState
  { builderNextVertex     :: !VertexRep
  , builderVertexLabels   :: !(Map VertexRep v)
  , builderGraph          :: !(AdjacencyMap (Last e) (Vertex r))
  , builderEntropy        :: !(MWC.Gen s)
  }

-- | Used to monadically construct graphs with pseudorandomness available.
type Builder s r f e v = ReaderT (MakeEdge f e r) (StateT (BuilderState s r e v) (ST s))

-- | Use pseudorandomness in a 'Builder'.
random :: Random t -> Builder s r f e v t
random k = do
  -- No need to explicitly update the state. It's done automatically in ST.
  gen <- lift $ gets builderEntropy
  runRandom k gen

newtype NoEdges t = NoEdges (forall e . e)

noMakeEdge :: forall e v . MakeEdge NoEdges e v
noMakeEdge (NoEdges impossible) = impossible

newtype Directed e = DirectedEdge { getDirectedEdge :: e }

makeDirectedEdge :: MakeEdge Directed e r
makeDirectedEdge (DirectedEdge e) u v = GR.edge (Last e) u v

newtype Undirected e = UndirectedEdge { getUndirectedEdge :: (e, e) }

makeUndirectedEdge :: MakeEdge Undirected e r
makeUndirectedEdge (UndirectedEdge (forward, backward)) u v = GR.overlay
  (GR.edge (Last forward)  u v)
  (GR.edge (Last backward) v u)

type VertexLabels v = VertexRep -> Maybe v

-- | Run a builder using system entropy. The seed is also returned so that you
-- can reproduce it (using 'buildFromSeed').
buildIO
  :: (forall s f . Builder s r f e v x)
  -> IO ((AdjacencyMap (Last e) VertexRep, VertexLabels v, x), MWC.Seed)
buildIO it = MWC.withSystemRandom $ \gen -> do
  seed <- MWC.save gen
  outcome <- build gen it
  pure (outcome, seed)

-- | Like 'buildIO' but you give the seed. Useful if you want to reproduce
-- something that was produced randomly ('buildIO' gives the seed that will
-- reproduce it).
buildFromSeed
  :: MWC.Seed
  -> (forall s f . Builder s r f e v x)
  -> ST s (AdjacencyMap (Last e) VertexRep, VertexLabels v, x)
buildFromSeed seed it = do
  gen <- MWC.restore seed
  build gen it

-- | Run a builder with a given source of randomness.
-- Vertices are @Word32@s.
build
  :: MWC.Gen s
  -> (forall s f . Builder s r f e v x)
  -> ST s (AdjacencyMap (Last e) VertexRep, VertexLabels v, x)
build gen it = do
  let st = BuilderState 0 Map.empty GR.empty gen
  (x, st') <- runStateT (runReaderT it noMakeEdge) st
  pure (GR.gmap getVertexRep (builderGraph st'), flip Map.lookup (builderVertexLabels st'), x)

directed :: Builder s r Directed e v t -> Builder s r f e v t
directed bdr = lift $ runReaderT bdr makeDirectedEdge

undirected :: Builder s r Undirected e v t -> Builder s r f e v t
undirected bdr = lift $ runReaderT bdr makeUndirectedEdge

-- | Make a directed graph undirected by copying each edge in reverse.
mirror :: Builder s r Directed e v t -> Builder s r Undirected e v t
mirror bdr = do
  mkDirected <- ask
  lift $ runReaderT bdr (\(DirectedEdge e) -> mkDirected (UndirectedEdge (e,e)))

overlay
  :: AdjacencyMap (Last e) (Vertex r)
  -> Builder s r f e v ()
overlay gr = lift $ modify $ \st ->
  st { builderGraph = GR.overlay (builderGraph st) gr }

-- | Make a new graph component: run the builder on an empty graph but with
-- the current unused vertex set, then overlay the component with the current
-- graph. The two parts will have no edges between them, since the vertex sets
-- are disjoint. The only vertices which can be used in the parameter Builder
-- _must_ have been generated within it, because of the universally quantified
-- vertex type. So this won't type check:
--
-- > should_not_compile :: Ord v => Builder s () v ()
-- > should_not_compile = do
-- >   v <- freshVertex
-- >   component $ do
-- >     w <- freshVertex
-- >     overlay (edge () v w)
--
component :: (forall q . Builder s q f e v t) -> Builder s r f e v t
component bdr = do
  rdr <- ask
  st <- lift get
  (t, st') <- lift . lift $ runStateT (runReaderT bdr rdr) (st { builderGraph = GR.empty })
  let v  = builderNextVertex st'
      gr = GR.overlay (builderGraph st) (builderGraph st')
      st'' = st' { builderGraph = gr, builderNextVertex = v }
  lift $ put st''
  pure t

freshVertex :: Builder s r f e v (Vertex r)
freshVertex = do
  st <- lift get
  let v = builderNextVertex st
  lift $ put (st { builderNextVertex = succ v })
  overlay (GR.vertex (Vertex v))
  pure (Vertex v)

freshVertices :: Natural -> Builder s r f e v [Vertex r]
freshVertices n = forM [0..(n-1)] (const freshVertex)

setLabel :: Vertex r -> v -> Builder s r f e v ()
setLabel v lab = lift $ modify $ \st ->
  st { builderVertexLabels = Map.insert (getVertexRep v) lab (builderVertexLabels st) }

getLabel :: Vertex r -> Builder s r f e v (Maybe v)
getLabel v = lift $ gets $ \st -> Map.lookup (getVertexRep v) (builderVertexLabels st)

-- | Create an edge and include it in the graph.
edge :: f e -> Vertex r -> Vertex r -> Builder s r f e v ()
edge e u v = do
  mk <- ask
  overlay (mk e u v)

directedEdge :: e -> Vertex r -> Vertex r -> Builder s r Directed e v ()
directedEdge e = edge (DirectedEdge e)

undirectedEdge :: e -> Vertex r -> Vertex r -> Builder s r Undirected e v ()
undirectedEdge e = edge (UndirectedEdge (e, e))

outEdges :: Vertex r -> Builder s r f e v (Map (Vertex r) e)
outEdges v = do
  gr <- lift $ gets builderGraph
  pure (fmap getLast (GR.postSetEdges v gr))

-- | Make a path on a given list of vertices, with edges randomly generated
-- by the given function.
pathOn :: [Vertex r] -> Random e -> Builder s r Directed e v ()
pathOn vs randomEdge = forM_ (adjacentPairs vs) $ \(v, w) -> do
  e <- random randomEdge
  edge (DirectedEdge e) v w

-- | Pair up adjacenct elements in a list.
adjacentPairs :: [w] -> [(w,w)]
adjacentPairs []     = []
adjacentPairs (w:ws) = adjacentPairs' w ws
  where
  adjacentPairs' :: w -> [w] -> [(w,w)]
  adjacentPairs' u (w:us) = (u, w) : adjacentPairs' w us
  adjacentPairs' _ []     = []

-- | Make a cyle on a given list of vertices, with edges randomly generated
-- by the given function.
cycleOn :: [Vertex r] -> Random e -> Builder s r Directed e v ()
cycleOn vs randomEdge = case vs of
  []    -> pure ()
  (_:_) -> do
    pathOn vs randomEdge
    let start = List.head vs
        end   = List.last vs
    e <- random randomEdge
    edge (DirectedEdge e) end start

-- | 'cycleOn' and then "shortcut" edges added between randomly-selected
-- pairs which are not already adjacent in the cycle.
--
-- FIXME not yet right. Doesn't ensure the shortcuts are not duplicate
-- edges.
cycleWithShortcutsOn
  :: [Vertex r]
  -> Random e
  -> Random Natural -- ^ Number of shortcuts to put in.
  -> Builder s r Directed e v ()
cycleWithShortcutsOn vs randomEdge randomN = do
  cycleOn vs randomEdge
  pairs <- random $ do
    n <- randomN
    pickNPairs n (Set.fromList vs)
  forM_ pairs $ \(v, w) -> do
    e <- random randomEdge
    edge (DirectedEdge e) v w

-- | Make a directed k regular (our-degree) connected graph.
-- It's not necessarily connected.
--
-- NB: making this undirected, by way of 'mirror' for instance, will not
-- necessarily give a regular graph in the undirected sense.
--
-- NB: not the most general regular graph! It starts with a cycle on the
-- vertices, so that the graph is always connected.
kregular
  :: [Vertex r]
  -> Random Natural -- ^ Degree
  -> Random e
  -> Builder s r Directed e v ()
kregular vs randomK randomEdge = do
  cycleOn vs randomEdge
  let vset = Set.fromList vs
  forM_ vs $ \v -> do
    es <- outEdges v
    k <- random randomK
    let currentK :: Natural
        currentK = fromIntegral (Map.size es)
    -- This forM_ iteratively adds edges, so for each, we need to check that
    -- it does not already have enough edges.
    when (k > currentK) $ do
      let n = k - currentK
      -- Pick n vertices from the set of vertices not already reachable from
      -- this one, and not this one itself (no loops).
      ws <- random $ pickN n (Set.delete v (vset Set.\\ Map.keysSet es))
      forM_ ws $ \w -> do
        e <- random $ randomEdge
        edge (DirectedEdge e) v w

-- | Out-degrees of all vertices.
degrees :: Ord v => AdjacencyMap e v -> Map v Natural
degrees gr = fmap (fromIntegral . Map.size) (adjacencyMap gr)

-- | Check that the graph is (out-degree) regular.
isRegular :: Ord v => AdjacencyMap e v -> Maybe Natural
isRegular gr = case Map.toList (adjacencyMap gr) of
  [] -> Just 0
  ((_,es):vs) -> fmap fromIntegral (foldl go (Just (Map.size es)) (fmap snd vs))
    where
    go Nothing  _   = Nothing
    go (Just n) es' = if Map.size es' == n then Just n else Nothing
-- TODO we shall plot the candlesticks of the 95th percentile time to transmit
-- of a given number of samples _for each parameter_ where the parameter is
-- the size of the cycle/kregular graph.
--
-- TODO also, some way to visualize the likelihood that a random graph is
-- strongly connected

-- | Given a list of rows, each row is connected in a cycle, and every vertex
-- in that row is connected to the corresponding vertex in the previous row,
-- or nothing if the previous row was smaller.
cylinder :: [[Vertex r]] -> Random e -> Builder s r Directed e v ()
cylinder vss randomEdge = case vss of
  [] -> pure ()
  [vs] -> cycleOn vs randomEdge
  (vs:vs':vss') -> go vs vs' vss'
  where
  go vs vs' vss' = do
    cycleOn vs randomEdge
    -- Connect this cross-section to the next cross-section.
    forM_ (zip vs vs') $ \(v,v') -> do
      e <- random randomEdge
      edge (DirectedEdge e) v' v
    case vss' of
      [] -> cycleOn vs' randomEdge
      (vs'':vss'') -> go vs' vs'' vss''
