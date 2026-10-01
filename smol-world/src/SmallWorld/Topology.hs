-- | Stratified small-world topology generation, with graded long-range links
-- and a static "churn relaxation" that approximates Cardano's peer-churn steady
-- state (see design.md, "Design decisions").
module SmallWorld.Topology
  ( TopoParams(..)
  , defaultParams
  , genTopology
  , clusterStakes
  , tierBetween
  ) where

import           Data.List          (foldl', sortBy)
import           Data.Ord           (comparing)
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector        as V
import           System.Random      (RandomGen, mkStdGen, randomR)

import           SmallWorld.Rand    (hashUnit, mix)
import           SmallWorld.Stake   (lognormalStakes)
import           SmallWorld.Types

data TopoParams = TopoParams
  { tpNodes      :: !Int
  , tpClusters   :: !Int
  , tpRegions    :: !Int
  , tpContinents :: !Int
  , tpValency    :: !Int      -- outbound connections per node
  , tpLocalFrac  :: !Double   -- fraction of outbound kept intra-cluster
  , tpRegionFrac :: !Double   -- of the long-range links, fraction kept same-region
  , tpHubBias    :: !Double   -- initial target selection ∝ stake^hubBias
  , tpStakeSigma :: !Double   -- log-normal stake dispersion
  , tpChurnRounds :: !Int     -- static churn-relaxation rounds (0 = raw random)
  , tpChurnFrac  :: !Double   -- fraction of each node's links rewired per round
  , tpRttFuzz    :: !Double   -- static per-edge RTT fuzz (± tpRttFuzz/2 around the
                              --   cluster-pair base), so node pairs vary a little
  , tpSeed       :: !Int
  } deriving (Show)

-- | Defaults at a realistic operating point (valency ~ real Cardano ~20–30).
defaultParams :: TopoParams
defaultParams = TopoParams
  { tpNodes = 750, tpClusters = 30, tpRegions = 6, tpContinents = 3
  , tpValency = 20, tpLocalFrac = 0.6, tpRegionFrac = 0.5, tpHubBias = 1.0
  , tpStakeSigma = 1.2, tpChurnRounds = 0, tpChurnFrac = 0.2, tpRttFuzz = 0.1
  , tpSeed = 42 }

groupOf :: Int -> Int -> Int -> Int
groupOf n k i = (i * k) `div` n

pickRemove :: RandomGen g => [(a, Double)] -> g -> Maybe (a, [(a, Double)], g)
pickRemove [] _ = Nothing
pickRemove cands g
  | total <= 0 = Nothing
  | otherwise  = Just (sel 0 [] cands)
  where
    total = sum (map snd cands)
    (r, g') = randomR (0, total) g
    sel _   before []              = pickLast before
    sel acc before (c@(x, w) : after)
      | null after || acc + w >= r = (x, reverse before ++ after, g')
      | otherwise                  = sel (acc + w) (c : before) after
    pickLast before = case reverse before of
      ((x, _) : rest) -> (x, reverse rest, g')
      []              -> error "pickRemove: empty"

sampleK :: RandomGen g => Int -> [(a, Double)] -> g -> ([a], g)
sampleK k0 cands0 g0 = go k0 cands0 g0 []
  where
    go k cands g acc
      | k <= 0    = (acc, g)
      | otherwise = case pickRemove cands g of
          Nothing            -> (acc, g)
          Just (x, rest, g') -> go (k - 1) rest g' (x : acc)

-- | Total stake per cluster, indexed by cluster id.
clusterStakes :: TopoParams -> Topology -> V.Vector Double
clusterStakes tp topo =
  let m = IM.fromListWith (+)
            [ (nCluster nd, nStake nd) | nd <- V.toList (topoNodes topo) ]
  in V.generate (tpClusters tp) (\c -> IM.findWithDefault 0 c m)

-- | RTT tier between two nodes, decided by cluster then continent (region shapes
-- link locality but not its own RTT magnitude — see 'Tier').
tierBetween :: V.Vector Int -> V.Vector Int -> NodeId -> NodeId -> Tier
tierBetween clusterV contV u w
  | clusterV V.! u == clusterV V.! w = IntraCluster
  | contV    V.! u == contV    V.! w = IntraContinent
  | otherwise                        = InterContinent

genTopology :: TopoParams -> Topology
genTopology tp
  -- Input guards (TCP-audit Part 4): the group denominators feed `groupOf`'s
  -- `div`, and a zero there divides by zero; a negative/empty node count yields
  -- an empty topology whose downstream means are NaN.  Fail fast with a clear
  -- message rather than crash deep in the graph build.
  | tpNodes tp      < 1 = errorWithoutStackTrace "--nodes must be >= 1"
  | tpClusters tp   < 1 = errorWithoutStackTrace "--clusters must be >= 1 (it is a groupOf divisor)"
  | tpRegions tp    < 1 = errorWithoutStackTrace "--regions must be >= 1 (it is a groupOf divisor)"
  | tpContinents tp < 1 = errorWithoutStackTrace "--continents must be >= 1"
  | tpRttFuzz tp < 0 || tpRttFuzz tp > 2 =
      errorWithoutStackTrace "--rtt-fuzz must be in [0, 2] (beyond 2 the fuzz drives RTTs negative)"
  | otherwise = Topology nodesV outV inDegV
  where
    n  = tpNodes tp
    g0 = mkStdGen (tpSeed tp)
    (stakes, g1) = lognormalStakes n (tpStakeSigma tp) g0
    stakeV = V.fromList stakes

    clusterV = V.generate n (\i -> groupOf n (tpClusters tp) i)
    regionV  = V.generate n (\i -> groupOf (tpClusters tp) (tpRegions tp) (clusterV V.! i))
    contV    = V.generate n (\i -> groupOf (tpRegions tp) (tpContinents tp) (regionV V.! i))
    nodesV   = V.generate n (\i -> Node i (stakeV V.! i) (clusterV V.! i) (regionV V.! i) (contV V.! i))

    weight i = (stakeV V.! i) ** tpHubBias tp

    membersByCluster = IM.fromListWith (++) [ (clusterV V.! i, [i]) | i <- [0 .. n - 1] ]
    membersOf c = IM.findWithDefault [] c membersByCluster
    allNodes = [0 .. n - 1]

    tier   = tierBetween clusterV contV
    rttOf u w = tierRttMs (tier u w)   -- scalar (midpoint) for the churn score only

    -- ---- cluster-pair base RTT (the coherence constraint) ----------------- --
    -- One RTT sample per cluster PAIR, from the tier's range: every node in
    -- cluster A is that base away from every node in cluster B (nodes inherit
    -- their cluster's inter-cluster distances).  Sampled with a dedicated seed so
    -- stakes and links are byte-identical to the point-RTT model.
    nC = tpClusters tp
    clusterCont c = groupOf (tpRegions tp) (tpContinents tp)
                            (groupOf (tpClusters tp) (tpRegions tp) c)
    cprTierC ci cj
      | ci == cj                         = IntraCluster
      | clusterCont ci == clusterCont cj = IntraContinent
      | otherwise                        = InterContinent
    cprKey ci cj = min ci cj * nC + max ci cj
    cprMap =
      let pairs = [ (i, j) | i <- [0 .. nC - 1], j <- [i .. nC - 1] ]
          step (m, g) (i, j) =
            let (lo, hi) = tierRange (cprTierC i j)
                (x, g')  = randomR (lo, hi) g
            in (IM.insert (cprKey i j) x m, g')
      in fst (foldl' step (IM.empty, mkStdGen (tpSeed tp + 777)) pairs)
    cprBase ci cj = IM.findWithDefault 0 (cprKey ci cj) cprMap
    -- small static per-edge fuzz so node pairs across a cluster pair are not
    -- identical; dynamic per-exchange variation is the D17 jitter model (Diffusion)
    edgeRtt u w =
      let base = cprBase (clusterV V.! u) (clusterV V.! w)
          fz   = hashUnit (mix [tpSeed tp, u, w, 5])
      in base * (1 + tpRttFuzz tp * (fz - 0.5))

    -- cluster stake share for churn scoring (recomputed on the raw stakes)
    clStake = let m = IM.fromListWith (+) [ (clusterV V.! i, stakeV V.! i) | i <- allNodes ]
              in \c -> IM.findWithDefault 0 c m

    -- ---- initial graded targets ---------------------------------------- --
    initStep (g, acc) u =
      let c   = clusterV V.! u
          r   = regionV  V.! u
          local = [ (i, weight i) | i <- membersOf c, i /= u ]
          sameR = [ (i, weight i) | i <- allNodes, regionV V.! i == r, clusterV V.! i /= c ]
          crossR = [ (i, weight i) | i <- allNodes, regionV V.! i /= r ]
          nLocal = min (round (tpLocalFrac tp * fromIntegral (tpValency tp))) (length local)
          nFar   = tpValency tp - nLocal
          nSame  = min (round (tpRegionFrac tp * fromIntegral nFar)) (length sameR)
          nCross = min (nFar - nSame) (length crossR)
          (tL, gA) = sampleK nLocal local g
          (tS, gB) = sampleK nSame  sameR gA
          (tC, gC) = sampleK nCross crossR gB
      in (gC, (u, tL ++ tS ++ tC) : acc)
    (gInit, initAssoc) = foldl' initStep (g1, []) allNodes
    targets0 = V.generate n (\i -> IM.findWithDefault [] i (IM.fromList initAssoc))

    -- ---- static churn relaxation --------------------------------------- --
    -- attractiveness: high-stake cluster, low RTT (fast block delivery proxy)
    attract u w = clStake (clusterV V.! w) / rttOf u w
    churnRound (g, tsV) =
      let stepC (gg, acc) u =
            let ts     = tsV V.! u
                nDrop  = ceiling (tpChurnFrac tp * fromIntegral (length ts)) :: Int
                worstF = sortBy (comparing (attract u)) ts       -- ascending attractiveness
                kept   = drop nDrop worstF                        -- keep the best
                keptS  = IM.fromList [ (k, ()) | k <- kept ]
                cands  = [ (w, attract u w)
                         | w <- allNodes, w /= u, not (w `IM.member` keptS) ]
                (adds, gg') = sampleK nDrop cands gg
            in (gg', (u, kept ++ adds) : acc)
          (g', assoc) = foldl' stepC (g, []) allNodes
      in (g', V.generate n (\i -> IM.findWithDefault [] i (IM.fromList assoc)))

    (_, targetsF) =
      foldl' (\st _ -> churnRound st) (gInit, targets0) [1 .. tpChurnRounds tp]

    -- ---- edges + in-degree from final targets -------------------------- --
    mkEdge u w = Edge w (tier u w) (edgeRtt u w)
    outV   = V.generate n (\u -> map (mkEdge u) (targetsF V.! u))
    inMap  = IM.fromListWith (+) [ (eTo e, 1 :: Int) | u <- allNodes, e <- outV V.! u ]
    inDegV = V.generate n (\i -> IM.findWithDefault 0 i inMap)
