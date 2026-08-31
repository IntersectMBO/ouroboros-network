-- | Statistical read-outs for a generated topology, so the stake→connectivity
-- bias introduced by churn can be sanity-checked (see design.md).
module SmallWorld.Metrics
  ( clusterStakeInboundCorr
  , rttToQuorum
  , topStakeNodes
  ) where

import           Data.List          (foldl', sortBy)
import           Data.Ord           (comparing, Down(..))
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import qualified Data.Map.Strict    as M
import qualified Data.Vector        as V

import           SmallWorld.Types

-- | Pearson correlation between a cluster's total stake and its inbound-edge
-- count.  Rises toward 1 as churn concentrates connectivity on rich clusters.
clusterStakeInboundCorr :: Int -> Topology -> Double
clusterStakeInboundCorr nClusters topo = pearson xs ys
  where
    nodes     = V.toList (topoNodes topo)
    stakeByCl = IM.fromListWith (+) [ (nCluster nd, nStake nd) | nd <- nodes ]
    inByCl    = IM.fromListWith (+)
                  [ (nCluster (topoNodes topo V.! eTo e), 1 :: Int)
                  | u <- [0 .. nNodes topo - 1], e <- topoOut topo V.! u ]
    xs = [ IM.findWithDefault 0 c stakeByCl             | c <- [0 .. nClusters - 1] ]
    ys = [ fromIntegral (IM.findWithDefault 0 c inByCl) | c <- [0 .. nClusters - 1] ]

pearson :: [Double] -> [Double] -> Double
pearson xs ys
  | vx == 0 || vy == 0 = 0
  | otherwise          = cov / sqrt (vx * vy)
  where
    n   = fromIntegral (length xs)
    mx  = sum xs / n
    my  = sum ys / n
    cov = sum (zipWith (\x y -> (x - mx) * (y - my)) xs ys)
    vx  = sum (map (\x -> (x - mx) * (x - mx)) xs)
    vy  = sum (map (\y -> (y - my) * (y - my)) ys)

-- | The @k@ highest-stake node ids (the producers that matter most).
topStakeNodes :: Int -> Topology -> [Int]
topStakeNodes k topo =
  take k $ map fst $ sortBy (comparing (Down . snd))
    [ (i, nStake (topoNodes topo V.! i)) | i <- [0 .. nNodes topo - 1] ]

-- | Mean latency (ms) at which cumulative reached stake first crosses @q@,
-- averaged over the given producers, on the undirected RTT-weighted graph.
-- A pre-Phase-2 proxy: min-RTT spread only — no bandwidth or loss yet.
rttToQuorum :: Double -> [Int] -> Topology -> Double
rttToQuorum q srcs topo =
  let ds     = [ dijkstraQuorum q s topo | s <- srcs ]
      finite = filter (not . isInfinite) ds
  in if null finite then 1 / 0 else sum finite / fromIntegral (length finite)

undirW :: Topology -> V.Vector [(Int, Double)]
undirW topo =
  let n = nNodes topo
      m = IM.fromListWith (++)
            (concat [ [(u, [(eTo e, eRttMs e)]), (eTo e, [(u, eRttMs e)])]
                    | u <- [0 .. n - 1], e <- topoOut topo V.! u ])
  in V.generate n (\i -> IM.findWithDefault [] i m)

dijkstraQuorum :: Double -> Int -> Topology -> Double
dijkstraQuorum q src topo = loop IS.empty (M.singleton (0, src) ()) 0
  where
    adj       = undirW topo
    stakeOf i = nStake (topoNodes topo V.! i)
    loop visited pq acc = case M.minViewWithKey pq of
      Nothing -> 1 / 0
      Just (((d, u), _), pq')
        | u `IS.member` visited -> loop visited pq' acc
        | acc + stakeOf u >= q  -> d
        | otherwise ->
            let visited' = IS.insert u visited
                pq''     = foldl'
                             (\m (w, rtt) ->
                                if w `IS.member` visited' then m
                                else M.insert (d + rtt, w) () m)
                             pq' (adj V.! u)
            in loop visited' pq'' (acc + stakeOf u)
