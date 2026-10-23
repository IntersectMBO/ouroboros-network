-- | Core topology types.  RTT magnitudes are the improved-ΔQ report's Praos
-- Table 1 (short / medium / long), used for the three small-world tiers.
module SmallWorld.Types
  ( NodeId
  , Tier(..)
  , tierRange
  , tierRttMs
  , Node(..)
  , Edge(..)
  , Topology(..)
  , nNodes
  ) where

import qualified Data.Vector as V

type NodeId = Int

-- | RTT tier.  Only three magnitudes are justified (report Praos Table 1), so
-- the two RTT-deciding boundaries are cluster and continent: same cluster is
-- local, anything else within a continent is intra-continental, and crossing a
-- continent is the long tier.  (Region is a structural sub-level used for link
-- locality, D2 — same-region links are correctly intra-continental at 69 ms, so
-- it does not get its own magnitude.)
data Tier = IntraCluster | IntraContinent | InterContinent
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | RTT range (ms) for a tier, sampled per cluster-pair at topology generation
-- (a realistic band rather than a single point). Bands touch at 50/125 so a
-- higher tier is always ≥ a lower one. Rough guide to the literature: local
-- peer-to-peer 10–50, intra-continental 50–125, inter-continental 125–250.
tierRange :: Tier -> (Double, Double)
tierRange IntraCluster   = (10, 50)
tierRange IntraContinent = (50, 125)
tierRange InterContinent = (125, 250)

-- | A representative scalar RTT for a tier (the band midpoint) — used for
-- topology read-outs and the churn-attractiveness score, not for diffusion
-- (which uses the per-cluster-pair sample on each 'Edge').
tierRttMs :: Tier -> Double
tierRttMs t = let (lo, hi) = tierRange t in (lo + hi) / 2

data Node = Node
  { nId        :: !NodeId
  , nStake     :: !Double   -- fractional; stakes over the topology sum to 1
  , nCluster   :: !Int
  , nRegion    :: !Int
  , nContinent :: !Int
  } deriving (Show)

data Edge = Edge
  { eTo    :: !NodeId
  , eTier  :: !Tier
  , eRttMs :: !Double
  } deriving (Show)

data Topology = Topology
  { topoNodes :: !(V.Vector Node)     -- indexed by NodeId
  , topoOut   :: !(V.Vector [Edge])   -- outbound adjacency, indexed by NodeId
  , topoInDeg :: !(V.Vector Int)      -- inbound degree (derived), by NodeId
  } deriving (Show)

nNodes :: Topology -> Int
nNodes = V.length . topoNodes
