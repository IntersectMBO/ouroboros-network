module Ouroboros.Consensus.Ledger.Mock.Stake (
    -- * Stakeholders
    StakeHolder(..)
    -- * Address distribution
  , AddrDist
    -- * Stake distribution
  , StakeDist(..)
  , stakeWithDefault
  , relativeStakes
  , totalStakes
  , equalStakeDist
  , genesisStakeDist
  ) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)

import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.UTxO
import           Ouroboros.Consensus.NodeId (NodeId (..))

{-------------------------------------------------------------------------------
  Stakeholders
-------------------------------------------------------------------------------}

data StakeHolder =
    -- | Stake of a core node
    StakeCore Int

    -- | Stake for everybody else (we don't need to distinguish)
  | StakeEverybodyElse
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Stake distribution
-------------------------------------------------------------------------------}

newtype StakeDist = StakeDist { stakeDistToIntMap :: IntMap Rational }
  deriving (Show)

stakeWithDefault :: Rational -> Int -> StakeDist -> Rational
stakeWithDefault d n = IntMap.findWithDefault d n . stakeDistToIntMap

relativeStakes :: Map StakeHolder Int -> StakeDist
relativeStakes m = StakeDist $
   let totalStake    = fromIntegral $ sum $ Map.elems m
   in  IntMap.fromList [ (nid, fromIntegral stake / totalStake)
                       | (StakeCore nid, stake) <- Map.toList m
                       ]

-- | Compute stakes of all nodes
--
-- The 'Nothing' value holds the total stake of all addresses that don't
-- get mapped to a NodeId.
totalStakes :: Map Addr NodeId -> Utxo -> Map StakeHolder Int
totalStakes addrDist = foldl f Map.empty
 where
   f :: Map StakeHolder Int -> TxOut -> Map StakeHolder Int
   f m (a, stake) = case Map.lookup a addrDist of
       Just (CoreId nid) -> Map.insertWith (+) (StakeCore nid)    stake m
       _                 -> Map.insertWith (+) StakeEverybodyElse stake m

-- | Stake distribution where every address has equal state
equalStakeDist :: AddrDist -> StakeDist
equalStakeDist = StakeDist
               . IntMap.fromList
               . mapMaybe (nodeStake . snd)
               . Map.toList
  where
    nodeStake :: NodeId -> Maybe (Int, Rational)
    nodeStake (RelayId _) = Nothing
    nodeStake (CoreId i)  = Just (i, 1)

-- | Genesis stake distribution
genesisStakeDist :: AddrDist -> StakeDist
genesisStakeDist addrDist =
    relativeStakes (totalStakes addrDist (genesisUtxo addrDist))
