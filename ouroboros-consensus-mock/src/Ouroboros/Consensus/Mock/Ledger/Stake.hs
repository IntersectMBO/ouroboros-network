{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Mock.Ledger.Stake (
    -- * Stakeholders
    StakeHolder (..)
    -- * Address distribution
  , AddrDist
    -- * Stake distribution
  , StakeDist (..)
  , equalStakeDist
  , genesisStakeDist
  , relativeStakes
  , stakeWithDefault
  , totalStakes
    -- * Type family instances
  , Ticked (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           NoThunks.Class (NoThunks)

import           Ouroboros.Consensus.Mock.Ledger.Address
import           Ouroboros.Consensus.Mock.Ledger.UTxO
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Ticked

{-------------------------------------------------------------------------------
  Stakeholders
-------------------------------------------------------------------------------}

data StakeHolder =
    -- | Stake of a core node
    StakeCore CoreNodeId

    -- | Stake for everybody else (we don't need to distinguish)
  | StakeEverybodyElse
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Stake distribution
-------------------------------------------------------------------------------}

-- | In the mock setup, only core nodes have stake
--
-- INVARIANT: The rationals should sum to 1.
newtype StakeDist = StakeDist { stakeDistToMap :: Map CoreNodeId Rational }
  deriving (Show, Eq, Serialise, NoThunks)

-- | Ticked stake distribution
newtype instance Ticked StakeDist = TickedStakeDist {
      tickedStakeDistToMap :: Map CoreNodeId Rational
    }

stakeWithDefault :: Rational -> CoreNodeId -> StakeDist -> Rational
stakeWithDefault d n = Map.findWithDefault d n . stakeDistToMap

relativeStakes :: Map StakeHolder Amount -> StakeDist
relativeStakes m = StakeDist $
   let totalStake    = fromIntegral $ sum $ Map.elems m
   in  Map.fromList [ (nid, fromIntegral stake / totalStake)
                    | (StakeCore nid, stake) <- Map.toList m
                    ]

-- | Compute stakes of all nodes
--
-- The 'Nothing' value holds the total stake of all addresses that don't
-- get mapped to a NodeId.
totalStakes :: Map Addr NodeId -> Utxo -> Map StakeHolder Amount
totalStakes addrDist = foldl f Map.empty
 where
   f :: Map StakeHolder Amount -> TxOut -> Map StakeHolder Amount
   f m (a, stake) = case Map.lookup a addrDist of
       Just (CoreId nid) -> Map.insertWith (+) (StakeCore nid)    stake m
       _                 -> Map.insertWith (+) StakeEverybodyElse stake m

-- | Stake distribution where every address has equal state
equalStakeDist :: AddrDist -> StakeDist
equalStakeDist ad =
    StakeDist $
    Map.fromList $
    mapMaybe (nodeStake . snd) $
    Map.toList ad
  where
    nodeStake :: NodeId -> Maybe (CoreNodeId, Rational)
    nodeStake (RelayId _) = Nothing
    nodeStake (CoreId i)  = Just (i, recip (fromIntegral n))

    n = length $ filter isCore $ Map.elems ad

    isCore :: NodeId -> Bool
    isCore CoreId{}  = True
    isCore RelayId{} = False

-- | Genesis stake distribution
genesisStakeDist :: AddrDist -> StakeDist
genesisStakeDist addrDist =
    relativeStakes (totalStakes addrDist (genesisUtxo addrDist))
