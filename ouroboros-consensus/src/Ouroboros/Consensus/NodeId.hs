{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.NodeId (
    -- * Node IDs
    NodeId (..)
  , CoreNodeId (..)
  , fromCoreNodeId
  ) where

import           Codec.Serialise (Serialise)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

-- TODO: It is not at all clear that this makes any sense anymore. The network
-- layer does not use or provide node ids (it uses addresses).
data NodeId = CoreId Int
            | RelayId Int
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ show i
  condense (RelayId i) = "r" ++ show i

-- | Core node ID
newtype CoreNodeId = CoreNodeId Int
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Condense, Serialise, NoUnexpectedThunks)

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId (CoreNodeId n) = CoreId n
