{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.NodeId (
    -- * Node IDs
    CoreNodeId (..)
  , NodeId (..)
  , fromCoreNodeId
  ) where

import           Codec.Serialise (Serialise)
import           Data.Hashable
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet

import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

-- TODO: It is not at all clear that this makes any sense anymore. The network
-- layer does not use or provide node ids (it uses addresses).
data NodeId = CoreId !CoreNodeId
            | RelayId !Word64
  deriving (Eq, Ord, Show, Generic, NoThunks)

instance Condense NodeId where
  condense (CoreId (CoreNodeId i)) = "c" ++ show i
  condense (RelayId            i ) = "r" ++ show i

instance Hashable NodeId

-- | Core node ID
newtype CoreNodeId = CoreNodeId {
      unCoreNodeId :: Word64
    }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Condense, Serialise, NoThunks)
  deriving Show via Quiet CoreNodeId

instance Hashable CoreNodeId

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId = CoreId
