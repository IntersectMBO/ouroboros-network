{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE LambdaCase                 #-}

module Ouroboros.Consensus.NodeId
  ( -- * Node IDs
    CoreNodeId (..)
  , NodeId (..)
  , fromCoreNodeId
  ) where

import           Cardano.Binary
import           Codec.Serialise(Serialise(..))
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

instance FromCBOR NodeId where
  fromCBOR = decodeTag >>= \case
    0   -> CoreId <$> fromCBOR @CoreNodeId
    1   -> RelayId <$> fromCBOR @Word64
    tag -> fail $ "NodeId: unknown tag " ++ show tag

instance ToCBOR NodeId where
    toCBOR nodeId = case nodeId of
      CoreId x  -> encodeTag 0 <> toCBOR x
      RelayId x -> encodeTag 1 <> toCBOR x

instance Serialise NodeId where
  decode = fromCBOR
  encode = toCBOR

instance Condense NodeId where
  condense (CoreId (CoreNodeId i)) = "c" ++ show i
  condense (RelayId            i ) = "r" ++ show i

instance Hashable NodeId

-- | Core node ID
newtype CoreNodeId = CoreNodeId {
      unCoreNodeId :: Word64
    }
  deriving stock   (Eq, Ord, Generic)
  deriving newtype (Condense, FromCBOR, ToCBOR, NoThunks)
  deriving Show via Quiet CoreNodeId

instance Hashable CoreNodeId

instance Serialise CoreNodeId where
  decode = fromCBOR
  encode = toCBOR

fromCoreNodeId :: CoreNodeId -> NodeId
fromCoreNodeId = CoreId
