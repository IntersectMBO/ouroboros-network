{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.NodeId (
    -- * Node IDs
    CoreNodeId (..)
  , NodeId (..)
  , fromCoreNodeId
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.Hashable
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Quiet

import           Cardano.Binary

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
  fromCBOR = do
    len <- decodeListLen
    tag <- decodeWord8
    case (len, tag) of
      (2, 0) -> CoreId <$> fromCBOR @CoreNodeId
      (2, 1) -> RelayId <$> fromCBOR @Word64
      _      -> fail $ "NodeId: unknown (len, tag) " ++ show (len, tag)

instance ToCBOR NodeId where
    toCBOR nodeId = case nodeId of
      CoreId x  -> encodeListLen 2 <> encodeWord8 0 <> toCBOR x
      RelayId x -> encodeListLen 2 <> encodeWord8 1 <> toCBOR x

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
