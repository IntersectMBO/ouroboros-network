{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers      #-}

module Ouroboros.Network.ConnectionId where

import NoThunks.Class (InspectHeap (..), NoThunks)

import Data.Aeson qualified as Aeson
import Data.Hashable
import Data.String (fromString)
import GHC.Generics (Generic)
import Ouroboros.Network.Util.ShowProxy (Proxy (..), ShowProxy (..))


-- | Connection is identified by local and remote address.
--
-- TODO: the type variable which this data type fills in is called `peerid`.  We
-- should renamed to `connectionId`.
--
data ConnectionId addr = ConnectionId {
    localAddress  :: !addr,
    remoteAddress :: !addr
  }
  deriving (Eq, Show, Generic)
  deriving NoThunks via InspectHeap (ConnectionId addr)
  deriving Functor

instance Aeson.ToJSON addr => Aeson.ToJSONKey (ConnectionId addr) where
instance Aeson.ToJSON addr => Aeson.ToJSON (ConnectionId addr) where
    toEncoding ConnectionId {remoteAddress, localAddress} =
      Aeson.pairs $
           fromString "remoteAddress" Aeson..= remoteAddress
        <> fromString "localAddress"  Aeson..= localAddress


-- | Order first by `remoteAddress` then by `localAddress`.
--
-- /Note:/ we relay on the fact that `remoteAddress` is an order
-- preserving map (which allows us to use `Map.mapKeysMonotonic` in some
-- cases.  We also relay on this particular order in
-- `Ouroboros.Network.ConnectionManager.State.liveConnections`
--
instance Ord addr => Ord (ConnectionId addr) where
    conn `compare` conn' =
         remoteAddress conn `compare` remoteAddress conn'
      <> localAddress conn `compare` localAddress conn'

instance Hashable a => Hashable (ConnectionId a)

instance forall addr. ShowProxy addr => ShowProxy (ConnectionId addr) where
  showProxy _ = "ConnectionId " ++ showProxy (Proxy :: Proxy addr)
