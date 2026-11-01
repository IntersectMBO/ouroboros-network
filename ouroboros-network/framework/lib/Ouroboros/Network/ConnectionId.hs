{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ouroboros.Network.ConnectionId where

import NoThunks.Class (InspectHeap (..), NoThunks)

import Data.Hashable
import GHC.Generics (Generic)
import Ouroboros.Network.Util (PrettyShow (..), Proxy (..), ShowProxy (..))


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

instance PrettyShow addr => PrettyShow (ConnectionId addr) where
  prettyShow ConnectionId {localAddress, remoteAddress} =
    prettyShow localAddress ++ "-" ++ prettyShow remoteAddress

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


-- | A unique identifier of a connection.
--
-- It's used even when we don't yet know `ConnectionId`.
--
-- Successive connections between two nodes often have the same 'ConnectionId'.
-- 'ConnStateId', on the other hand, is never reused. Thus "per-peer state" (eg
-- shared/maintained by the various mini protocols) should be indexed by
-- 'ConnStateId' instead of 'ConnectionId'.
--
-- The connection manager draws a fresh one for every connection it creates,
-- inbound or outbound.
--
newtype ConnStateId = ConnStateId Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Enum, NoThunks)

instance PrettyShow ConnStateId where
  prettyShow (ConnStateId connStateId) = show connStateId
