{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE StaticPointers      #-}

module Ouroboros.Network.ConnectionId where

import NoThunks.Class (InspectHeap (..), NoThunks)

import Data.Hashable
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
  deriving (Eq, Ord, Show, Generic)
  deriving NoThunks via InspectHeap (ConnectionId addr)
  deriving Functor

instance Hashable a => Hashable (ConnectionId a)

instance forall addr. ShowProxy addr => ShowProxy (ConnectionId addr) where
  showProxy _ = "ConnectionId " ++ showProxy (Proxy :: Proxy addr)
