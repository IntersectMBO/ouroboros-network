{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.ConnectionId where

import NoThunks.Class (InspectHeap (..), NoThunks)

import Data.Hashable
import GHC.Generics (Generic)
import Ouroboros.Network.Util (PrettyShow (..), Proxy (..), ShowProxy (..))


-- | Connection is identified by local and remote address.
--
-- #exclusivity#
-- __Exclusivity.__ Successive connections may have the same 'ConnectionId',
-- but mini-protocols that run at the same time with the same 'ConnectionId'
-- always run on the same connection. So a connection's mini-protocols can use
-- its 'ConnectionId' as the key for the state they share. That covers only
-- the mini-protocols' own threads: a thread one of them forks may outlive it,
-- and so use that key after a later connection has started using it.
--
-- Exclusivity holds because a connection's socket stays open until all its
-- mini-protocols have ended, and while it is open no other connection can
-- have the same pair of addresses:
--
-- * Each mini-protocol runs as a job of its connection's mux, and the mux's
--   @run@ (see "Network.Mux") returns only once it has cancelled every job and
--   waited for it to end, finalisers included.
-- * The socket is closed only after @run@ has returned. The connection manager
--   (see "Ouroboros.Network.ConnectionManager.Core") closes it once the
--   connection handler, which runs the mux, has returned, and @connectToNode@
--   (see "Ouroboros.Network.Socket") once the mux's thread has ended.
-- * While a socket is open, no other socket has the same pair of addresses.
--   TCP ensures that, as does the simulated snocket (see
--   "Simulation.Network.Snocket"). Over any other snocket, exclusivity holds
--   only if it ensures that too.
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
