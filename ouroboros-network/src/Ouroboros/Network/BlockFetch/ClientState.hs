
module Ouroboros.Network.BlockFetch.ClientState (
    FetchClientStateVars(..),
    PeerFetchStatus(..),
    PeerFetchInFlight(..),
    initialPeerFetchInFlight,
    FetchRequest(..),
    TFetchRequestVar,
    newTFetchRequestVar,
    takeTFetchRequestVar,
    writeTFetchRequestVar,
  ) where

import qualified Data.Set as Set
import           Data.Set (Set)
import           Control.Monad.Class.MonadSTM

import           Ouroboros.Network.Chain (Point)
import           Ouroboros.Network.DeltaQ (SizeInBytes)


-- | A set of variables shared between the block fetch logic thread and each
-- thread executing the client side of the block fetch protocol. That is, these
-- are the shared variables per peer. The 'FetchClientRegistry' contains the
-- mapping of these for all peers.
--
-- The variables are used for communicating from the protocol thread to the
-- decision making thread the status of things with that peer. And in the other
-- direction one shared variable is for providing new fetch requests.
--
data FetchClientStateVars header m =
     FetchClientStateVars {

       -- | The current status of communication with the peer. It is written
       -- by the protocol thread and monitored and read by the decision logic
       -- thread. Changes in this state trigger re-evaluation of fetch
       -- decisions.
       --
       fetchClientStatusVar   :: TVar m (PeerFetchStatus header),

       -- | The current number of requests in-flight and the amount of data
       -- in-flight with the peer. It is written by the protocol thread and
       -- read by the decision logic thread. This is used in fetch decisions
       -- but changes here do not trigger re-evaluation of fetch decisions.
       --
       fetchClientInFlightVar :: TVar m (PeerFetchInFlight header),

       -- | The shared variable used to communicate fetch requests to the thread
       -- running the block fetch protocol. Fetch requests are posted by the
       -- decision logic thread. The protocol thread accepts the requests and
       -- acts on them, updating the in-flight stats. While this is a 'TMVar',
       -- it is not used as a one-place queue: the requests can be updated
       -- before being accepted.
       --
       fetchClientRequestVar  :: TFetchRequestVar m header
     }


-- | The status of the block fetch communication with a peer. This is maintained
-- by fetch protocol threads and used in the block fetch decision making logic.
-- Changes in this status trigger re-evaluation of fetch decisions.
--
data PeerFetchStatus header =
       -- | Communication with the peer has failed. This is a temporary status
       -- that may occur during the process of shutting down the thread that
       -- runs the block fetch protocol. The peer will promptly be removed from
       -- the peer registry and so will not be considered at all.
       --
       PeerFetchStatusShutdown

       -- | The peer is in a potentially-temporary state in which it has not
       -- responded to us within a certain expected time limit. This is not
       -- a hard protocol timeout where the whole connection will be abandoned,
       -- it is simply a reply that has taken longer than expected. This status
       -- is used to trigger re-evaluating which peer to ask for blocks from,
       -- so that we can swiftly ask other peers for blocks if one unexpectedly
       -- responds too slowly
       --
       -- Peers in this state may later return to normal states if communication
       -- resumes, or they may eventually hit a hard timeout and fail.
       --
     | PeerFetchStatusAberrant

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered too busy to accept new requests. Changing from this state
       -- to the ready state is used to trigger re-evaluating fetch decisions
       -- and may eventually result in new fetch requests. This state is used
       -- as part of a policy to batch new requests: instead of switching to
       -- the ready state the moment there is tiny bit of capacity available,
       -- the state is changed once the capacity reaches a certain threshold.
       --
     | PeerFetchStatusBusy

       -- | Communication with the peer is in a normal state, and the peer is
       -- considered ready to accept new requests.
       --
     | PeerFetchStatusReady (Set (Point header))
  deriving (Eq, Show)


-- | The number of requests in-flight and the amount of data in-flight with a
-- peer. This is maintained by fetch protocol threads and used in the block
-- fetch decision making logic.
--
data PeerFetchInFlight header = PeerFetchInFlight {
       -- | The number of block fetch requests that are currently in-flight.
       -- This is the number of /requests/ not the number of blocks. Each
       -- request is for a range of blocks.
       --
       -- We track this because there is a fixed maximum number of outstanding
       -- requests that the protocol allows.
       --
       peerFetchReqsInFlight :: !Word,

       -- | The sum of the byte count of blocks expected from all in-flight
       -- fetch requests. This is a close approximation of the amount of data
       -- we expect to receive, assuming no failures.
       --
       -- We track this because we pipeline fetch requests and we want to keep
       -- some but not too much data in flight at once.
       --
       peerFetchBytesInFlight :: !SizeInBytes,

       -- | The points for the set of blocks that are currently in-flight.
       -- Note that since requests are for ranges of blocks this does not
       -- correspond to the number of requests in flight.
       --
       -- We track this because as part of the decision for which blocks to
       -- fetch from which peers we take into account what blocks are already
       -- in-flight with peers.
       --
       peerFetchBlocksInFlight :: Set (Point header)
     }
  deriving (Eq, Show)

initialPeerFetchInFlight :: PeerFetchInFlight header
initialPeerFetchInFlight =
    PeerFetchInFlight {
      peerFetchReqsInFlight   = 0,
      peerFetchBytesInFlight  = 0,
      peerFetchBlocksInFlight = Set.empty
    }



newtype FetchRequest header = FetchRequest [[header]]
  deriving Show

instance Functor FetchRequest where
  fmap f (FetchRequest hdrss) = FetchRequest (map (map f) hdrss)

newtype TFetchRequestVar m header =
        TFetchRequestVar (TMVar m (FetchRequest header))

newTFetchRequestVar :: MonadSTM m => STM m (TFetchRequestVar m header)
newTFetchRequestVar = TFetchRequestVar <$> newEmptyTMVar

-- This may seem a bit odd, but we unconditionally overwrite the TMVar here.
-- The reason is that we are not using this TMVar as a one-place queue of
-- requests. Rather we use this as a box containing the current request.
-- The request is not considered accepted when we put it in the box, but
-- when the fetch protocol client takes it out of the box. Up until the
-- point that the request is accepted we can overwrite it with a updated
-- request. So the logic here is that we either fill the box or or
-- overwrite the contents. We achieve that by atomically trying to empty it
-- (ignoring any content), followed by filling it.
--
writeTFetchRequestVar :: MonadSTM m
                      => TFetchRequestVar m header
                      -> FetchRequest header
                      -> STM m ()
writeTFetchRequestVar (TFetchRequestVar v) r = tryTakeTMVar v >> putTMVar v r

takeTFetchRequestVar :: MonadSTM m
                     => TFetchRequestVar m header
                     -> STM m (FetchRequest header)
takeTFetchRequestVar (TFetchRequestVar v) = takeTMVar v

