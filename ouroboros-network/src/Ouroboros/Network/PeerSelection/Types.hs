module Ouroboros.Network.PeerSelection.Types
  ( PeerSource (..)
  , PeerStatus (..)
  , PublicExtraPeersActions (..)
  ) where
import Data.Set (Set)

-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourcePeerShare
  deriving (Eq, Ord, Show, Enum)

data PeerStatus =
       PeerCold
     -- ^ Peer is in true cold which means no connection to exists
     -- and the outbound governor is safe to promote it.
     | PeerCooling
     -- ^ Peer is in cold state but its connection still lingers.
     -- I.e. it is still in progress to be fully demoted.
     --
     -- Note:
     -- The `PeerCooling -> PeerCold` state transition is an `outbound-governor`
     -- reflection of the connection-manager's `TerminatingSt -> TerminatedSt`
     -- state transition (our version of tcp's `TimeWait`).  It is only
     -- triggered in case of a clean connection shutdown, not in the case of
     -- errors.
     --
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

-- PublicRootPeers extra peers bundle

data PublicExtraPeersActions extraPeers peeraddr =
  PublicExtraPeersActions {
    -- | Check if extraPeers is empty
    --
    nullExtraPeers         :: extraPeers -> Bool

    -- | Check extraPeers invariant
    --
  , invariantExtraPeers    :: extraPeers -> Bool

    -- | Check extraPeers membership
    --
  , memberExtraPeers       :: peeraddr -> extraPeers -> Bool

    -- | Convert extraPeers to peeraddr Set
    --
  , extraPeersToSet        :: extraPeers -> Set peeraddr

    -- | Compute extraPeers size
    --
  , sizeExtraPeers         :: extraPeers -> Int

    -- | Compute extraPeers difference
    --
  , differenceExtraPeers   :: (extraPeers -> Set peeraddr -> extraPeers)

    -- | Compute extraPeers intersection
    --
  , intersectionExtraPeers :: (extraPeers -> Set peeraddr -> extraPeers)
  }
