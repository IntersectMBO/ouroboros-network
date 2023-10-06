module Ouroboros.Network.PeerSelection.Types
  ( PeerSource (..)
  , PeerStatus (..)
  ) where

-- | Where did this peer come from? Policy functions can choose to treat
-- peers differently depending on where we found them from.
--
data PeerSource = PeerSourceLocalRoot
                | PeerSourcePublicRoot
                | PeerSourcePeerShare
  deriving (Eq, Ord, Show, Enum)

data PeerStatus =
       PeerReallyCold
     -- ^ Peer is in true cold which means no connection to exists
     -- and the outbound governor is safe to promote it.
     | PeerCold
     -- ^ Peer is in cold state but its connection still lingers.
     -- I.e. it is still in progress to be fully demoted.
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

