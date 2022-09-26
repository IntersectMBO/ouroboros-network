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
       PeerCold
     | PeerWarm
     | PeerHot
  deriving (Eq, Ord, Show)

