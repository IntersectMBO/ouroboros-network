{-# LANGUAGE DeriveGeneric #-}

module Ouroboros.Network.PeerSelection.Types (
    RootPeers,
    RootPeerInfo(..)
  ) where

import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)


type RootPeers peeraddr = Map peeraddr RootPeerInfo
data RootPeerInfo = RootPeerInfo {
{-
       -- | Should we treat this root peer as persistent or ephemeral in the
       -- known peer set? When the root peer set changes to remove peers,
       -- persistent ones are kept in the known peer set, while ephemeral ones
       -- are removed.
       --
       -- An ephemeral policy is appropriate for peers specified by
       -- configuration, or DNS for singular addresses. A persistent policy
       -- is appropriate for DNS names that correspond to pools of addresses.
       --
       rootPeerEphemeral :: !Bool,
-}
       -- | Should this root peer be advertised to other peers asking for
       -- known peers? For certain peers specified by configuration it would
       -- be an appropriate policy to keep them private.
       --
       rootPeerAdvertise :: !Bool
     }
  deriving (Eq, Show, Generic)

