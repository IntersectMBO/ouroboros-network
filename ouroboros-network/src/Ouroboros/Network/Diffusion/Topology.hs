{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}

module Ouroboros.Network.Diffusion.Topology where

import Data.Map qualified as Map
import Data.Map.Strict (Map)

import Ouroboros.Network.Diffusion.Configuration (DiffusionMode)
import Ouroboros.Network.PeerSelection.LedgerPeers.Type (UseLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
import Ouroboros.Network.PeerSelection.RelayAccessPoint (RelayAccessPoint)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers hiding (extraFlags)
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LRP

data NetworkTopology extraConfig extraFlags =
  NetworkTopology {
    localRootPeersGroups :: !(LocalRootPeersGroups extraFlags)
  , publicRootPeers      :: ![PublicRootPeers]
  , useLedgerPeers       :: !UseLedgerPeers
  , peerSnapshotPath     :: !(Maybe FilePath)
  , extraConfig          :: !extraConfig
  }
  deriving (Eq, Show)

newtype LocalRootPeersGroups extraFlags = LocalRootPeersGroups
  { groups :: [LocalRootPeersGroup extraFlags]
  } deriving (Eq, Show)

-- | A local root peers group.  Local roots are treated by the outbound
-- governor in a special way.  The node will make sure that a node has the
-- requested number ('valency'/'hotValency') of connections to the local root peer group.
-- 'warmValency' value is the value of warm/established connections that the node
-- will attempt to maintain. By default this value will be equal to 'hotValency'.
--
data LocalRootPeersGroup extraFlags = LocalRootPeersGroup
  { localRoots        :: RootConfig
  , hotValency        :: HotValency
  , warmValency       :: WarmValency
  , rootDiffusionMode :: DiffusionMode
    -- ^ diffusion mode; used for local root peers.
  , extraFlags        :: extraFlags
  } deriving (Eq, Show)

newtype PublicRootPeers = PublicRootPeers
  { publicRoots :: RootConfig
  } deriving (Eq, Show)

-- | Each root peer consists of a list of access points and a shared
-- 'PeerAdvertise' field.
--
data RootConfig = RootConfig
  { rootAccessPoints :: [RelayAccessPoint]
    -- ^ a list of relay access points, each of which is either an ip address
    -- or domain name and a port number.
  , rootAdvertise    :: PeerAdvertise
    -- ^ 'advertise' configures whether the root should be advertised through
    -- peer sharing.
  } deriving (Eq, Show)


-- | Transforms a 'RootConfig' into a pair of 'RelayAccessPoint' and its
-- corresponding 'PeerAdvertise' value.
--
rootConfigToRelayAccessPoint
  :: RootConfig
  -> [(RelayAccessPoint, PeerAdvertise)]
rootConfigToRelayAccessPoint RootConfig { rootAccessPoints, rootAdvertise  } =
    [ (ap, rootAdvertise) | ap <- rootAccessPoints ]

producerAddresses
  :: NetworkTopology extraConfig extraFlags
  -> ( [(HotValency, WarmValency, Map RelayAccessPoint (LocalRootConfig extraFlags))]
     , Map RelayAccessPoint PeerAdvertise
     )
  -- ^ local roots & public roots
producerAddresses NetworkTopology { localRootPeersGroups
                                  , publicRootPeers
                                  } =
  ( map (\lrp -> ( hotValency lrp
                 , warmValency lrp
                 , Map.fromList
                 . map (\(addr, peerAdvertise) ->
                         ( addr
                         , LocalRootConfig {
                             diffusionMode = rootDiffusionMode lrp,
                             peerAdvertise,
                             LRP.extraFlags = extraFlags lrp
                           }
                         )
                       )
                 . rootConfigToRelayAccessPoint
                 $ localRoots lrp
                 )
        )
        (groups localRootPeersGroups)
  , foldMap ( Map.fromList
            . rootConfigToRelayAccessPoint
            . publicRoots
            ) publicRootPeers
  )
