{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}

module DMQ.Diffusion.Applications where

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel (NodeKernel (..))
import DMQ.NodeToClient (NodeToClientVersion, NodeToClientVersionData,
           stdVersionDataNTC)
import DMQ.NodeToClient qualified as NTC
import DMQ.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           stdVersionDataNTN)
import DMQ.NodeToNode qualified as NTN

import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionPolicy)
import Ouroboros.Network.Protocol.Handshake.Version (combineVersions,
           simpleSingletonVersions)
import Ouroboros.Network.RethrowPolicy (ioErrorRethrowPolicy,
           muxErrorRethrowPolicy)

diffusionApplications
  :: NodeKernel ntnAddr m
  -> Configuration
  -> Diffusion.Configuration NoExtraFlags m ntnFd ntnAddr ntcFd ntcAddr
  -> NTN.LimitsAndTimeouts ntnAddr
  -> NTN.Apps ntnAddr m a ()
  -> NTC.Apps ntcAddr m ()
  -> PeerSelectionPolicy ntnAddr m
  -> Diffusion.Applications ntnAddr NodeToNodeVersion   NodeToNodeVersionData
                            ntcAddr NodeToClientVersion NodeToClientVersionData
                            m a
diffusionApplications
  NodeKernel {
    peerSharingRegistry
  }
  Configuration {
    dmqcNetworkMagic = I networkMagic
  }
  Diffusion.Configuration {
    dcMode
  , dcPeerSharing
  }
  ntnLimitsAndTimeouts
  ntnApps
  ntcApps
  peerSelectionPolicy =
  Diffusion.Applications {
    daApplicationInitiatorMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN networkMagic dcMode dcPeerSharing)
            (NTN.initiatorProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , daApplicationInitiatorResponderMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN networkMagic dcMode dcPeerSharing)
            (NTN.initiatorAndResponderProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , daLocalResponderApplication         =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTC networkMagic)
            (NTC.responders ntcApps version)
        | version <- [minBound..maxBound]
        ]
  , daRethrowPolicy                     =  muxErrorRethrowPolicy
                                        <> ioErrorRethrowPolicy
  , daReturnPolicy                      = const dmqRepromoteDelay
  , daRepromoteErrorDelay               = dmqRepromoteDelay
  , daLocalRethrowPolicy                = mempty
  , daPeerSelectionPolicy               = peerSelectionPolicy
  , daPeerSharingRegistry               = peerSharingRegistry
  }


-- | PeerSelection RepromoteDelay used after
dmqRepromoteDelay :: RepromoteDelay
dmqRepromoteDelay = 10
