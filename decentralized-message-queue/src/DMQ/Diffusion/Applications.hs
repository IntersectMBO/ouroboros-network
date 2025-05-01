{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}

module DMQ.Diffusion.Applications where

import DMQ.Configuration
import DMQ.Diffusion.NodeKernel (NodeKernel (..))
import DMQ.NodeToClient (NodeToClientVersion, NodeToClientVersionData,
           stdVersionDataNTC)
import DMQ.NodeToNode (NodeToNodeVersion, NodeToNodeVersionData,
           stdVersionDataNTN)
import DMQ.NodeToNode qualified as NTN

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Monad.Class.MonadAsync (MonadAsync)
import Control.Monad.Class.MonadFork (MonadFork, MonadThread)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable)
import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.ExitPolicy (RepromoteDelay (..))
import Ouroboros.Network.Mux (OuroborosApplication (..))
import Ouroboros.Network.NodeToClient (combineVersions)
import Ouroboros.Network.NodeToNode ()
import Ouroboros.Network.PeerSelection.Governor.Types (PeerSelectionPolicy)
import Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import Ouroboros.Network.RethrowPolicy (ioErrorRethrowPolicy,
           muxErrorRethrowPolicy)

diffusionApplications
  :: ( Alternative (STM m)
     , MonadAsync m
     , MonadFork m
     , MonadMask m
     , MonadMVar m
     , MonadST m
     , MonadThread m
     , MonadThrow (STM m)
     , MonadTimer m
     , Ord ntnAddr
     , Hashable ntnAddr
     )
  => NodeKernel ntnAddr m
  -> Configuration ntnFd ntnAddr ntcFd ntcAddr
  -> Diffusion.Configuration extraFlags m ntnFd ntnAddr ntcFd ntcAddr
  -> NTN.LimitsAndTimeouts ntnAddr
  -> NTN.Apps ntnAddr BL.ByteString BL.ByteString m a ()
  -> PeerSelectionPolicy ntnAddr m
  -> Diffusion.Applications ntnAddr NodeToNodeVersion   NodeToNodeVersionData
                            ntcAddr NodeToClientVersion NodeToClientVersionData
                            m a
diffusionApplications
  NodeKernel {
    peerSharingRegistry
  }
  Configuration {
    dmqcNetworkMagic
  }
  Diffusion.Configuration {
    dcMode
  , dcOwnPeerSharing
  }
  ntnLimitsAndTimeouts
  ntnApps
  peerSelectionPolicy =
  Diffusion.Applications {
    daApplicationInitiatorMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN dmqcNetworkMagic dcMode dcOwnPeerSharing)
            (NTN.initiatorProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , daApplicationInitiatorResponderMode =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTN dmqcNetworkMagic dcMode dcOwnPeerSharing)
            (NTN.initiatorAndResponderProtocols ntnLimitsAndTimeouts ntnApps version)
        | version <- [minBound..maxBound]
        ]
  , daLocalResponderApplication         =
      combineVersions
        [ simpleSingletonVersions
            version
            (stdVersionDataNTC dmqcNetworkMagic)
            (\_versionData ->
                OuroborosApplication
                  [
                  ]
            )
        | version <- [minBound..maxBound]
        ]
  , daRethrowPolicy                     =  muxErrorRethrowPolicy
                                        <> ioErrorRethrowPolicy
  , daReturnPolicy                      = const (RepromoteDelay 0)
  , daLocalRethrowPolicy                = mempty
  , daPeerSelectionPolicy               = peerSelectionPolicy
  , daPeerSharingRegistry               = peerSharingRegistry
  }

