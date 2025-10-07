{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}

module DMQ.Diffusion.Arguments
  ( diffusionArguments
  , NoExtraPeers (..)
  , NoExtraState (..)
  , NoExtraDebugState (..)
  , NoExtraCounters (..)
  , NoExtraFlags (..)
  , NoExtraConfig (..)
  , NoExtraAPI (..)
  , NoExtraChurnArgs (..)
  ) where

import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadSTM (MonadSTM (..))
import Control.Exception (IOException)
import Control.Monad.Class.MonadST (MonadST)
import Control.Monad.Class.MonadThrow (MonadCatch)
import Control.Monad.Class.MonadTimer.SI (MonadDelay, MonadTimer)
import "contra-tracer" Control.Tracer (Tracer)
import Network.DNS (Resolver)
import Network.Socket (Socket)

import DMQ.NodeToClient as NtC
import DMQ.NodeToNode as NtN
import DMQ.NodeToNode qualified as DMQ
import DMQ.Tracer

import Ouroboros.Network.Diffusion.Types qualified as Diffusion
import Ouroboros.Network.PeerSelection.Churn (peerChurnGovernor)
import Ouroboros.Network.PeerSelection.Governor.Types
           (ExtraGuardedDecisions (..), PeerSelectionGovernorArgs (..))
import Ouroboros.Network.PeerSelection.LedgerPeers.Type
           (LedgerPeersConsensusInterface (..))
import Ouroboros.Network.PeerSelection.RelayAccessPoint (SRVPrefix)
import Ouroboros.Network.PeerSelection.Types (nullPublicExtraPeersAPI)


diffusionArguments
  :: ( Alternative (STM m)
     , MonadCatch m
     , MonadDelay m
     , MonadST m
     , MonadTimer m
     )
  => Tracer m (NtN.HandshakeTr ntnAddr)
  -> Tracer m (NtC.HandshakeTr ntcAddr)
  -> Diffusion.Arguments
       NoExtraState NoExtraDebugState NoExtraFlags NoExtraPeers NoExtraAPI NoExtraChurnArgs NoExtraCounters
       IOException
       Resolver
       m
       Socket
       ntnAddr
       NodeToNodeVersion
       NodeToNodeVersionData
       ntcAddr
       NodeToClientVersion
       NodeToClientVersionData
diffusionArguments handshakeNtNTracer
                   handshakeNtCTracer =
  Diffusion.Arguments {
    Diffusion.daNtnDataFlow    = DMQ.ntnDataFlow
  , Diffusion.daNtnPeerSharing = peerSharing
  , Diffusion.daUpdateVersionData =
      \versionData diffusionMode -> versionData { diffusionMode }
  , Diffusion.daNtnHandshakeArguments = ntnHandshakeArguments handshakeNtNTracer
  , Diffusion.daNtcHandshakeArguments = ntcHandshakeArguments handshakeNtCTracer
  , Diffusion.daLedgerPeersCtx        =
      LedgerPeersConsensusInterface {
        lpGetLatestSlot  = return minBound
      , lpGetLedgerPeers = return []
      , lpExtraAPI       = NoExtraAPI
      }
  , Diffusion.daEmptyExtraState           = NoExtraState
  , Diffusion.daEmptyExtraCounters        = NoExtraCounters
  , Diffusion.daExtraPeersAPI             = nullPublicExtraPeersAPI
  , Diffusion.daInstallSigUSR1Handler     = \_ _ -> pure ()
  , Diffusion.daPeerSelectionGovernorArgs =
      PeerSelectionGovernorArgs {
        abortGovernor   = \_ _ -> Nothing
      , updateWithState = \_ _ _ _ -> pure ()
      , extraDecisions  =
          ExtraGuardedDecisions {
            preBlocking     = mempty
          , postBlocking    = mempty
          , postNonBlocking = mempty
          , customTargetsAction         = Nothing
          , customLocalRootsAction      = Nothing
          , enableProgressMakingActions = const True
          , ledgerPeerSnapshotExtraStateChange = id
          }
      }
  , Diffusion.daPeerSelectionStateToExtraCounters = const NoExtraCounters
  , Diffusion.daToExtraPeers                      = const NoExtraPeers
  , Diffusion.daRequestPublicRootPeers            = Nothing
  , Diffusion.daPeerChurnGovernor                 = peerChurnGovernor
  , Diffusion.daExtraChurnArgs                    = NoExtraChurnArgs
  , Diffusion.daSRVPrefix                         = dmqSRVPrefix
  }


-- | SRVPrefix as registered in `CIP#0155`.
--
dmqSRVPrefix :: SRVPrefix
dmqSRVPrefix = "_dmq._mithril._cardano._tcp"
