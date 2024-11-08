{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TupleSections            #-}

module Ouroboros.Network.PeerSelection.PeerSelectionActions
  ( withPeerSelectionActions
  , getPeerShare
  , getPublicRootPeers
  , PeerSelectionActionsArgs (..)
  , PeerSelectionActionsDiffusionMode (..)
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)

import Network.DNS qualified as DNS

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.RootPeersDNS.LocalRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
import Ouroboros.Network.PeerSelection.State.LocalRootPeers
import Ouroboros.Network.PeerSharing (PeerSharingController, requestPeers)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))

-- | Record of parameters for withPeerSelectionActions independent of diffusion mode
--
data PeerSelectionActionsArgs extraActions extraAPI extraPeers extraFlags peeraddr peerconn exception m = PeerSelectionActionsArgs {
  psLocalRootPeersTracer      :: Tracer m (TraceLocalRootPeers extraFlags peeraddr exception),
  psPublicRootPeersTracer     :: Tracer m TracePublicRootPeers,
  psReadTargets               :: STM m PeerSelectionTargets,
  -- ^ peer selection governor know, established and active targets
  getLedgerStateCtx           :: LedgerPeersConsensusInterface extraAPI m,
  -- ^ Is consensus close to current slot?
  psReadLocalRootPeers        :: STM m [(HotValency, WarmValency, Map RelayAccessPoint (PeerAdvertise, extraFlags))],
  psReadPublicRootPeers       :: STM m (Map RelayAccessPoint PeerAdvertise),
  psPeerSharing               :: PeerSharing,
  -- ^ peer sharing configured value
  psPeerConnToPeerSharing     :: peerconn -> PeerSharing,
  -- ^ Extract peer sharing information from peerconn
  psReadPeerSharingController :: STM m (Map peeraddr (PeerSharingController peeraddr m)),
  -- ^ Callback which updates information about outbound connections state.
  psReadInboundPeers          :: m (Map peeraddr PeerSharing),
  -- ^ inbound duplex peers
  readLedgerPeerSnapshot      :: STM m (Maybe LedgerPeerSnapshot),
  psRequestPublicRootPeers    :: (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
                              -> LedgerPeersKind -> Int -> m (PublicRootPeers extraPeers peeraddr, DiffTime),
  psExtraActions              :: extraActions
  }

-- | Record of remaining parameters for withPeerSelectionActions
-- that were extracted out since the following vary based on the diffusion mode
--
newtype PeerSelectionActionsDiffusionMode peeraddr peerhandle m = PeerSelectionActionsDiffusionMode {
  psPeerStateActions :: PeerStateActions peeraddr peerhandle m
  -- ^ callbacks for peer state changes
  }

withPeerSelectionActions
  :: forall extraActions extraPeers extraFlags extraAPI peeraddr peerconn resolver exception m a.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , MonadMVar  m
     , Ord peeraddr
     , Exception exception
     , Eq extraFlags
     )
  => StrictTVar m (Config extraFlags peeraddr)
  -> PeerActionsDNS peeraddr resolver exception m
  -> PeerSelectionActionsArgs extraActions extraAPI extraPeers extraFlags peeraddr peerconn exception m
  -> WithLedgerPeersArgs extraAPI m
  -> PeerSelectionActionsDiffusionMode peeraddr peerconn m
  -> (   (Async m Void, Async m Void)
      -> PeerSelectionActions extraActions extraPeers extraFlags extraAPI peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  localRootsVar
  paDNS@PeerActionsDNS { paToPeerAddr = toPeerAddr, paDnsActions = dnsActions }
  PeerSelectionActionsArgs {
    psLocalRootPeersTracer      = localTracer,
    psReadTargets               = selectionTargets,
    psReadLocalRootPeers        = localRootPeers,
    psPeerSharing               = sharing,
    psPeerConnToPeerSharing     = peerConnToPeerSharing,
    psReadPeerSharingController = sharingController,
    psReadInboundPeers          = readInboundPeers,
    psRequestPublicRootPeers,
    psExtraActions,
    readLedgerPeerSnapshot,
    getLedgerStateCtx
  }
  ledgerPeersArgs
  PeerSelectionActionsDiffusionMode { psPeerStateActions = peerStateActions }
  k = do
    withLedgerPeers
      paDNS
      ledgerPeersArgs
      (\getLedgerPeers lpThread -> do
          let peerSelectionActions =
                PeerSelectionActions {
                  readPeerSelectionTargets = selectionTargets
                , readLocalRootPeers       = readTVar localRootsVar
                , peerSharing              = sharing
                , peerConnToPeerSharing    = peerConnToPeerSharing
                , requestPublicRootPeers   = psRequestPublicRootPeers getLedgerPeers
                , extraActions             = psExtraActions
                , requestPeerShare         = getPeerShare sharingController
                , peerStateActions
                , readInboundPeers
                , getLedgerStateCtx
                , readLedgerPeerSnapshot
                }
          withAsync
            (localRootPeersProvider
              localTracer
              toPeerAddr
              -- NOTE: we don't set `resolvConcurrent` because
              -- of https://github.com/kazu-yamamoto/dns/issues/174
              DNS.defaultResolvConf
              dnsActions
              localRootPeers
              localRootsVar)
            (\lrppThread -> k (lpThread, lrppThread) peerSelectionActions))

getPeerShare :: ( MonadSTM m
                , MonadMVar m
                , Ord peeraddr
                )
             => STM m (Map peeraddr (PeerSharingController peeraddr m))
             -> PeerSharingAmount
             -> peeraddr
             -> m (PeerSharingResult peeraddr)
getPeerShare sharingController amount peer = do
  controllerMap <- atomically sharingController
  case Map.lookup peer controllerMap of
    -- Peer Registering happens asynchronously with respect to
    -- requestPeerShare. This means that there's a possible race where the
    -- Peer Selection Governor can decide to peer share request to a peer
    -- for the peer is registered. When this happens this map lookup is
    -- going to fail, so instead of erroring we report this to the governor
    -- so it can deal with this particular case accordingly.
    Nothing -> return PeerSharingNotRegisteredYet
    Just psController ->
      PeerSharingResult <$> requestPeers psController amount

-- We start by reading the current ledger state judgement, if it is
-- YoungEnough we only care about fetching for ledger peers, otherwise we
-- aim to fetch bootstrap peers.
getPublicRootPeers
  :: ( Monad m
     , Monoid extraPeers
     )
  => (NumberOfPeers -> m (extraPeers, DiffTime))
  -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> LedgerPeersKind
  -> Int
  -> m (PublicRootPeers extraPeers peeraddr, DiffTime)
getPublicRootPeers getExtraPeers getLedgerPeers  ledgerPeersKind n = do
  mbLedgerPeers <- getLedgerPeers (NumberOfPeers $ fromIntegral n) ledgerPeersKind
  case mbLedgerPeers of
    -- no peers from the ledger
    Nothing -> do
      (extraPeers, dt) <- getExtraPeers (NumberOfPeers $ fromIntegral n)
      pure (PublicRootPeers.empty extraPeers, dt)
    Just (ledgerPeers, dt) ->
      case ledgerPeersKind of
        AllLedgerPeers ->
          pure (PublicRootPeers.fromLedgerPeers ledgerPeers, dt)
        BigLedgerPeers ->
          pure (PublicRootPeers.fromBigLedgerPeers ledgerPeers, dt)
