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
    -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
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

import Data.Bifunctor (first)
import Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..),
           requiresBootstrapPeers)
import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.LocalRootPeers (OutboundConnectionsState)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise (..))
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.PeerTrustable (PeerTrustable)
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
data PeerSelectionActionsArgs peeraddr peerconn exception m = PeerSelectionActionsArgs {
  psLocalRootPeersTracer      :: Tracer m (TraceLocalRootPeers peeraddr exception),
  psPublicRootPeersTracer     :: Tracer m TracePublicRootPeers,
  psReadTargets               :: STM m PeerSelectionTargets,
  peerTargets                 :: ConsensusModePeerTargets,
  -- ^ peer selection governor know, established and active targets
  getLedgerStateCtx          :: LedgerPeersConsensusInterface m,
  -- ^ Is consensus close to current slot?
  psReadLocalRootPeers        :: STM m [(HotValency, WarmValency, Map RelayAccessPoint (PeerAdvertise, PeerTrustable))],
  psReadPublicRootPeers       :: STM m (Map RelayAccessPoint PeerAdvertise),
  psReadUseBootstrapPeers     :: STM m UseBootstrapPeers,
  psPeerSharing               :: PeerSharing,
  -- ^ peer sharing configured value
  psPeerConnToPeerSharing     :: peerconn -> PeerSharing,
  -- ^ Extract peer sharing information from peerconn
  psReadPeerSharingController :: STM m (Map peeraddr (PeerSharingController peeraddr m)),
  -- ^ peer sharing registry
  psUpdateOutboundConnectionsState
                              :: OutboundConnectionsState -> STM m (),
  -- ^ Callback which updates information about outbound connections state.
  psReadInboundPeers          :: m (Map peeraddr PeerSharing),
  -- ^ inbound duplex peers
  readLedgerPeerSnapshot      :: STM m (Maybe LedgerPeerSnapshot)
  }

-- | Record of remaining parameters for withPeerSelectionActions
-- that were extracted out since the following vary based on the diffusion mode
--
newtype PeerSelectionActionsDiffusionMode peeraddr peerhandle m = PeerSelectionActionsDiffusionMode {
  psPeerStateActions :: PeerStateActions peeraddr peerhandle m
  -- ^ callbacks for peer state changes
  }

withPeerSelectionActions
  :: forall peeraddr peerconn resolver exception m a.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , MonadMVar  m
     , Ord peeraddr
     , Exception exception
     )
  => StrictTVar m (Config peeraddr)
  -> PeerActionsDNS peeraddr resolver exception m
  -> PeerSelectionActionsArgs peeraddr peerconn exception m
  -> WithLedgerPeersArgs m
  -> PeerSelectionActionsDiffusionMode peeraddr peerconn m
  -> (   (Async m Void, Async m Void)
      -> PeerSelectionActions peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  localRootsVar
  paDNS@PeerActionsDNS { paToPeerAddr = toPeerAddr, paDnsActions = dnsActions, paDnsSemaphore = dnsSemaphore }
  PeerSelectionActionsArgs {
    psLocalRootPeersTracer = localTracer,
    psPublicRootPeersTracer = publicTracer,
    peerTargets,
    psReadTargets = selectionTargets,
    getLedgerStateCtx,
    psReadLocalRootPeers = localRootPeers,
    psReadPublicRootPeers = publicRootPeers,
    psReadUseBootstrapPeers = useBootstrapped,
    psPeerSharing = sharing,
    psPeerConnToPeerSharing = peerConnToPeerSharing,
    psReadPeerSharingController = sharingController,
    psReadInboundPeers = readInboundPeers,
    psUpdateOutboundConnectionsState = updateOutboundConnectionsState,
    readLedgerPeerSnapshot }
  ledgerPeersArgs
  PeerSelectionActionsDiffusionMode { psPeerStateActions = peerStateActions }
  k = do
    withLedgerPeers
      paDNS
      ledgerPeersArgs
      (\getLedgerPeers lpThread -> do
          let peerSelectionActions = PeerSelectionActions {
                                       readPeerSelectionTargets = selectionTargets,
                                       readLocalRootPeers = readTVar localRootsVar,
                                       peerSharing = sharing,
                                       peerConnToPeerSharing = peerConnToPeerSharing,
                                       requestPublicRootPeers = \lpk n -> requestPublicRootPeers lpk n getLedgerPeers,
                                       requestPeerShare,
                                       peerStateActions,
                                       peerTargets,
                                       readUseBootstrapPeers = useBootstrapped,
                                       readInboundPeers,
                                       getLedgerStateCtx,
                                       updateOutboundConnectionsState,
                                       readLedgerPeerSnapshot }
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
  where
    -- We start by reading the current ledger state judgement, if it is
    -- YoungEnough we only care about fetching for ledger peers, otherwise we
    -- aim to fetch bootstrap peers.
    requestPublicRootPeers
      :: LedgerPeersKind
      -> Int
      -> (NumberOfPeers -> LedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
      -> m (PublicRootPeers peeraddr, DiffTime)
    requestPublicRootPeers ledgerPeersKind n getLedgerPeers = do
      -- Check if the node is in a sensitive state
      usingBootstrapPeers <- atomically
                           $ requiresBootstrapPeers <$> useBootstrapped
                                                    <*> lpGetLedgerStateJudgement getLedgerStateCtx
      if usingBootstrapPeers
         then do
          -- If the ledger state is in sensitive state we should get trustable peers.
          (bootstrapPeers, dt) <- requestConfiguredBootstrapPeers n
          pure (PublicRootPeers.fromBootstrapPeers bootstrapPeers, dt)
         else do
          -- If the ledger state is not in a sensitive state we should get ledger
          -- peers, the Nothing case should not happen but there can be a race
          -- condition. If that's the case we try again soon enough.
          mbLedgerPeers <- getLedgerPeers (NumberOfPeers $ fromIntegral n) ledgerPeersKind
          case mbLedgerPeers of
            -- no peers from the ledger
            Nothing -> do
              (publicRootPeers', dt) <- requestConfiguredPublicRootPeers n
              pure (PublicRootPeers.fromPublicRootPeers publicRootPeers', dt)
            Just (ledgerPeers, dt) ->
              case ledgerPeersKind of
                AllLedgerPeers ->
                  pure (PublicRootPeers.fromLedgerPeers ledgerPeers, dt)
                BigLedgerPeers ->
                  pure (PublicRootPeers.fromBigLedgerPeers ledgerPeers, dt)

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/intersectmbo/cardano-node/issues/731
    requestConfiguredPublicRootPeers :: Int -> m (Map peeraddr PeerAdvertise, DiffTime)
    requestConfiguredPublicRootPeers n =
      -- NOTE: we don't set `resolvConcurrent` because of
      -- https://github.com/kazu-yamamoto/dns/issues/174
      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              -- NOTE: we don't set `resolveConcurrent` because
                              -- of https://github.com/kazu-yamamoto/dns/issues/174
                              DNS.defaultResolvConf
                              publicRootPeers
                              dnsActions
                              ($ n)

    requestConfiguredBootstrapPeers :: Int -> m (Set peeraddr, DiffTime)
    requestConfiguredBootstrapPeers n = do
      let readBootstrapPeersMap =
            fmap (\case
                    DontUseBootstrapPeers     -> Map.empty
                    UseBootstrapPeers domains ->
                      Map.fromList ((,DoNotAdvertisePeer) <$> domains)
                 )
                 useBootstrapped

      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              DNS.defaultResolvConf
                              readBootstrapPeersMap
                              dnsActions
                              (fmap (first Map.keysSet) . ($ n))

    requestPeerShare :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr)
    requestPeerShare amount peer = do
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
