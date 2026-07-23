{-# LANGUAGE BlockArguments           #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Ouroboros.Network.PeerSelection.PeerSelectionActions
  ( PeerSelectionActions (..)
  , WithPeerSelectionActionsArgs (..)
  , withPeerSelectionActions
  , requestPeerSharingResult
  , requestPublicRootPeersImpl
  ) where


import Control.Applicative (Alternative)
import Control.Concurrent.Class.MonadMVar (MonadMVar (..))
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer)

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (Void)

import Network.DNS qualified as DNS

import Ouroboros.Network.PeerSelection.Governor.Types
import Ouroboros.Network.PeerSelection.LedgerPeers hiding (getLedgerPeers)
import Ouroboros.Network.PeerSelection.PeerAdvertise (PeerAdvertise)
-- import Ouroboros.Network.PeerSelection.PeerStateActions (PeerConnectionHandle)
import Ouroboros.Network.PeerSelection.PeerSharing
import Ouroboros.Network.PeerSelection.PublicRootPeers (PublicRootPeers)
import Ouroboros.Network.PeerSelection.PublicRootPeers qualified as PublicRootPeers
import Ouroboros.Network.PeerSelection.RootPeersDNS
import Ouroboros.Network.PeerSelection.State.LocalRootPeers qualified as LocalRootPeers
import Ouroboros.Network.PeerSelection.Types
import Ouroboros.Network.PeerSharing (PeerSharingController, requestPeers)
import Ouroboros.Network.Protocol.PeerSharing.Type (PeerSharingAmount (..))

import System.Random

data WithPeerSelectionActionsArgs extraState extraFlags extraPeers extraAPI peeraddr peerconn resolver m a =
    WithPeerSelectionActionsArgs {
      localRootPeersTracer       :: Tracer m (TraceLocalRootPeers extraFlags peeraddr),
      -- ^ trace local root peers
      peerSelectionTargets       :: PeerSelectionTargets,
      -- ^ initial peer selection targets
      readPeerSelectionTargets   :: STM m PeerSelectionTargets,
      -- ^ read peer selection targets
      getLedgerStateCtx          :: LedgerPeersConsensusInterface extraAPI m,
      -- ^ ledger peer consensus API

      --
      -- local root peerr
      --

      localRootPeersRng          :: StdGen,
      readLocalRootPeersFromFile :: STM m (LocalRootPeers.Config extraFlags RelayAccessPoint),
      localRootsVar              :: StrictTVar m (LocalRootPeers.Config extraFlags peeraddr),

      --
      -- peer sharing
      --

      peerSharing                :: PeerSharing,
      -- ^ configuration value of peer sharing
      peerConnToPeerSharing      :: peerconn -> PeerSharing,
      -- ^ handshake negotiated value of peer sharing
      requestPeerShare           :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr),
      -- ^ request peers through peer sharing callback

      requestPublicRootPeers     :: SomeLedgerPeersKind
                                      -> StdGen
                                      -> Int
                                      -> m (PublicRootPeers extraPeers peeraddr, DiffTime),
      -- ^ request public root peers callback
      readInboundPeers           :: m (Map peeraddr PeerSharing),
      -- ^ inbound peers which are injected into results of peer sharing
      -- depending on the `PeerSharing` value
      readLedgerPeerSnapshot     :: STM m (Maybe (LedgerPeerSnapshot BigLedgerPeers)),
      -- ^ read ledger peer snapshot, it is read form a TVar which can be updated on SIGHUP from a file.
      extraPeersAPI              :: PublicExtraPeersAPI extraPeers peeraddr,
      peerStateActions           :: PeerStateActions peeraddr extraFlags peerconn m,
      peerActionsDNS             :: PeerActionsDNS peeraddr resolver m
    }

withPeerSelectionActions
  :: forall extraState extraFlags extraPeers extraAPI peeraddr peerconn resolver m a.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , Ord peeraddr
     , Eq extraFlags
     )
  => WithPeerSelectionActionsArgs extraState extraFlags extraPeers extraAPI peeraddr peerconn resolver m a
  -> (   PeerSelectionActions extraState extraFlags extraPeers extraAPI peeraddr peerconn m
      -> Async m Void
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  WithPeerSelectionActionsArgs {
    localRootPeersTracer,
    peerSelectionTargets,
    readPeerSelectionTargets,
    getLedgerStateCtx,
    localRootPeersRng,
    readLocalRootPeersFromFile,
    localRootsVar,
    peerSharing,
    peerConnToPeerSharing,
    requestPeerShare,
    requestPublicRootPeers,
    readInboundPeers,
    readLedgerPeerSnapshot,
    extraPeersAPI,
    peerStateActions,
    peerActionsDNS
  }
  k = do
  let peerSelectionActions =
        PeerSelectionActions {
          peerSelectionTargets,
          readPeerSelectionTargets,
          getLedgerStateCtx,
          readLocalRootPeersFromFile,
          readLocalRootPeers = readTVar localRootsVar,
          peerSharing,
          peerConnToPeerSharing,
          requestPeerShare,
          requestPublicRootPeers,
          readInboundPeers =
            case peerSharing of
              PeerSharingDisabled -> pure Map.empty
              PeerSharingEnabled  -> readInboundPeers,
          readLedgerPeerSnapshot,
          extraPeersAPI,
          peerStateActions
        }
  withAsync
    (do
      labelThisThread "local-roots-peers"
      localRootPeersProvider
        localRootPeersTracer
        peerActionsDNS
        -- NOTE: we don't set `resolvConcurrent` because
        -- of https://github.com/kazu-yamamoto/dns/issues/174
        DNS.defaultResolvConf
        localRootPeersRng
        readLocalRootPeersFromFile
        localRootsVar)
    (k peerSelectionActions)

requestPeerSharingResult :: ( MonadSTM m
                            , MonadMVar m
                            , Ord peeraddr
                            )
                         => STM m (Map peeraddr (PeerSharingController peeraddr m))
                         -> PeerSharingAmount
                         -> peeraddr
                         -> m (PeerSharingResult peeraddr)
requestPeerSharingResult sharingController amount peer = do
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


-- | Retrieves public root peers
--
-- This function attempts to fetch ledger peers of a specified kind. If no ledger peers
-- are found, it retrieves extra peers instead. The result includes the
-- public root peers and the time taken for the operation.
--
requestPublicRootPeersImpl
  :: forall m peeraddr extraPeers resolver.
    ( MonadThrow m
    , MonadAsync m
    , Monoid extraPeers
    , Ord peeraddr
    )
  => Tracer m TracePublicRootPeers
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -> PeerActionsDNS peeraddr resolver m
  -> DNSSemaphore m
  -> (Map peeraddr PeerAdvertise -> extraPeers)
  -- ^ Function to convert DNS result into extra peers
  -> (NumberOfPeers -> SomeLedgerPeersKind -> m (Maybe (Set peeraddr, DiffTime)))
  -> SomeLedgerPeersKind
  -> StdGen
  -> Int
  -> m (PublicRootPeers extraPeers peeraddr, DiffTime)
requestPublicRootPeersImpl
  publicTracer readPublicRootPeers
  PeerActionsDNS { paToPeerAddr = toPeerAddr
                 , paDnsActions = dnsActions
                 }
  dnsSemaphore toExtraPeers getLedgerPeers ledgerPeersKind rng n = do
  mbLedgerPeers <- getLedgerPeers (NumberOfPeers $ fromIntegral n)
                                   ledgerPeersKind
  case mbLedgerPeers of
    -- no peers from the ledger
    Nothing -> do
      (extraPeers, dt) <- first toExtraPeers
                      <$> getExtraPeers n
      pure (PublicRootPeers.empty extraPeers, ttlDiffTime dt)
    Just (ledgerPeers, dt) ->
      case ledgerPeersKind of
        SomeLedgerPeersKind SingAllLedgerPeers ->
          pure (PublicRootPeers.fromLedgerPeers ledgerPeers, dt)
        SomeLedgerPeersKind SingBigLedgerPeers ->
          pure (PublicRootPeers.fromBigLedgerPeers ledgerPeers, dt)
  where
    getExtraPeers :: Int -> m (Map peeraddr PeerAdvertise, TTL)
    getExtraPeers x =
      -- NOTE: we don't set `resolvConcurrent` because of
      -- https://github.com/kazu-yamamoto/dns/issues/174
      publicRootPeersProvider publicTracer
                              toPeerAddr
                              dnsSemaphore
                              -- NOTE: we don't set `resolveConcurrent` because
                              -- of https://github.com/kazu-yamamoto/dns/issues/174
                              DNS.defaultResolvConf
                              readPublicRootPeers
                              dnsActions
                              rng
                              ($ x)
