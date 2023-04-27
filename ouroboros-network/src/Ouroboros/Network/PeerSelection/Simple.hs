{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.PeerSelection.Simple
  ( withPeerSelectionActions
    -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  ) where


import           Control.Applicative (Alternative)
import           Control.Concurrent.Class.MonadMVar (MonadMVar (..), takeMVar)
import           Control.Concurrent.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Class.MonadTimer.SI
import           Control.Tracer (Tracer)
import           Data.Foldable (toList)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void (Void)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.PeerAdvertise
                     (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSelection.RootPeersDNS
import           Ouroboros.Network.PeerSharing (PeerSharingController (..))
import           Ouroboros.Network.Protocol.PeerSharing.Type
                     (PeerSharingAmount (..))


withPeerSelectionActions
  :: forall peeraddr peerconn resolver exception m a.
     ( Alternative (STM m)
     , MonadAsync m
     , MonadDelay m
     , MonadThrow m
     , MonadMVar  m
     , Ord peeraddr
     , Exception exception
     , Eq (Async m Void)
     )
  => Tracer m (TraceLocalRootPeers peeraddr exception)
  -> Tracer m TracePublicRootPeers
  -> (IP -> Socket.PortNumber -> peeraddr)
  -> DNSActions resolver exception m
  -> STM m PeerSelectionTargets
  -> STM m [(Int, Map RelayAccessPoint PeerAdvertise)]
  -- ^ local root peers
  -> STM m (Map RelayAccessPoint PeerAdvertise)
  -- ^ public root peers
  -> PeerSharing
  -- ^ peer sharing configured value
  -> (peerconn -> PeerSharing)
  -- ^ Extract peer sharing information from peerconn
  -> STM m (Map peeraddr (PeerSharingController peeraddr m))
  -- ^ peer sharing registry
  -> PeerStateActions peeraddr peerconn m
  -> (NumberOfPeers -> m (Maybe (Set peeraddr, DiffTime)))
  -> (   Async m Void
      -> PeerSelectionActions peeraddr peerconn m
      -> m a)
  -- ^ continuation, receives a handle to the local roots peer provider thread
  -- (only if local root peers were non-empty).
  -> m a
withPeerSelectionActions
  localRootTracer
  publicRootTracer
  toPeerAddr
  dnsActions
  readTargets
  readLocalRootPeers
  readPublicRootPeers
  peerSharing
  peerConnToPeerSharing
  readPeerSharingController
  peerStateActions
  getLedgerPeers
  k = do
    localRootsVar <- newTVarIO mempty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = toList <$> readTVar localRootsVar,
            peerSharing,
            peerConnToPeerSharing,
            requestPublicRootPeers = requestPublicRootPeers,
            requestPeerShare = requestPeerShare,
            peerStateActions
          }
    withAsync
      (localRootPeersProvider
        localRootTracer
        toPeerAddr
        DNS.defaultResolvConf
        dnsActions
        readLocalRootPeers
        localRootsVar)
      (\thread -> k thread peerSelectionActions)
  where
    -- We first try to get public root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestPublicRootPeers :: Int -> m (Map peeraddr (PeerAdvertise, IsLedgerPeer), DiffTime)
    requestPublicRootPeers n = do
      peers_m <- getLedgerPeers (NumberOfPeers $ fromIntegral n)
      case peers_m of
           -- No peers from Ledger
           Nothing    -> do
             (m, dt) <- requestConfiguredRootPeers n
             let m' = Map.map (\a -> (a, IsNotLedgerPeer)) m
             return (m', dt)

           -- These peers come from Ledger
           --
           -- We set peers coming from ledger as DoNotAdvertisePeer so they do
           -- not get shared via Peer Sharing
           Just (peers, dt) ->
             return ( Map.fromList
                      $ map (\a -> (a, (DoNotAdvertisePeer, IsLedgerPeer)))
                      $ Set.toList peers
                    , dt)

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestConfiguredRootPeers :: Int -> m (Map peeraddr PeerAdvertise, DiffTime)
    requestConfiguredRootPeers n =
      publicRootPeersProvider publicRootTracer
                              toPeerAddr
                              DNS.defaultResolvConf
                              readPublicRootPeers
                              dnsActions
                              ($ n)

    requestPeerShare :: PeerSharingAmount -> peeraddr -> m (PeerSharingResult peeraddr)
    requestPeerShare amount peer = do
      resultQueue <- newEmptyMVar
      controller <- atomically readPeerSharingController
      case Map.lookup peer controller of
        -- Peer Registering happens asynchronously with respect to
        -- requestPeerShare. This means that there's a possible race where the
        -- Peer Selection Governor can decide to peer share request to a peer
        -- for the peer is registered. When this happens this map lookup is
        -- going to fail, so instead of erroring we report this to the governor
        -- so it can deal with this particular case accordingly.
        Nothing -> return PeerSharingNotRegisteredYet
        Just (PeerSharingController requestQueue) -> do
          atomically $ putTMVar requestQueue (amount, resultQueue)
          result <- takeMVar resultQueue
          return (PeerSharingResult result)
