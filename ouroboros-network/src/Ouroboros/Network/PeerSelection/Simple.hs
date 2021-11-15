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


import           Data.Foldable (toList)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Void (Void)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket

import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS


withPeerSelectionActions
  :: forall peeraddr peerconn resolver exception m a.
     ( MonadAsync m
     , MonadDelay m
     , MonadThrow m
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
  -> STM m [RelayAccessPoint]
  -- ^ public root peers
  -> PeerStateActions peeraddr peerconn m
  -> (NumberOfPeers -> m (Maybe (Set peeraddr, DiffTime)))
  -> (Maybe (Async m Void)
      -> PeerSelectionActions peeraddr peerconn m
      -> m a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> m a
withPeerSelectionActions
  localRootTracer
  publicRootTracer
  toPeerAddr
  dnsActions
  readTargets
  readLocalRootPeers
  readPublicRootPeers
  peerStateActions
  getLedgerPeers
  k = do
    localRootsVar <- newTVarIO mempty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = toList <$> readTVar localRootsVar,
            requestPublicRootPeers = requestPublicRootPeers,
            requestPeerGossip = \_ -> pure [],
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
      (\thread -> k (Just thread) peerSelectionActions)
  where
    -- We first try to get poublic root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestPublicRootPeers :: Int -> m (Set peeraddr, DiffTime)
    requestPublicRootPeers n = do
      peers_m <- getLedgerPeers (NumberOfPeers $ fromIntegral n)
      case peers_m of
           Nothing    -> requestConfiguredRootPeers n
           Just peers -> return peers

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestConfiguredRootPeers :: Int -> m (Set peeraddr, DiffTime)
    requestConfiguredRootPeers n =
      publicRootPeersProvider publicRootTracer
                              toPeerAddr
                              DNS.defaultResolvConf
                              readPublicRootPeers
                              dnsActions
                              ($ n)
