{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Ouroboros.Network.PeerSelection.Simple
  ( withPeerSelectionActions
  -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  ) where


import           Data.Foldable (toList)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)
import           Control.Exception (IOException)

import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Void (Void)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.LedgerPeers (NumberOfPeers (..))
import           Ouroboros.Network.PeerSelection.RootPeersDNS


withPeerSelectionActions
  :: Tracer IO (TraceLocalRootPeers IOException)
  -> Tracer IO TracePublicRootPeers
  -> TimeoutFn IO
  -> STM IO PeerSelectionTargets
  -> StrictTVar IO [(Int, Map RelayAddress PeerAdvertise)]
  -- ^ local root peers
  -> StrictTVar IO [RelayAddress]
  -- ^ public root peers
  -> PeerStateActions Socket.SockAddr peerconn IO
  -> (NumberOfPeers -> STM IO ())
  -> STM IO (Maybe (Set Socket.SockAddr, DiffTime))
  -> (Maybe (Async IO Void) -> PeerSelectionActions Socket.SockAddr peerconn IO -> IO a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> IO a
withPeerSelectionActions localRootTracer publicRootTracer timeout readTargets
  localRootPeersVar publicRootPeersVar peerStateActions reqLedgerPeers getLedgerPeers k = do
    localRootsVar <- newTVarIO mempty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = readTargets,
            readLocalRootPeers = toList <$> readTVar localRootsVar,
            requestPublicRootPeers = requestLedgerPeers dnsActions,
            requestPeerGossip = \_ -> pure [],
            peerStateActions
          }
        dnsActions = DNSActions {
            dnsResolverResource = resolverResource,
            dnsAsyncResolverResource = asyncResolverResource,
#if defined(mingw32_HOST_OS)
            dnsNewResolverResource = newResolverResource,
#endif
            dnsLookupAWithTTL = lookupAWithTTL
          }
    withAsync
      (localRootPeersProvider
        localRootTracer
        timeout
        DNS.defaultResolvConf
        localRootsVar
        localRootPeersVar
        dnsActions)
      (\thread -> k (Just thread) peerSelectionActions)
  where
    -- We first try to get poublic root peers from the ledger, but if it fails
    -- (for example because the node hasn't synced far enough) we fall back
    -- to using the manually configured bootstrap root peers.
    requestLedgerPeers :: DNSActions ResolvConf DNS.Resolver IOException IO
                       -> Int -> IO (Set Socket.SockAddr, DiffTime)
    requestLedgerPeers dnsActions n = do
        atomically $ reqLedgerPeers $ NumberOfPeers $ fromIntegral n
        peers_m <- atomically getLedgerPeers
        case peers_m of
             Nothing -> requestPublicRootPeers dnsActions n
             Just peers -> return peers

    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestPublicRootPeers :: DNSActions ResolvConf DNS.Resolver IOException IO
                           -> Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers dnsActions n =
      publicRootPeersProvider publicRootTracer
                              timeout
                              DNS.defaultResolvConf
                              publicRootPeersVar
                              ($ n)
                              dnsActions

