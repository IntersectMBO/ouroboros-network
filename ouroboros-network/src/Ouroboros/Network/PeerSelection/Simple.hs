{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}

module Ouroboros.Network.PeerSelection.Simple
  ( withPeerSelectionActions
  -- * Re-exports
  , PeerSelectionTargets (..)
  , PeerAdvertise (..)
  ) where


import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer)

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)

import qualified Network.DNS as DNS
import qualified Network.Socket as Socket
import           Network.Mux.Timeout

import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise (..))
import           Ouroboros.Network.PeerSelection.Governor.Types
import           Ouroboros.Network.PeerSelection.RootPeersDNS


withPeerSelectionActions
  :: Tracer IO TraceLocalRootPeers
  -> Tracer IO TracePublicRootPeers
  -> TimeoutFn IO
  -> PeerSelectionTargets
  -> [(Int, Map RelayAddress PeerAdvertise)]
  -- ^ static local root peers
  -> [(DomainAddress, PeerAdvertise)]
  -- ^ local root peers
  -> [DomainAddress]
  -- ^ public root peers
  -> PeerStateActions Socket.SockAddr peerconn IO
  -> (Maybe (Async IO ()) -> PeerSelectionActions Socket.SockAddr peerconn IO -> IO a)
  -- ^ continuation, recieves a handle to the local roots peer provider thread
  -- (only if local root peers where non-empty).
  -> IO a
withPeerSelectionActions localRootTracer publicRootTracer timeout targets _staticLocalRootPeers localRootPeers publicRootPeers peerStateActions k = do
    localRootsVar <- newTVarIO Map.empty
    let peerSelectionActions = PeerSelectionActions {
            readPeerSelectionTargets = pure targets,
            readLocalRootPeers = pure [],
            requestPublicRootPeers,
            requestPeerGossip = \_ -> pure [],
            peerStateActions
          }
    case localRootPeers of
      [] -> k Nothing peerSelectionActions
      as ->
        withAsync
          (localRootPeersProvider
            localRootTracer
            timeout
            DNS.defaultResolvConf
            localRootsVar
            as)
          (\thread -> k (Just thread) peerSelectionActions)
  where
    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestPublicRootPeers :: Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers n =
      publicRootPeersProvider publicRootTracer
                              timeout
                              DNS.defaultResolvConf
                              publicRootPeers
                              ($ n)

