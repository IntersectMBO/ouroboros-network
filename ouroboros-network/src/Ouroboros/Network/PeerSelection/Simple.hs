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
  -> PeerSelectionTargets
  -> Map Socket.SockAddr PeerAdvertise
  -- ^ static local root peers
  -> [(DomainAddress, PeerAdvertise)]
  -- ^ local root peers
  -> [DomainAddress]
  -- ^ public root peers
  -> PeerStateActions Socket.SockAddr peerconn IO
  -> (Async IO () -> PeerSelectionActions Socket.SockAddr peerconn IO -> IO a)
  -- ^ continuation, which allows to tra
  -> IO a
withPeerSelectionActions localRootTracer publicRootTracer targets staticLocalRootPeers localRootPeers publicRootPeers peerStateActions k = do
    localRootsVar <- newTVarIO Map.empty
    withTimeoutSerial $ \timeout ->
      withAsync
        (localRootPeersProvider
          localRootTracer
          timeout
          DNS.defaultResolvConf
          localRootsVar
          localRootPeers)
        $ \thread ->
          k thread
            PeerSelectionActions {
                readPeerSelectionTargets = pure targets,
                readLocalRootPeers = do
                  localRoots <- readTVar localRootsVar
                  pure (foldr Map.union staticLocalRootPeers localRoots),
                requestPublicRootPeers = requestPublicRootPeers timeout,
                requestPeerGossip = \_ -> pure [],
                peerStateActions
              }
  where
    -- For each call we re-initialise the dns library which forces reading
    -- `/etc/resolv.conf`:
    -- https://github.com/input-output-hk/cardano-node/issues/731
    requestPublicRootPeers :: TimeoutFn IO -> Int -> IO (Set Socket.SockAddr, DiffTime)
    requestPublicRootPeers timeout n =
      publicRootPeersProvider publicRootTracer timeout DNS.defaultResolvConf publicRootPeers ($ n)

