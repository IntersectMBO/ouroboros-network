{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}

 module Test.Ouroboros.Network.PeerSelection.RootPeersDNS (
  tests
  ) where

import           Ouroboros.Network.PeerSelection.RootPeersDNS
import           Ouroboros.Network.PeerSelection.Types (PeerAdvertise)

import           Data.Map.Strict (Map)
import           Data.Void (Void)
import qualified Network.DNS.Resolver as DNSResolver
import           Control.Monad.IOSim
import qualified Control.Monad.Class.MonadTimer as MonadTimer
import           Control.Tracer (Tracer(Tracer))
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO, readTVar)

import           Test.QuickCheck
import           Test.Tasty (TestTree, testGroup)


tests :: TestTree
tests =
  testGroup "RootPeersDNS"
  [
  ]


mockLocalRootPeersProvider :: [(Int, Map RelayAddress PeerAdvertise)] -> IOSim s Void
mockLocalRootPeersProvider localRootPeers = do
      localRootPeersVar <- newTVarIO localRootPeers
      resultVar <- newTVarIO mempty

      localRootPeersProvider tracer
                             MonadTimer.timeout
                             DNSResolver.defaultResolvConf
                             resultVar
                             (readTVar localRootPeersVar)
                             dnsActions
 where
   tracer = Tracer (traceM @(TraceLocalRootPeers Failure))

   dnsResolverResource _ = return (constantResource ())
   dnsAsyncResolverResource _ = return (constantResource ())
   dnsLookupAWithTTL timeout _ _ domain = return (Right [])

   dnsActions = DNSActions {
                  dnsResolverResource,
                  dnsAsyncResolverResource,
                  dnsLookupAWithTTL
                }

prop_local_preservesGroupNumberAndTargets :: [(Int, Map RelayAddress PeerAdvertise)] -> Property
prop_local_preservesGroupNumberAndTargets localRootPeers = property True
