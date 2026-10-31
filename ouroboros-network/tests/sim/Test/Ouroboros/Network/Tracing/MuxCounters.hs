{-# LANGUAGE LambdaCase #-}

-- | The mux counters' trace instance: every event's namespace is one the
-- instance enumerates, so configuration by namespace reaches it, and the
-- enumeration lists nothing no event has.
module Test.Ouroboros.Network.Tracing.MuxCounters (tests) where

import Cardano.Logging (MetaTrace (..), Namespace)
import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.Map.Strict qualified as Map
import Network.Mux qualified as Mux
import Network.Mux.Tracing ()

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup "Network.Mux.Tracing"
    [ testProperty "every counters event's namespace is enumerated, and only those"
                   (once prop_counters_namespaces)
    ]

-- | One event of each constructor; the namespace depends on nothing else,
-- so the check is exhaustive.
events :: [Mux.CountersTrace]
events =
    [ Mux.TraceRemoteEgress egress
    , Mux.TraceRemoteIngress ingress
    , Mux.TraceLocalEgress egress
    , Mux.TraceLocalIngress ingress
    , Mux.TraceEgressBurst 0 burst
    ]
  where
    egress  = Mux.EgressCounts 0 0 Nothing Map.empty
    ingress = Mux.IngressCounts 0 0 0 0 Map.empty
    burst   = Mux.Burst (Time 0) (Time 0) False 0 0 Map.empty 0 0 0 0 0

-- | A new constructor fails to compile here until 'events' lists it.
_covered :: Mux.CountersTrace -> ()
_covered = \case
    Mux.TraceRemoteEgress {}  -> ()
    Mux.TraceRemoteIngress {} -> ()
    Mux.TraceLocalEgress {}   -> ()
    Mux.TraceLocalIngress {}  -> ()
    Mux.TraceEgressBurst {}   -> ()

prop_counters_namespaces :: Property
prop_counters_namespaces =
         counterexample ("produced, not enumerated: " ++ show missing) (null missing)
    .&&. counterexample ("enumerated, never produced: " ++ show extra) (null extra)
  where
    produced = map namespaceFor events :: [Namespace Mux.CountersTrace]
    listed   = allNamespaces :: [Namespace Mux.CountersTrace]
    missing  = filter (`notElem` listed) produced
    extra    = filter (`notElem` produced) listed
