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
    [ Mux.TraceRemoteCounters Mux.noCounts
    , Mux.TraceLocalCounters Mux.noCounts
    , Mux.TraceEgressCounters scheduling
    , Mux.TraceEgressBurst 0 burst
    ]
  where
    scheduling = Mux.SchedulingCounts 0 0 0 0 0 0 0 0 0 0 [] 0 0 0 0 0 0 [] [] 0 []
    burst      = Mux.Burst (Time 0) (Time 0) False 0 0 Map.empty 0 0 0 0 0

-- | A new constructor fails to compile here until 'events' lists it.
_covered :: Mux.CountersTrace -> ()
_covered = \case
    Mux.TraceRemoteCounters {} -> ()
    Mux.TraceLocalCounters {}  -> ()
    Mux.TraceEgressCounters {} -> ()
    Mux.TraceEgressBurst {}    -> ()

prop_counters_namespaces :: Property
prop_counters_namespaces =
         counterexample ("produced, not enumerated: " ++ show missing) (null missing)
    .&&. counterexample ("enumerated, never produced: " ++ show extra) (null extra)
  where
    produced = map namespaceFor events :: [Namespace Mux.CountersTrace]
    listed   = allNamespaces :: [Namespace Mux.CountersTrace]
    missing  = filter (`notElem` listed) produced
    extra    = filter (`notElem` produced) listed
