
module Test.Ouroboros.Network.BlockFetch (tests) where

import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.ChainGenerators (TestChainFork(..))

import           Control.Monad.IOSim
import           Control.Tracer (nullTracer)

import qualified Ouroboros.Network.Chain            as Chain
import           Ouroboros.Network.AnchoredFragment as AnchoredFragment
import           Ouroboros.Network.BlockFetch.Examples


--
-- The list of all tests
--

tests :: TestTree
tests = testGroup "BlockFetch"
  [ testProperty "fetch example 1" prop_blockFetchExample1
  ]


--
-- Properties
--

prop_blockFetchExample1 :: TestChainFork -> Bool
prop_blockFetchExample1 (TestChainFork common fork1 fork2) =
    -- So far, this just checks it runs to completion without deadlocking.
    -- TODO: use the tracers and check properties of the trace.
    runSimOrThrow (blockFetchExample1
                     nullTracer nullTracer
                     common' forks) == ()
  where
    -- TODO: consider making a specific generator for anchored fragment forks
    common' = chainToAnchoredFragment common
    fork1'  = chainToAnchoredFragment fork1
    fork2'  = chainToAnchoredFragment fork2
    forks   = [fork1', fork2']

    chainToAnchoredFragment =
        AnchoredFragment.fromNewestFirst Chain.genesisPoint
      . Chain.chainToList

