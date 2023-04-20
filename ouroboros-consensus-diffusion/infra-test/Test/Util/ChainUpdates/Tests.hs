module Test.Util.ChainUpdates.Tests (tests) where

import           Ouroboros.Consensus.Config
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.ChainUpdates

tests :: TestTree
tests = testGroup "Test.Util.ChainUpdates"
    [ testProperty "genChainUpdates" $ prop_genChainUpdates k updatesToGenerate
    ]
  where
    k = SecurityParam 3
    updatesToGenerate = 100
