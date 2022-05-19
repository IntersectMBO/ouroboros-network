module Test.Util.ChainUpdates.Tests (tests) where

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Util.ChainUpdates

import           Ouroboros.Consensus.Config

tests :: TestTree
tests = testGroup "Test.Util.ChainUpdates"
    [ testProperty "genChainUpdates" $ prop_genChainUpdates k updatesToGenerate
    ]
  where
    k = SecurityParam 3
    updatesToGenerate = 100
