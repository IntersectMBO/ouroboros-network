module Test.Ouroboros.Consensus.ChainGenerator.Tests (
  tests,
  ) where

import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial as A
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest as H
import qualified Test.Tasty as TT

-----

tests :: TT.TestTree
tests = TT.testGroup "ChainGenerator" $ []
    <> A.tests
    <> BV.tests
    <> C.tests
    <> H.tests
