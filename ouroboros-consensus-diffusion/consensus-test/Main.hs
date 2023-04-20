module Main (main) where

import qualified Test.Consensus.HardFork.Combinator (tests)
import qualified Test.Consensus.Node (tests)
import           Test.Tasty
import           Test.Util.TestEnv (defaultMainWithTestEnv,
                     defaultTestEnvConfig)

main :: IO ()
main = defaultMainWithTestEnv defaultTestEnvConfig tests

tests :: TestTree
tests =
  testGroup "ouroboros-consensus"
  [ Test.Consensus.Node.tests
  , testGroup "HardFork" [
        testGroup "Combinator" [
            Test.Consensus.HardFork.Combinator.tests
          ]
      ]
  ]
