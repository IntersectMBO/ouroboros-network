module Test.Ouroboros.Storage.FS (
    tests
  ) where

import qualified Test.Ouroboros.Storage.FS.StateMachine as StateMachine
import           Test.Tasty (TestTree, testGroup)

tests :: FilePath -> TestTree
tests tmpDir = testGroup "HasFS" [
      StateMachine.tests tmpDir
    ]
