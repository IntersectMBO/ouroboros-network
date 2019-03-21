module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import           Test.Tasty

import qualified Test.Ouroboros.Storage
import           Test.Ouroboros.Storage.VolatileDB.StateMachine hiding (tests)

main :: IO ()
main = do
  showLabelledExamples
  sysTmpDir <- Dir.getTemporaryDirectory
  withTempDirectory sysTmpDir "cardano-s-m" $ \tmpDir ->
    defaultMain (tests tmpDir)

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests tmpDir
  ]
