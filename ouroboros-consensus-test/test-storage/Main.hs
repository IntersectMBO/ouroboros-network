module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import qualified Test.Ouroboros.Storage
import           Test.Tasty
import           Test.Util.TestEnv

main :: IO ()
main = do
  sysTmpDir <- Dir.getTemporaryDirectory
  withTempDirectory sysTmpDir "cardano-s-m" $ \tmpDir ->
    defaultMainWithTestEnv defaultTestEnvConfig (tests tmpDir)

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests tmpDir
  ]
