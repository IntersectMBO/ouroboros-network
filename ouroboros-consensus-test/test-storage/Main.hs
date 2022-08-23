module Main (main) where

import qualified System.Directory as Dir
import           System.IO.Temp
import           Test.Tasty

import qualified Test.Ouroboros.Storage

main :: IO ()
main = do
  sysTmpDir <- Dir.getTemporaryDirectory
  withTempDirectory sysTmpDir "cardano-s-m" $ \tmpDir ->
    defaultMain (tests tmpDir)

tests :: FilePath -> TestTree
tests tmpDir =
  testGroup "ouroboros-storage"
  [ Test.Ouroboros.Storage.tests tmpDir
  ]

-- A bug in CI requires to modify this component; If you encountered
-- ```
-- test-storage: cannot execute binary file: Exec format error
-- ```
-- increment the following /unlucky/ counter and cross fingers:
--
-- 1
