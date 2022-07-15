{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Monad (forever)
import qualified System.Directory as Dir
import           System.IO (hFlush, stdout)
import           System.IO.Temp
import           Test.Tasty

import qualified Test.Ouroboros.Storage
import           Test.Util.TestEnv

main :: IO ()
main = runTests `race_` heartbeat
  where
    runTests = do
      sysTmpDir <- Dir.getTemporaryDirectory
      withTempDirectory sysTmpDir "cardano-s-m" $ \tmpDir ->
        defaultMainWithTestEnv defaultTestEnvConfig (tests tmpDir)
    heartbeat = forever $ threadDelay (30 * 1_000_000)  >> putChar '.' >> hFlush stdout

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
