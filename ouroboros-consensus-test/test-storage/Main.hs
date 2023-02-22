{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Monad (forever)
import           System.IO (hFlush, stdout)
import qualified Test.Ouroboros.Storage
import           Test.Tasty
import           Test.Util.TestEnv

main :: IO ()
main = runTests `race_` heartbeat
  where
    runTests  = defaultMainWithTestEnv defaultTestEnvConfig tests
    heartbeat = forever $ threadDelay (30 * 1_000_000)  >> putChar '.' >> hFlush stdout

tests :: TestTree
tests = testGroup "ouroboros-storage" [
    Test.Ouroboros.Storage.tests
  ]
