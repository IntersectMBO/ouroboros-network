module Main (main) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Data.Bits

import           System.IO
import           System.Win32.NamedPipes

import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

pipeName :: String
pipeName = "\\\\.\\pipe\\nWin32-named-pipes-test"

tests :: TestTree
tests =
  testGroup "Win32-named-pipes"
  [ testCase "interruptible connectNamedPipe"
      test_interruptible_connectNamedPipe
  ]


-- | This test would fail (deadlock) if the blocking call `connectNamedPipe`,
-- would not be interruptible.
--
test_interruptible_connectNamedPipe :: IO ()
test_interruptible_connectNamedPipe = do
    hpipe <- createNamedPipe pipeName
                            pIPE_ACCESS_DUPLEX
                            (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                            pIPE_UNLIMITED_INSTANCES
                            512
                            512
                            0
                            Nothing
    tid <- forkIO (connectNamedPipe hpipe Nothing)
    threadDelay 100
    killThread tid
    pipeToHandle hpipe pipeName ReadWriteMode
      >>= hClose
