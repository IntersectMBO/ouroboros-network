module Main (main) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Data.Functor (void)
import           Data.Bits

import           System.Win32
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
  , testCase "interruptible readPipe"
      test_interruptible_readPipe
  , testCase "interruptible readPipe twice (synchronous)"
      test_interruptible_readPipe_sync
  , testCase "interruptible readPipe twice (concurrent)"
      test_interruptible_readPipe_conc
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
    closePipe hpipe

-- | Check if 'readPipe'`is interruptible
--
test_interruptible_readPipe :: IO ()
test_interruptible_readPipe = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    closePipe hpipe'
    closePipe hpipe

-- | Interrupt two consecutive reads.
--
test_interruptible_readPipe_sync :: IO ()
test_interruptible_readPipe_sync = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                        gENERIC_READ
                        fILE_SHARE_NONE
                        Nothing
                        oPEN_EXISTING
                        fILE_ATTRIBUTE_NORMAL
                        Nothing
    tid <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid'
    closePipe hpipe'
    closePipe hpipe


-- | Interrupt two simultanous reads.
--
test_interruptible_readPipe_conc :: IO ()
test_interruptible_readPipe_conc = do
    hpipe <- createNamedPipe pipeName
                             pIPE_ACCESS_DUPLEX
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing
    hpipe' <- createFile pipeName
                         gENERIC_READ
                         fILE_SHARE_NONE
                         Nothing
                         oPEN_EXISTING
                         fILE_ATTRIBUTE_NORMAL
                         Nothing
    tid  <- forkIO (void $ readPipe hpipe' 1)
    tid' <- forkIO (void $ readPipe hpipe' 1)
    threadDelay 100
    killThread tid
    killThread tid'
    closePipe hpipe'
    closePipe hpipe
