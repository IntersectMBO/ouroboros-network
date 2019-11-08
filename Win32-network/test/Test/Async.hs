{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Async (tests) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Exception (AsyncException (..), catch, bracket, throwIO)
import           Data.Functor (void)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import           System.Win32
import           System.Win32.NamedPipes
import qualified System.Win32.Async as Win32.Async

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck

import           Test.NamedPipes ( Blocking (..)
                                 , test_PingPong
                                 , test_PingPongPipelined
                                 , handleToBinaryChannel
                                 , LargeNonEmptyBS (..))

pipeName :: String
pipeName = "\\\\.\\pipe\\test-Win32-network-async"

tests :: TestTree
tests =
  testGroup "Win32.Async"
  [ testCase "interruptible readHandle"
      test_interruptible_readHandle
  , testCase "interruptible readHandle_2"
      test_interruptible_readHandle_2
  , testCase "interruptible writeHandle"
      test_interruptible_writeHandle
  , testProperty "async reads and writes"
      prop_async_read_and_writes
  , testProperty "PingPong test"
      (prop_PingPong)
  , testProperty "PingPongPipelined test"
      (prop_PingPongPipelined)
  ]

-- | Check if 'readHandle'`is interruptible
--
test_interruptible_readHandle :: IO ()
test_interruptible_readHandle = Win32.Async.withIOManager $ \iocp ->
    bracket ((,) <$> createNamedPipe pipeName
                                     (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                gENERIC_READ
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_FLAG_OVERLAPPED
                                Nothing)
            (foldMap closeHandle)
            $ \(hpipe, hpipe') -> do
                _ <- Win32.Async.associateWithIOCompletionPort hpipe  iocp
                _ <- Win32.Async.associateWithIOCompletionPort hpipe' iocp
                tid <- forkIO (void $ Win32.Async.readHandle hpipe' 1)
                threadDelay 100
                killThread tid

-- | Interrupt two simultanous reads.
--
test_interruptible_readHandle_2 :: IO ()
test_interruptible_readHandle_2 = Win32.Async.withIOManager $ \iocp -> do
    bracket ((,) <$> createNamedPipe pipeName
                                     (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                (gENERIC_READ .|. gENERIC_WRITE)
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_FLAG_OVERLAPPED
                                Nothing)
            (foldMap closeHandle)
            $ \(hpipe, hpipe') -> do
              _ <- Win32.Async.associateWithIOCompletionPort hpipe  iocp
              _ <- Win32.Async.associateWithIOCompletionPort hpipe' iocp
              tid  <- forkIO (void $ Win32.Async.readHandle hpipe' 1)
              tid' <- forkIO (void $ Win32.Async.readHandle hpipe' 1)
              threadDelay 100
              killThread tid
              killThread tid'


test_interruptible_writeHandle :: IO ()
test_interruptible_writeHandle = Win32.Async.withIOManager $ \iocp -> do
    let bs = BSC.pack $ replicate 100 'a'
    v <- newEmptyMVar
    syncVar <- newEmptyMVar

    bracket
      ((,) <$> createNamedPipe pipeName
                               (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                               (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                               pIPE_UNLIMITED_INSTANCES
                               1
                               1
                               0
                               Nothing
           <*> createFile pipeName
                          gENERIC_WRITE
                          fILE_SHARE_NONE
                          Nothing
                          oPEN_EXISTING
                          fILE_FLAG_OVERLAPPED
                          Nothing)
      (foldMap closeHandle)
      $ \(r, w) -> do
        _ <- Win32.Async.associateWithIOCompletionPort r iocp
        _ <- Win32.Async.associateWithIOCompletionPort w iocp

        tid <- forkIOWithUnmask $ \unmask ->
          void $ do
            putMVar syncVar ()
            unmask (Win32.Async.writeHandle w bs)
          `catch` \(e :: AsyncException) -> putMVar v e >> throwIO e

        -- wait for 'writeHandle'
        takeMVar syncVar
        threadDelay 100
        killThread tid

        e <- takeMVar v
        assertBool "test_interruptible_writeHandle" (ThreadKilled == e)

-- | Run a server and client which both simultanously read and write from a
-- handle.
--
prop_async_read_and_writes :: LargeNonEmptyBS
                           -> LargeNonEmptyBS
                           -> Property
prop_async_read_and_writes (LargeNonEmptyBS bsIn bufSizeIn) (LargeNonEmptyBS bsOut bufSizeOut) =
    ioProperty $ Win32.Async.withIOManager $ \iocp -> do
      threadDelay 100

      syncVarStart <- newEmptyMVar
      syncVarEnd   <- newEmptyMVar
      clientVar <- newEmptyMVar
      serverVar <- newEmptyMVar

      -- fork a server
      _ <- forkIO $
        bracket
            (createNamedPipe pipeName
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             (fromIntegral bufSizeIn)
                             (fromIntegral bufSizeOut)
                             0
                             Nothing)
            closeHandle
            $ \h -> do
              -- associate 'h' with  I/O completion 'port'
              _ <- Win32.Async.associateWithIOCompletionPort h iocp
              putMVar syncVarStart ()
              Win32.Async.connectNamedPipe h
              void $ forkIO $
                Win32.Async.readHandle h (BS.length bsIn)
                  >>= putMVar serverVar
              void $ forkIO $ Win32.Async.writeHandle h bsOut
              takeMVar syncVarEnd


      -- fork a client
      _ <- forkIO $ do
        takeMVar syncVarStart
        bracket
          (createFile pipeName
                      (gENERIC_READ .|. gENERIC_WRITE)
                      fILE_SHARE_NONE
                      Nothing
                      oPEN_EXISTING
                      fILE_FLAG_OVERLAPPED
                      Nothing)
          closeHandle
          $ \h -> do
            -- associate 'h' with  I/O completion 'port'
            _ <- Win32.Async.associateWithIOCompletionPort h iocp
            readerAsync <- async $
              Win32.Async.readHandle h (BS.length bsOut)
                >>= putMVar clientVar
            writerAsync <- async $ Win32.Async.writeHandle h bsIn
            _ <- waitBoth readerAsync writerAsync
            putMVar syncVarEnd ()

      bsOut' <- takeMVar clientVar
      bsIn'  <- takeMVar serverVar

      pure $ bsIn == bsIn' && bsOut == bsOut'

prop_PingPong :: NonNegative Int
              -- ^ number of messages exchanged in the ping pong protocol
              -> Blocking
              -> LargeNonEmptyBS
              -> Property
prop_PingPong (NonNegative n) blocking bs0 = ioProperty $ Win32.Async.withIOManager $ \iocp -> do
    test_PingPong
        (\h -> handleToBinaryChannel
                 (\s  -> Win32.Async.readHandle h s)
                 (\bs -> Win32.Async.writeHandle h bs)
                 h)
        Win32.Async.connectNamedPipe
        (\name bufSize -> do
          h <- createNamedPipe name
                               (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                               (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                               pIPE_UNLIMITED_INSTANCES
                               bufSize
                               bufSize
                               0
                               Nothing
          Win32.Async.associateWithIOCompletionPort h iocp
          pure h)
        (\name -> do
          h <- createFile name
                          (gENERIC_READ .|. gENERIC_WRITE)
                          fILE_SHARE_NONE
                          Nothing
                          oPEN_EXISTING
                          fILE_FLAG_OVERLAPPED
                          Nothing
          Win32.Async.associateWithIOCompletionPort h iocp
          pure h)
        n blocking bs0


prop_PingPongPipelined :: Blocking
                       -> Positive Int
                       -> NonEmptyList LargeNonEmptyBS
                       -> Property
prop_PingPongPipelined blocking (Positive bufSize) (NonEmpty bss) =
    ioProperty $ Win32.Async.withIOManager $ \iocp ->
      test_PingPongPipelined
          (\h -> handleToBinaryChannel
                   (\s  -> Win32.Async.readHandle h s)
                   (\bs -> Win32.Async.writeHandle h bs)
                   h)
          Win32.Async.connectNamedPipe
          (\name -> do
            h <- createNamedPipe name
                                 (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                                 (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                 pIPE_UNLIMITED_INSTANCES
                                 (fromIntegral bufSize)
                                 maxBound
                                 0
                                 Nothing
            Win32.Async.associateWithIOCompletionPort h iocp
            pure h)
          (\name -> do
            h <- createFile name
                            (gENERIC_READ .|. gENERIC_WRITE)
                            fILE_SHARE_NONE
                            Nothing
                            oPEN_EXISTING
                            fILE_FLAG_OVERLAPPED
                            Nothing
            Win32.Async.associateWithIOCompletionPort h iocp
            pure h)
          blocking (map getLargeNonEmptyBS bss)
