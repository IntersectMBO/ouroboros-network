{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Async.Socket (tests) where

import           Control.Exception
import           Control.Concurrent
import           Data.Functor (void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import qualified System.Win32.Async.IOManager as Async
import qualified System.Win32.Async.Socket as Async
import qualified System.Win32.Async.Socket.ByteString as Async

import           Network.Socket (Socket, SockAddr (..))
import qualified Network.Socket as Socket


import           Test.Generators hiding (tests)

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

tests :: TestTree
tests =
  testGroup "Async.Socket"
  [ testCase "interruptible connect"
      test_interruptible_connect
  , testCase "interruptible accept"
      test_interruptible_accept
  , testProperty "send and recv"
      (ioProperty . prop_send_recv)
  ]

-- The stock 'connect' is not interruptible.  This tests is not reliable on
-- Windows because of using loopback device.
--
test_interruptible_connect :: IO ()
test_interruptible_connect =
    Async.withIOManager $ \iocp ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \ (fd_in, fd_out) -> do
          lock <- newEmptyMVar
          Async.associateWithIOCompletionPort (Right fd_in)  iocp
          Async.associateWithIOCompletionPort (Right fd_out) iocp
          (v :: MVar (Maybe SomeException)) <- newEmptyMVar
          let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
          Socket.bind fd_in addr
          addr' <- Socket.getSocketName fd_in
          Socket.listen fd_in 1
          tid <- mask_ $ forkIOWithUnmask $ \unmask ->
            do
              putMVar lock ()
              unmask (Async.connect fd_out addr')
                `catch` (putMVar v . Just)
            `finally` tryPutMVar v Nothing
          takeMVar lock
          killThread tid
          me <- takeMVar v
          case me of
            Nothing -> assertFailure "connect finished before ThreadKilled was delivered"
            Just e -> do
              -- check that the 'ThreadKilled' exception was caught.
              assertEqual "wrong exception"
                          (Just ThreadKilled)
                          (fromException e :: Maybe AsyncException)


test_interruptible_accept :: IO ()
test_interruptible_accept =
    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \ fd -> do
        (v :: MVar SomeException) <- newEmptyMVar
        let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
        Socket.bind fd addr
        Socket.listen fd 1
        tid <- forkIO $ void $
          Async.accept fd
          `catch` (\e -> putMVar v e >> throwIO e)
        threadDelay 100
        killThread tid
        e <- takeMVar v
        assertEqual "wrong exception"
                    (Just ThreadKilled)
                    (fromException e :: Maybe AsyncException)

recvLen :: Socket -> Int -> IO ByteString
recvLen s l0 = go l0 []
  where
    go !l bufs | l <= 0 = pure $ BS.concat $ reverse bufs
               | otherwise = do
                  buf <- Async.recv s l
                  go (l - BS.length buf) (buf : bufs)

prop_send_recv :: LargeNonEmptyBS -> IO Bool
prop_send_recv (LargeNonEmptyBS bs _size) =
    Async.withIOManager $ \iocp ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \ (fd_in, fd_out) -> do
          v <- newEmptyMVar
          syncVar <- newEmptyMVar
          Async.associateWithIOCompletionPort (Right fd_in)  iocp

          _ <- forkIO $ do
            let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
            Socket.bind fd_out addr
            addr' <- Socket.getSocketName fd_out
            Socket.listen fd_out 1024
              `catch` \(e :: IOException) -> putStrLn ("listen errored: " ++ displayException e) >> throwIO e
            putMVar syncVar addr'
            (fd, _) <- Async.accept fd_out
                        `catch` \(e :: IOException) -> putStrLn ("accept errored: " ++ displayException e) >> throwIO e

            Async.associateWithIOCompletionPort (Right fd) iocp
            bs' <- recvLen fd (BS.length bs)
            putMVar v bs'

          _ <- forkIO $ do
            -- wait for the other end to start listening
            addr' <- takeMVar syncVar
            Socket.connect fd_in addr'
            Async.sendAll fd_in bs
              `catch` \(e :: IOException) -> putStrLn ("sendAll errored: " ++ displayException e) >> throwIO e

          bs' <- takeMVar v
          pure $ bs == bs'
