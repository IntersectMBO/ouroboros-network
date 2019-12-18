{-# LANGUAGE ScopedTypeVariables #-}

module Test.Async.Socket (tests) where

import           Control.Exception
import           Control.Concurrent

import qualified System.Win32.Async as Async

import           Network.Socket (SockAddr (..))
import qualified Network.Socket as Socket

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.QuickCheck.Instances.ByteString ()

tests :: TestTree
tests =
  testGroup "Async.Socket"
  [ testCase "interruptible connect"
      test_interruptible_connect
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
