{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Async.Socket (tests) where

import           Control.Exception
import           Control.Concurrent
import           Data.Binary
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Functor (void)
import           Data.Foldable (foldl', traverse_)

import qualified System.Win32.Async.IOManager as Async
import qualified System.Win32.Async.Socket as Async
import qualified System.Win32.Async.Socket.ByteString as Async

import           Network.Socket (Socket, SockAddr (..))
import qualified Network.Socket as Socket


import           Test.Generators hiding (tests)
import           Test.Async.PingPong

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
  , testProperty "PingPong test"
      prop_PingPong
  , testProperty "PingPongPipelined test"
      prop_PingPongPipelined
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

recvLen :: Socket -> Int -> IO BL.ByteString
recvLen sock = go []
  where
    go bufs !l | l <= 0    = pure $ BL.fromChunks (reverse bufs)
               | otherwise = do
                  buf <- Async.recv sock l
                  go (buf : bufs) (l - BS.length buf)

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
            bs' <- BL.toStrict <$> recvLen fd (BS.length bs)
            putMVar v bs'

          _ <- forkIO $ do
            -- wait for the other end to start listening
            addr' <- takeMVar syncVar
            Socket.connect fd_in addr'
            Async.sendAll fd_in bs
              `catch` \(e :: IOException) -> putStrLn ("sendAll errored: " ++ displayException e) >> throwIO e

          bs' <- takeMVar v
          pure $ bs == bs'


socketToBinaryChannel :: Binary a
                      => Socket
                      -> BinaryChannel a
socketToBinaryChannel sock = BinaryChannel { readChannel, writeChannel, closeChannel }
  where
    readChannel b = do
      s <- decode . BL.fromStrict <$> Async.recv sock 8
      if b
        then do
          bs' <- recvLen sock s
          pure $ Just $ decode bs'
        else pure Nothing

    writeChannel b a = do
      let chunks :: [ByteString]
          chunks = BL.toChunks (encode a)
          size   :: Int
          size = bool (+1) id b $ foldl' (\x y -> x + BS.length y) 0 chunks
      _ <- Async.sendAll sock (BL.toStrict $ encode size)
      traverse_ (\chunk -> Async.sendAll sock chunk) chunks

    closeChannel = Socket.close sock


prop_PingPong :: Positive Int
              -> Blocking
              -> LargeNonEmptyBS
              -> Property
prop_PingPong (Positive n) blocking (LargeNonEmptyBS bs _bufSize) =
    ioProperty $ Async.withIOManager $ \iocp ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \(sockIn, sockOut) -> do
          lock <- newEmptyMVar

          -- fork a PingPong server
          let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
          Socket.bind sockIn addr
          addr' <- Socket.getSocketName sockIn
          Socket.listen sockIn 1024
          tid <- mask_ $ forkIOWithUnmask $ \unmask ->
            do
              (socket, _) <- Socket.accept sockIn
              Async.associateWithIOCompletionPort (Right socket) iocp
              let channel = socketToBinaryChannel socket
              unmask (runPingPongServer channel (constPingPongServer @ByteString))
            `finally` putMVar lock ()

          -- run a PingPong client, at this stage server socket is in
          -- listening state accepting connections.
          Socket.connect sockOut addr'
          Async.associateWithIOCompletionPort (Right sockOut) iocp
          let channelOut = socketToBinaryChannel sockOut
          res <- runPingPongClient channelOut blocking tid (constPingPongClient n bs)

          -- this lock asserts that the server was terminated
          -- TODO: check that it was killed by the right exception
          takeMVar lock
          pure $ case blocking of
            NonBlocking -> res == replicate n bs
            _           -> res == replicate (pred n) bs



prop_PingPongPipelined :: Blocking
                       -> NonEmptyList LargeNonEmptyBS
                       -> Property
prop_PingPongPipelined blocking (NonEmpty bss0) =
    ioProperty $ Async.withIOManager $ \iocp ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \(sockIn, sockOut) -> do
          let bss = map getLargeNonEmptyBS bss0
          lock <- newEmptyMVar

          -- fork a PingPong server
          let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
          Socket.bind sockIn addr
          addr' <- Socket.getSocketName sockIn
          Socket.listen sockIn 1024
          tid <- mask_ $ forkIOWithUnmask $ \unmask ->
            do
              (socket, _) <- Socket.accept sockIn
              Async.associateWithIOCompletionPort (Right socket) iocp
              let channel = socketToBinaryChannel socket
              unmask (runPingPongServer channel (constPingPongServer @ByteString))
            `finally` putMVar lock ()

          -- run a PingPong client, at this stage server socket is in
          -- listening state accepting connections.
          Socket.connect sockOut addr'
          Async.associateWithIOCompletionPort (Right sockOut) iocp
          let channelOut = socketToBinaryChannel sockOut
          res <- runPingPongClientPipelined channelOut blocking tid bss

          -- this lock asserts that the server was terminated
          -- TODO: check that it was killed by the right exception
          takeMVar lock
          pure $ case blocking of
            NonBlocking -> case res of
              Just bss' -> bss == bss'
              Nothing   -> False
            _           -> True -- if we evalute this case branch, it means that
                                     -- killing blocked thread did not deadlock.

