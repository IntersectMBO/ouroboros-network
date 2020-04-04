{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
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
import           Data.Function (on)
import           Data.Foldable (all, foldl', traverse_)
import           GHC.IO.Exception (IOException (..))

import           System.IOManager
import qualified System.Win32.Async.Socket                 as Async
import qualified System.Win32.Async.Socket.ByteString      as Async
import qualified System.Win32.Async.Socket.ByteString.Lazy as Async.Lazy

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
  , testCase "terminate accept via close"
      test_close_accept
  , testProperty "send and recv"
      (ioProperty . prop_send_recv)
  , testProperty "sendTo and recvFrom"
      (ioProperty . prop_sendTo_recvFrom)
  , testProperty "PingPong test"
      prop_PingPong
  , testProperty "PingPongPipelined test"
      prop_PingPongPipelined
  , testGroup "vectored io"
    [ testProperty "PingPong test"
        prop_PingPongLazy
    , testProperty "PingPongPipelined test"
        prop_PingPongPipelinedLazy
    ]
  ]

-- The stock 'connect' is not interruptible.  This tests is not reliable on
-- Windows because of using loopback device.
--
test_interruptible_connect :: IO ()
test_interruptible_connect =
    withIOManager $ \ioManager ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \ (fd_in, fd_out) -> do
          lock <- newEmptyMVar
          associateWithIOManager ioManager (Right fd_in)
          associateWithIOManager ioManager (Right fd_out)
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


-- | Verify that closing a socket will terminate the `Network.Socket.accept`
-- call.
--
test_close_accept :: IO ()
test_close_accept =
    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \fd -> do
        (v :: MVar SomeException) <- newEmptyMVar
        let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
        Socket.bind fd addr
        Socket.listen fd 1
        _ <- forkIO $
          void (Socket.accept fd) `catch` putMVar v
        threadDelay 1000
        -- THIS IS WIRED: it should block if `close` does not run!
        Socket.close fd
        e <- takeMVar v
        case fromException e of
          Nothing    -> assertFailure $ "wrong exception: " ++ show e
          Just err -> do
            -- `WSAEINTR` which is explained as:
            -- ```
            -- Interrupted function call.
            -- A blocking operation was interrupted by a call to WSACancelBlockingCall.
            -- ```
            -- It might be the case that windows is using something similar to
            -- WSACancelBlockingCall to cancel the thread which is blocked on
            -- `accept` when the socket was closed.
            -- 
            -- TODO: `ioe_errno` returns `Nothing`, the wsa errors are not in
            -- `errtable` in base/cbits/Win32Utils.c used by `failWith`.  We
            -- should improve `wsaFailWith` and not use `failWith`.
            "Interrupted function call (WSAEINTR)" @=? ioe_description err


recvLen :: Socket -> Int -> IO BL.ByteString
recvLen sock = go []
  where
    go bufs !l | l <= 0    = pure $ BL.fromChunks (reverse bufs)
               | otherwise = do
                  buf <- Async.recv sock l
                  go (buf : bufs) (l - BS.length buf)


prop_send_recv :: LargeNonEmptyBS -> IO Bool
prop_send_recv (LargeNonEmptyBS bs _size) =
    withIOManager $ \ioManager ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \ (fd_in, fd_out) -> do
          v <- newEmptyMVar
          syncVar <- newEmptyMVar
          associateWithIOManager ioManager (Right fd_in)

          mainThread <- myThreadId

          _ <- forkIO $ handle (\e -> throwTo mainThread e >> ioError e) $ do
              let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
              Socket.bind fd_out addr
              addr' <- Socket.getSocketName fd_out
              Socket.listen fd_out 1024
                `catch` \(e :: IOException) -> putStrLn ("listen errored: " ++ displayException e) >> throwIO e
              putMVar syncVar addr'
              (fd, _) <- Async.accept fd_out
                          `catch` \(e :: IOException) -> putStrLn ("accept errored: " ++ displayException e) >> throwIO e

              associateWithIOManager ioManager (Right fd)
              bs' <- BL.toStrict <$> recvLen fd (BS.length bs)
              putMVar v bs'

          _ <- forkIO $ handle (\e -> throwTo mainThread e >> ioError e) $ do
            -- wait for the other end to start listening
            addr' <- takeMVar syncVar
            Socket.connect fd_in addr'
            Async.sendAll fd_in bs
              `catch` \(e :: IOException) -> putStrLn ("sendAll errored: " ++ displayException e) >> throwIO e

          bs' <- takeMVar v
          pure $ bs == bs'


--  | Like recv but using `recvFrom`.  It assumes that each 'recvFrom' will
--  return the same address otherwise it will throw an exception.
--
recvFromLen :: Socket -> Int -> IO (BL.ByteString, Socket.SockAddr)
recvFromLen sock len0 = do
    res <- go [] len0
    case res of
      as@((_, addr) : _)
          | all (uncurry $ on (==) snd) (zip as (tail as))
          -> pure (BL.fromChunks (fst `map` as), addr)

          -- not all addresses where the same
          | otherwise
          -> throwIO $ userError "recvFromLen: recevied from various addresses"

      []  -> throwIO $ userError "recvFromLen: requesting less than zero bytes is not supported"


  where
    go as !l | l <= 0    = pure $ reverse as
             | otherwise = do
                a@(buf, _) <- Async.recvFrom sock l
                go (a : as) (l - BS.length buf)


-- TODO: this test fails when using 'LargeNonEmptyBS' generator.
--
prop_sendTo_recvFrom :: NonEmptyBS -> IO Bool
prop_sendTo_recvFrom (NonEmptyBS bs) =
    withIOManager $ \ioManager ->
      bracket
        ((,) <$> Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol
             <*> Socket.socket Socket.AF_INET Socket.Datagram Socket.defaultProtocol)
        (\(x, y) -> Socket.close x >> Socket.close y)
        $ \ (fd_in, fd_out) -> do
          v <- newEmptyMVar
          syncVar <- newEmptyMVar
          associateWithIOManager ioManager (Right fd_in)
          associateWithIOManager ioManager (Right fd_out)

          mainThread <- myThreadId

          _ <- forkIO $ handle (\e -> throwTo mainThread e >> ioError e) $ do
              let addr = SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1))
              Socket.bind fd_in addr
              addr' <- Socket.getSocketName fd_in
              putMVar syncVar addr'

              (bs', _) <- recvFromLen fd_in (BS.length bs)
              putMVar v (BL.toStrict bs')

          _ <- forkIO $ handle (\e -> throwTo mainThread e >> ioError e) $ do
            -- wait for the other end to start listening
            addr' <- takeMVar syncVar
            Async.sendAllTo fd_out bs addr'

          bs' <- takeMVar v
          pure $ bs == bs'


--
-- BinaryChannels using 'System.Win32.Socket.Bytestring' or
-- 'System.Win32.Socket.ByteString.Lazy' (vectored io).
--


-- | 'BinaryChannel' defined in terms of
-- 'System.Win32.Socket.Bytestring.sendAll' and
-- 'System.Win32.Socket.Bytestring.recv'
--
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


-- | Like 'socketToBinaryChannel' but using
-- 'System.Win32.Async.Socket.ByteString.Lazy' (vectored io).
--
socketToLazyBinaryChannel :: Binary a
                          => Socket
                          -> BinaryChannel a
socketToLazyBinaryChannel sock = BinaryChannel { readChannel, writeChannel, closeChannel }
  where
    recvLazyLen :: Int -> IO BL.ByteString
    recvLazyLen = go []
      where
        go bufs !l | l <= 0    = return $ BL.concat (reverse bufs)
                   | otherwise = do
                      buf <- Async.Lazy.recv sock l
                      go (buf : bufs) (l - fromIntegral (BL.length buf))

    readChannel b = do
      -- putStrLn "readChannel: header"
      s <- decode <$> Async.Lazy.recv sock 8
      -- putStrLn $ "readChannel: header: " ++ show s
      if b
        then do
          -- putStrLn $ "recvLazyLen: " ++ show s
          bs' <- recvLazyLen s
          -- putStrLn $ "recvLazyLen: done"
          pure $ Just $ decode bs'
        else pure Nothing

    writeChannel b a =
      do
        let bs :: BL.ByteString
            bs = encode a
            size :: Int
            size = bool (+1) id b (fromIntegral $ BL.length bs)
        Async.Lazy.sendAll sock (encode size)
        Async.Lazy.sendAll sock bs
      `catch` (\(e :: IOException) -> putStrLn (show e) >> throwIO e)

    closeChannel = Socket.close sock


--
-- Ping Pong Tests
--

test_PingPong :: (forall a. Binary a => Socket -> BinaryChannel a)
              -> Int
              -> Blocking
              -> ByteString
              -> IO Bool
test_PingPong createBinaryChannel n blocking bs =
    withIOManager $ \ioManager ->
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
              (socket, _remoteAddr) <- Socket.accept sockIn
              associateWithIOManager ioManager (Right socket)
              let channel = createBinaryChannel socket
              unmask (runPingPongServer channel (constPingPongServer @ByteString))
                `catch` \(e :: IOException) -> do
                    putStrLn ("ping-pong server cought: " ++ show e)
                    throwIO e
            `finally` putMVar lock ()

          -- run a PingPong client, at this stage server socket is in
          -- listening state accepting connections.
          Socket.connect sockOut addr'
          associateWithIOManager ioManager (Right sockOut)
          let channelOut = createBinaryChannel sockOut
          res <- runPingPongClient channelOut blocking tid (constPingPongClient n bs)
            `catch` \(e :: IOException) -> do
              putStrLn ("ping-pong client cought: " ++ show e)
              throwIO e

          -- this lock asserts that the server was terminated
          -- TODO: check that it was killed by the right exception
          takeMVar lock
          pure $ case blocking of
            NonBlocking -> res == replicate n bs
            _           -> res == replicate (pred n) bs


prop_PingPong :: Positive Int
              -> Blocking
              -> LargeNonEmptyBS
              -> Property
prop_PingPong (Positive n) blocking (LargeNonEmptyBS bs _bufSize) =
    ioProperty $ test_PingPong socketToBinaryChannel n blocking bs

prop_PingPongLazy :: Positive Int
                  -> Blocking
                  -> LargeNonEmptyBS
                  -> Property
prop_PingPongLazy (Positive n) blocking (LargeNonEmptyBS bs _bufSize) =
    ioProperty $ test_PingPong socketToLazyBinaryChannel n blocking bs


--
-- Pipelined Ping Pong Tests
--

test_PingPongPipelined :: (forall a. Binary a => Socket -> BinaryChannel a)
                       -> Blocking
                       -> [ByteString]
                       -> IO Bool
test_PingPongPipelined createBinaryChannel blocking bss =
    withIOManager $ \ioManager ->
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
              associateWithIOManager ioManager (Right socket)
              let channel = createBinaryChannel socket
              unmask (runPingPongServer channel (constPingPongServer @ByteString))
                `catch` \(e :: IOException) -> do
                  putStrLn ("ping-pong server cought: " ++ show e)
                  throwIO e
            `finally` putMVar lock ()

          -- run a PingPong client, at this stage server socket is in
          -- listening state accepting connections.
          Socket.connect sockOut addr'
          associateWithIOManager ioManager (Right sockOut)
          let channelOut = createBinaryChannel sockOut
          res <-
            runPingPongClientPipelined channelOut blocking tid bss
              `catch` \(e :: IOException) -> do
                putStrLn ("ping-pong client cought: " ++ show e)
                throwIO e
          -- this lock asserts that the server was terminated
          -- TODO: check that it was killed by the right exception
          takeMVar lock
          pure $ case blocking of
            NonBlocking -> case res of
              Just bss' -> bss == bss'
              Nothing   -> False
            _           -> True -- if we evalute this case branch, it means that
                                     -- killing blocked thread did not deadlock.


prop_PingPongPipelined :: Blocking
                       -> NonEmptyList LargeNonEmptyBS
                       -> Property
prop_PingPongPipelined blocking (NonEmpty bss) =
    ioProperty $
      test_PingPongPipelined socketToBinaryChannel blocking (map getLargeNonEmptyBS bss)

prop_PingPongPipelinedLazy :: Blocking
                       -> NonEmptyList LargeNonEmptyBS
                       -> Property
prop_PingPongPipelinedLazy blocking (NonEmpty bss) =
    ioProperty $
      test_PingPongPipelined socketToLazyBinaryChannel blocking (map getLargeNonEmptyBS bss)
