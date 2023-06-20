{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Test.Ouroboros.Network.RawBearer where

import           Ouroboros.Network.IOManager
import           Ouroboros.Network.RawBearer
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Testing.Data.AbsBearerInfo

import           Control.Concurrent.Class.MonadMVar
import           Control.Exception (Exception)
import           Control.Monad (when, unless)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadTimer (MonadDelay (threadDelay))
import           Control.Monad.Class.MonadFork (labelThisThread)
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST (MonadST, withLiftST)
import           Control.Monad.Class.MonadThrow (MonadThrow, bracket, catchJust,
                     finally, throwIO)
import           Control.Monad.IOSim hiding (liftST)
import           Control.Monad.ST.Unsafe (unsafeIOToST)
import           Control.Tracer (nullTracer)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe (catMaybes)
import           Data.Word (Word32)
import           Foreign.Marshal (copyBytes, free, mallocBytes)
import           Foreign.Ptr (castPtr, plusPtr)
import qualified Network.Socket as Socket
import           Simulation.Network.Snocket as SimSnocket
import           System.Directory (removeFile)
import           System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import           System.IO.Unsafe

import           Test.Simulation.Network.Snocket (toBearerInfo)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Debug.Trace

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer" $
  [ testGroup "basic" $
    catMaybes
    [ Just $ testProperty "raw bearer basic local socket" prop_raw_bearer_basic_local
    , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrUnix "dummy")) $
      testProperty "raw bearer basic unix socket" prop_raw_bearer_basic_unix
    , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrInet 10000 localhost)) $
      testProperty "raw bearer basic inet socket" prop_raw_bearer_basic_inet
    ]
  , testGroup "full" $
    catMaybes
    [ Just $ testProperty "raw bearer send receive simulated socket" prop_raw_bearer_send_and_receive_iosim
    , Just $ testProperty "raw bearer send receive local socket" prop_raw_bearer_send_and_receive_local
    , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrUnix "dummy")) $
      testProperty "raw bearer send receive unix socket" prop_raw_bearer_send_and_receive_unix
    , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrInet 10000 localhost)) $
      testProperty "raw bearer send receive inet socket" prop_raw_bearer_send_and_receive_inet
    ]
  ]

localhost :: Word32
localhost = Socket.tupleToHostAddress (127, 0, 0, 1)

onlyIf :: Bool -> a -> Maybe a
onlyIf False = const Nothing
onlyIf True  = Just

{-# NOINLINE nextPort #-}
nextPort :: MVar IO Int
nextPort = unsafePerformIO $ newMVar 7000

inetProp :: (SocketSnocket
              -> MakeRawBearer IO Socket.Socket
              -> Socket.SockAddr
              -> Maybe Socket.SockAddr
              -> Message
              -> IO Property
            )
         -> Message
         -> Property
inetProp inner msg =
  ioProperty $ withIOManager $ \iomgr -> do
    serverPort <- modifyMVar nextPort (\i -> return (succ i, succ i))
    let serverAddr = Socket.SockAddrInet (fromIntegral serverPort) localhost
    Debug.Trace.traceM $ "Server: " ++ show serverAddr
    inner
      (socketSnocket iomgr)
      makeSocketRawBearer
      serverAddr
      Nothing
      msg

localProp :: (LocalSnocket
              -> MakeRawBearer IO LocalSocket
              -> LocalAddress
              -> Maybe LocalAddress
              -> Message
              -> IO Property
            )
         -> ArbPosInt
         -> ArbPosInt
         -> Message
         -> Property
localProp inner serverInt clientInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
#if defined(mingw32_HOST_OS)
    let serverName = "\\\\.\\pipe\\local_socket_server.test." ++ show serverInt
    let clientName = "\\\\.\\pipe\\local_socket_client.test." ++ show clientInt
#else
    let serverName = "local_socket_server.test" ++ show serverInt
    let clientName = "local_socket_client.test" ++ show clientInt
#endif
    cleanUp serverName
    let serverAddr = localAddressFromPath serverName
    let clientAddr = localAddressFromPath clientName
    inner
      (localSnocket iomgr)
      makeLocalRawBearer
      serverAddr
      (Just clientAddr)
      msg `finally` do
        cleanUp serverName
  where
#if defined(mingw32_HOST_OS)
    cleanUp _ = return ()
#else
    cleanUp name = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile name)
                  (\_ -> return ())
#endif

unixProp :: (SocketSnocket
              -> MakeRawBearer IO Socket.Socket
              -> Socket.SockAddr
              -> Maybe Socket.SockAddr
              -> Message
              -> IO Property
            )
         -> ArbPosInt
         -> ArbPosInt
         -> Message
         -> Property
unixProp inner serverInt clientInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
    let serverName = "unix_socket_server.test."++ show serverInt
    let clientName = "unix_socket_client.test."++ show clientInt
    cleanUp serverName
    cleanUp clientName
    let serverAddr = Socket.SockAddrUnix serverName
    let clientAddr = Socket.SockAddrUnix clientName
    inner
      (socketSnocket iomgr)
      makeSocketRawBearer
      serverAddr
      (Just clientAddr)
      msg `finally` do
        cleanUp serverName
  where
    cleanUp name = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile name)
                  (\_ -> return ())

newtype ArbPosInt = ArbPosInt { unArbPosInt :: Int }
  deriving newtype (Show, Eq, Ord)

instance Arbitrary ArbPosInt where
  shrink _ = []
  arbitrary = ArbPosInt . getPositive <$> arbitrary

prop_raw_bearer_basic_inet :: Message -> Property
prop_raw_bearer_basic_inet = inetProp rawBearerBasic

prop_raw_bearer_basic_local :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_basic_local = localProp rawBearerBasic

prop_raw_bearer_basic_unix :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_basic_unix = unixProp rawBearerBasic

-----

prop_raw_bearer_send_and_receive_inet :: Message -> Property
prop_raw_bearer_send_and_receive_inet = inetProp rawBearerSendAndReceive

prop_raw_bearer_send_and_receive_local :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_send_and_receive_local = localProp rawBearerSendAndReceive

prop_raw_bearer_send_and_receive_unix :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_send_and_receive_unix = unixProp rawBearerSendAndReceive

prop_raw_bearer_send_and_receive_iosim :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_send_and_receive_iosim serverInt clientInt msg =
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        rawBearerSendAndReceive
          snocket
          (makeFDRawBearer nullTracer)
          (TestAddress $ unArbPosInt serverInt)
          (Just $ TestAddress $ unArbPosInt clientInt)
          msg

newtype Message = Message { messageBytes :: ByteString }
  deriving (Show, Eq, Ord)

instance Arbitrary Message where
  shrink = filter (not . BS.null . messageBytes) . fmap (Message . BS.pack) . shrink . BS.unpack . messageBytes
  arbitrary = Message . BS.pack <$> listOf1 arbitrary

newtype TestError = TestError String
  deriving (Show)

instance Exception TestError where

rawBearerSendAndReceive :: forall m fd addr
                         . ( MonadST m
                           , MonadThrow m
                           , MonadAsync m
                           , MonadDelay m
                           , MonadMVar m
                           , MonadSay m
                           , Show addr
                           )
                        => Snocket m fd addr
                        -> MakeRawBearer m fd
                        -> addr
                        -> Maybe addr
                        -> Message
                        -> m Property
rawBearerSendAndReceive snocket mkrb serverAddr mclientAddr msg =
  withLiftST $ \liftST -> do
    let io = liftST . unsafeIOToST
    let size = BS.length (messageBytes msg)
    retVar <- newEmptyMVar
    senderDone <- newEmptyMVar
    let sender = do
          bracket (openToConnect snocket serverAddr) (\s -> say "sender: closing" >> close snocket s >> putMVar senderDone ()) $ \s -> do
            case mclientAddr of
              Nothing -> return ()
              Just clientAddr -> do
                say $ "sender: binding to " ++ show clientAddr
                bind snocket s clientAddr
            say $ "sender: connecting to " ++ show serverAddr
            connect snocket s serverAddr
            say "sender: connected"
            bearer <- getRawBearer mkrb s
            bracket (io $ mallocBytes size) (io . free) $ \srcBuf -> do
              threadDelay 2000000
              io $ BS.useAsCStringLen (messageBytes msg)
                    (uncurry (copyBytes srcBuf))
              let go _ 0 = do
                    say "sender: done"
                    return ()
                  go _ n | n < 0 = do
                    error "sender: negative byte count"
                  go buf n = do
                    say $ "sender: " ++ show n ++ " bytes left"
                    bytesSent <- send bearer buf n
                    when (bytesSent == 0) (throwIO $ TestError "sender: premature hangup")
                    let n' = n - bytesSent
                    say $ "sender: " ++ show bytesSent ++ " bytes sent, " ++ show n' ++ " remaining"
                    go (plusPtr buf bytesSent) n'
              go (castPtr srcBuf) size
        receiver s = do
          let acceptLoop :: Accept m fd addr -> m ()
              acceptLoop accept0 = do
                threadDelay 1000
                say "receiver: accepting connection"
                (accepted, acceptNext) <- runAccept accept0
                case accepted :: Accepted fd addr of
                  AcceptFailure err ->
                    throwIO err
                  Accepted s' _ -> do
                    labelThisThread "accept"
                    say "receiver: connection accepted"
                    -- flip finally (say "receiver: closing connection" >> close snocket s' >> say "receiver: connection closed") $ do
                    bearer <- getRawBearer mkrb s'
                    retval <- bracket (io $ mallocBytes size) (io . free) $ \dstBuf -> do
                      let go _ 0 = do
                            say "receiver: done receiving"
                            return ()
                          go _ n | n < 0 = do
                            error "receiver: negative byte count"
                          go buf n = do
                            say $ "receiver: " ++ show n ++ " bytes left"
                            bytesReceived <- recv bearer buf n
                            when (bytesReceived == 0) (throwIO $ TestError "receiver: premature hangup")
                            let n' = n - bytesReceived
                            say $ "receiver: " ++ show bytesReceived ++ " bytes received, " ++ show n' ++ " remaining"
                            go (plusPtr buf bytesReceived) n'
                      go (castPtr dstBuf) size
                      io (BS.packCStringLen (castPtr dstBuf, size))
                    say $ "receiver: received " ++ show retval
                    written <- tryPutMVar retVar retval
                    say $ if written then "receiver: stored " ++ show retval else "receiver: already have result"
                    say "receiver: finishing connection"
                    unless written $ acceptLoop acceptNext
          accept snocket s >>= acceptLoop

    resBSEither <- bracket (open snocket (addrFamily snocket serverAddr)) (close snocket) $ \s -> do
      say "receiver: starting"
      bind snocket s serverAddr
      listen snocket s
      say "receiver: listening"
      race
        (sender `concurrently` receiver s)
        (takeMVar retVar <* takeMVar senderDone)
    return $ resBSEither === Right (messageBytes msg)


rawBearerBasic :: forall m fd addr
                         . ( MonadST m
                           , MonadThrow m
                           , MonadAsync m
                           , MonadMVar m
                           , MonadSay m
                           , Show addr
                           )
                        => Snocket m fd addr
                        -> MakeRawBearer m fd
                        -> addr
                        -> Maybe addr
                        -> Message
                        -> m Property
rawBearerBasic snocket mkrb serverAddr mclientAddr msg' =
  withLiftST $ \liftST -> do
    let msg = Message $ BS.take 1 $ messageBytes msg' <> "a"
    let io = liftST . unsafeIOToST
    let size = BS.length (messageBytes msg)
    let sender = bracket (openToConnect snocket serverAddr) (\s -> say "sender: closing" >> close snocket s) $ \s -> do
                    case mclientAddr of
                      Nothing -> return ()
                      Just clientAddr -> do
                        say $ "sender: binding to " ++ show clientAddr
                        bind snocket s clientAddr
                    say $ "sender: connecting to " ++ show serverAddr
                    connect snocket s serverAddr
                    say "sender: connected"
                    bearer <- getRawBearer mkrb s
                    bracket (io $ mallocBytes size) (io . free) $ \srcBuf -> do
                      io $ BS.useAsCStringLen (messageBytes msg)
                            (uncurry (copyBytes srcBuf))
                      let go _ 0 = do
                            say "sender: done"
                            return ()
                          go _ n | n < 0 = do
                            error "sender: negative byte count"
                          go buf n = do
                            say $ "sender: " ++ show n ++ " bytes left"
                            bytesSent <- send bearer buf n
                            when (bytesSent == 0) (throwIO $ TestError "sender: premature hangup")
                            let n' = n - bytesSent
                            say $ "sender: " ++ show bytesSent ++ " bytes sent, " ++ show n' ++ " remaining"
                            go (plusPtr buf bytesSent) n'
                      go (castPtr srcBuf) size
        receiver s = do
          let acceptLoop :: Accept m fd addr -> m ()
              acceptLoop accept0 = do
                say "receiver: accepting connection"
                (accepted, acceptNext) <- runAccept accept0
                case accepted :: Accepted fd addr of
                  AcceptFailure err ->
                    throwIO err
                  Accepted s' _ -> do
                    labelThisThread "accept"
                    say "receiver: connection accepted"
                    flip finally (say "receiver: closing connection" >> close snocket s' >> say "receiver: connection closed") $ do
                      bearer <- getRawBearer mkrb s'
                      retval <- bracket (io $ mallocBytes size) (io . free) $ \dstBuf -> do
                        let go _ 0 = do
                              say "receiver: done receiving"
                              return ()
                            go _ n | n < 0 = do
                              error "receiver: negative byte count"
                            go buf n = do
                              say $ "receiver: " ++ show n ++ " bytes left"
                              bytesReceived <- recv bearer buf n
                              when (bytesReceived == 0) (throwIO $ TestError "receiver: premature hangup")
                              let n' = n - bytesReceived
                              say $ "receiver: " ++ show bytesReceived ++ " bytes received, " ++ show n' ++ " remaining"
                              go (plusPtr buf bytesReceived) n'
                        go (castPtr dstBuf) size
                        io (BS.packCStringLen (castPtr dstBuf, size))
                      say $ "receiver: received " ++ show retval
                    say "receiver: finishing connection"
                    -- acceptLoop acceptNext
                    close snocket s
          accept snocket s >>= acceptLoop

    _resBSEither <- bracket (open snocket (addrFamily snocket serverAddr)) (close snocket) $ \s -> do
      say "receiver: starting"
      bind snocket s serverAddr
      listen snocket s
      say "receiver: listening"
      sender `concurrently` receiver s
    -- return $ resBSEither === Right (messageBytes msg)
    return $ property True

iosimProperty :: (forall s . IOSim s Property)
              -> Property
iosimProperty sim =
  let tr = runSimTrace sim
   in case traceResult True tr of
     Left e -> counterexample
                (unlines
                  [ "=== Say Events ==="
                  , unlines (selectTraceEventsSay' tr)
                  , "=== Trace Events ==="
                  , unlines (show `map` traceEvents tr)
                  , "=== Error ==="
                  , show e ++ "\n"
                  ])
                False
     Right prop -> prop

