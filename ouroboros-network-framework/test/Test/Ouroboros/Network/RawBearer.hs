{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Ouroboros.Network.RawBearer where

import           Ouroboros.Network.IOManager
import           Ouroboros.Network.RawBearer
import           Ouroboros.Network.Snocket
import           Ouroboros.Network.Testing.Data.AbsBearerInfo

import           Control.Concurrent.Class.MonadMVar
import           Control.Exception (Exception)
import           Control.Monad (when)
import           Control.Monad.Class.MonadAsync
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

import           Test.Simulation.Network.Snocket (toBearerInfo)
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer" $
  catMaybes
  [ Just $ testProperty "raw bearer send receive simulated socket" prop_raw_bearer_send_and_receive_iosim
  , Just $ testProperty "raw bearer send receive local socket" prop_raw_bearer_send_and_receive_local
  , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrUnix "dummy")) $
    testProperty "raw bearer send receive unix socket" prop_raw_bearer_send_and_receive_unix
  , onlyIf (Socket.isSupportedSockAddr (Socket.SockAddrInet 10000 localhost)) $
    testProperty "raw bearer send receive inet socket" prop_raw_bearer_send_and_receive_inet
  ]

onlyIf :: Bool -> a -> Maybe a
onlyIf False = const Nothing
onlyIf True  = Just

prop_raw_bearer_send_and_receive_inet :: Message -> Property
prop_raw_bearer_send_and_receive_inet msg =
  ioProperty $ withIOManager $ \iomgr -> do
    let serverAddr = Socket.SockAddrInet 10001 localhost
    rawBearerSendAndReceive
      (socketSnocket iomgr)
      makeSocketRawBearer
      serverAddr
      msg

newtype ArbPosInt = ArbPosInt { unArbPosInt :: Int }
  deriving newtype (Show, Eq, Ord)

instance Arbitrary ArbPosInt where
  shrink _ = []
  arbitrary = ArbPosInt . getPositive <$> arbitrary

prop_raw_bearer_send_and_receive_local :: ArbPosInt -> Message -> Property
prop_raw_bearer_send_and_receive_local serverInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
#if defined(mingw32_HOST_OS)
    let serverName = "\\\\.\\pipe\\local_socket_server.test" ++ show serverInt
#else
    let serverName = "local_socket_server.test" ++ show serverInt
#endif
    cleanUp serverName
    let serverAddr = localAddressFromPath serverName
    rawBearerSendAndReceive
      (localSnocket iomgr)
      makeLocalRawBearer
      serverAddr
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


localhost :: Word32
localhost = Socket.tupleToHostAddress (127, 0, 0, 1)

prop_raw_bearer_send_and_receive_unix :: Int -> Message -> Property
prop_raw_bearer_send_and_receive_unix serverInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
    let serverName = "unix_socket_server.test"++ show serverInt
    cleanUp serverName
    let serverAddr = Socket.SockAddrUnix serverName
    rawBearerSendAndReceive
      (socketSnocket iomgr)
      makeSocketRawBearer
      serverAddr
      msg `finally` do
        cleanUp serverName
  where
    cleanUp name = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile name)
                  (\_ -> return ())

prop_raw_bearer_send_and_receive_iosim :: Int -> Message -> Property
prop_raw_bearer_send_and_receive_iosim serverInt msg =
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        rawBearerSendAndReceive
          snocket
          (makeFDRawBearer nullTracer)
          (TestAddress serverInt)
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
                           , MonadMVar m
                           , MonadSay m
                           )
                        => Snocket m fd addr
                        -> MakeRawBearer m fd
                        -> addr
                        -> Message
                        -> m Property
rawBearerSendAndReceive snocket mkrb serverAddr msg =
  withLiftST $ \liftST -> do
    let io = liftST . unsafeIOToST
    let size = BS.length (messageBytes msg)
    retVar <- newEmptyMVar
    senderDone <- newEmptyMVar
    let sender = bracket (openToConnect snocket serverAddr) (close snocket) $ \s -> do
                    say "sender: connecting"
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
                      putMVar senderDone ()
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
                      written <- tryPutMVar retVar retval
                      say $ if written then "receiver: stored " ++ show retval else "receiver: already have result"
                    say "receiver: finishing connection"
                    acceptLoop acceptNext
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

