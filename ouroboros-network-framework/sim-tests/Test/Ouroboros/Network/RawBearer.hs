{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Ouroboros.Network.RawBearer where

import Ouroboros.Network.IOManager
import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket
import Ouroboros.Network.Testing.Data.AbsBearerInfo

import Control.Concurrent.Class.MonadMVar
import Control.Exception (Exception)
import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSay
import Control.Monad.Class.MonadST (MonadST, stToIO)
import Control.Monad.Class.MonadThrow (MonadThrow, bracket, catchJust, finally,
           throwIO)
import Control.Monad.IOSim hiding (liftST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Tracer (Tracer (..), nullTracer, traceWith)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word32)
import Foreign.Marshal (copyBytes, free, mallocBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Network.Socket qualified as Socket
import Simulation.Network.Snocket as SimSnocket
import System.Directory (removeFile)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import System.IO.Unsafe

import Test.Simulation.Network.Snocket (toBearerInfo)
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer"
  [ testProperty "raw bearer send receive simulated socket" prop_raw_bearer_send_and_receive_iosim
#if !defined(mingw32_HOST_OS)
  , testProperty "raw bearer send receive local socket" prop_raw_bearer_send_and_receive_local
  , testProperty "raw bearer send receive unix socket" prop_raw_bearer_send_and_receive_unix
#endif
  , testProperty "raw bearer send receive inet socket" prop_raw_bearer_send_and_receive_inet
  ]

iosimTracer :: forall s. Tracer (IOSim s) String
iosimTracer = Tracer say

ioTracer :: Tracer IO String
ioTracer = nullTracer

onlyIf :: Bool -> a -> Maybe a
onlyIf False = const Nothing
onlyIf True  = Just

{-# NOINLINE nextPort #-}
nextPort :: MVar IO Int
nextPort = unsafePerformIO $ newMVar 7000

prop_raw_bearer_send_and_receive_inet :: Message -> Property
prop_raw_bearer_send_and_receive_inet msg =
  ioProperty $ withIOManager $ \iomgr -> do
    serverPort <- modifyMVar nextPort (\i -> return (succ i, succ i))
    let serverAddr = Socket.SockAddrInet (fromIntegral serverPort) localhost
    rawBearerSendAndReceive
      ioTracer
      (socketSnocket iomgr)
      makeSocketRawBearer
      serverAddr
      Nothing
      msg

newtype ArbPosInt = ArbPosInt { unArbPosInt :: Int }
  deriving newtype (Show, Eq, Ord)

instance Arbitrary ArbPosInt where
  shrink _ = []
  arbitrary = ArbPosInt . getPositive <$> arbitrary

prop_raw_bearer_send_and_receive_local :: ArbPosInt -> ArbPosInt -> Message -> Property
prop_raw_bearer_send_and_receive_local serverInt clientInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
#if defined(mingw32_HOST_OS)
    let serverName = "\\\\.\\pipe\\local_socket_server.test" ++ show serverInt
    let clientName = "\\\\.\\pipe\\local_socket_client.test" ++ show clientInt
#else
    let serverName = "local_socket_server.test" ++ show serverInt
    let clientName = "local_socket_client.test" ++ show clientInt
#endif
    cleanUp serverName
    cleanUp clientName
    let serverAddr = localAddressFromPath serverName
    let clientAddr = localAddressFromPath clientName
    rawBearerSendAndReceive
      ioTracer
      (localSnocket iomgr)
      makeLocalRawBearer
      serverAddr
      (Just clientAddr)
      msg `finally` do
        cleanUp serverName
        cleanUp clientName
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

prop_raw_bearer_send_and_receive_unix :: Int -> Int -> Message -> Property
prop_raw_bearer_send_and_receive_unix serverInt clientInt msg =
  ioProperty $ withIOManager $ \iomgr -> do
    let serverName = "unix_socket_server.test"++ show serverInt
    let clientName = "unix_socket_client.test"++ show clientInt
    cleanUp serverName
    cleanUp clientName
    let serverAddr = Socket.SockAddrUnix serverName
    let clientAddr = Socket.SockAddrUnix clientName
    rawBearerSendAndReceive
      ioTracer
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

prop_raw_bearer_send_and_receive_iosim :: Int -> Int -> Message -> Property
prop_raw_bearer_send_and_receive_iosim serverInt clientInt msg =
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        rawBearerSendAndReceive
          iosimTracer
          snocket
          (makeFDRawBearer nullTracer)
          (TestAddress serverInt)
          (Just $ TestAddress clientInt)
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
                           , Show addr
                           )
                        => Tracer m String
                        -> Snocket m fd addr
                        -> MakeRawBearer m fd
                        -> addr
                        -> Maybe addr
                        -> Message
                        -> m Property
rawBearerSendAndReceive tracer snocket mkrb serverAddr mclientAddr msg = do
    let io = stToIO . unsafeIOToST
    let size = BS.length (messageBytes msg)
    retVar <- newEmptyMVar
    senderDone <- newEmptyMVar
    let sender = bracket (openToConnect snocket serverAddr) (\s -> traceWith tracer "sender: closing" >> close snocket s) $ \s -> do
                    case mclientAddr of
                      Nothing -> return ()
                      Just clientAddr -> do
                        traceWith tracer $ "sender: binding to " ++ show clientAddr
                        bind snocket s clientAddr
                    traceWith tracer $ "sender: connecting to " ++ show serverAddr
                    connect snocket s serverAddr
                    traceWith tracer "sender: connected"
                    bearer <- getRawBearer mkrb s
                    bracket (io $ mallocBytes size) (io . free) $ \srcBuf -> do
                      io $ BS.useAsCStringLen (messageBytes msg)
                            (uncurry (copyBytes srcBuf))
                      let go _ 0 = do
                            traceWith tracer "sender: done"
                            return ()
                          go _ n | n < 0 = do
                            error "sender: negative byte count"
                          go buf n = do
                            traceWith tracer $ "sender: " ++ show n ++ " bytes left"
                            bytesSent <- send bearer buf n
                            when (bytesSent == 0) (throwIO $ TestError "sender: premature hangup")
                            let n' = n - bytesSent
                            traceWith tracer $ "sender: " ++ show bytesSent ++ " bytes sent, " ++ show n' ++ " remaining"
                            go (plusPtr buf bytesSent) n'
                      go (castPtr srcBuf) size
                      putMVar senderDone ()
        receiver s = do
          let acceptLoop :: Accept m fd addr -> m ()
              acceptLoop accept0 = do
                traceWith tracer "receiver: accepting connection"
                (accepted, acceptNext) <- runAccept accept0
                case accepted :: Accepted fd addr of
                  AcceptFailure err ->
                    throwIO err
                  Accepted s' _ -> do
                    labelThisThread "accept"
                    traceWith tracer "receiver: connection accepted"
                    flip finally (traceWith tracer "receiver: closing connection" >> close snocket s' >> traceWith tracer "receiver: connection closed") $ do
                      bearer <- getRawBearer mkrb s'
                      retval <- bracket (io $ mallocBytes size) (io . free) $ \dstBuf -> do
                        let go _ 0 = do
                              traceWith tracer "receiver: done receiving"
                              return ()
                            go _ n | n < 0 = do
                              error "receiver: negative byte count"
                            go buf n = do
                              traceWith tracer $ "receiver: " ++ show n ++ " bytes left"
                              bytesReceived <- recv bearer buf n
                              when (bytesReceived == 0) (throwIO $ TestError "receiver: premature hangup")
                              let n' = n - bytesReceived
                              traceWith tracer $ "receiver: " ++ show bytesReceived ++ " bytes received, " ++ show n' ++ " remaining"
                              go (plusPtr buf bytesReceived) n'
                        go (castPtr dstBuf) size
                        io (BS.packCStringLen (castPtr dstBuf, size))
                      traceWith tracer $ "receiver: received " ++ show retval
                      written <- tryPutMVar retVar retval
                      traceWith tracer $ if written then "receiver: stored " ++ show retval else "receiver: already have result"
                    traceWith tracer "receiver: finishing connection"
                    acceptLoop acceptNext
          accept snocket s >>= acceptLoop

    resBSEither <- bracket (open snocket (addrFamily snocket serverAddr)) (close snocket) $ \s -> do
      traceWith tracer "receiver: starting"
      bind snocket s serverAddr
      listen snocket s
      traceWith tracer "receiver: listening"
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

