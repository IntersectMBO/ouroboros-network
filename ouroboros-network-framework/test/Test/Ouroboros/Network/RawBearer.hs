{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Test.Ouroboros.Network.RawBearer
where

import Ouroboros.Network.RawBearer
import Ouroboros.Network.Snocket
import Ouroboros.Network.Testing.Data.AbsBearerInfo
import Ouroboros.Network.IOManager

import Control.Monad (when)
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadST (MonadST, withLiftST)
import Control.Monad.Class.MonadThrow (MonadThrow, bracket, throwIO, catchJust, finally)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadTimer (MonadTimer (..), threadDelay)
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadSay
import Control.Monad.IOSim hiding (liftST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Foreign.Marshal (copyBytes, mallocBytes, free)
import Foreign.Ptr (castPtr, plusPtr)
import qualified Network.Socket as Socket
import Simulation.Network.Snocket as SimSnocket
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistErrorType, ioeGetErrorType)
import Control.Tracer (nullTracer)
import Control.Exception (Exception)

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Simulation.Network.Snocket (toBearerInfo)

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer"
  [ testProperty "raw bearer send receive simulated socket" prop_raw_bearer_send_and_receive_iosim
  , testProperty "raw bearer send receive unix socket" prop_raw_bearer_send_and_receive_unix
  ]

prop_raw_bearer_send_and_receive_unix :: Message -> Property
prop_raw_bearer_send_and_receive_unix msg =
  ioProperty $ withIOManager $ \iomgr -> do
    let clientName = "unix_socket_client.test"
    let serverName = "unix_socket_server.test"
    cleanUp clientName
    cleanUp serverName
    let clientAddr = Socket.SockAddrUnix clientName
    let serverAddr = Socket.SockAddrUnix serverName
    rawBearerSendAndReceive
      (socketSnocket iomgr)
      clientAddr serverAddr
      msg `finally` do
        cleanUp clientName
        cleanUp serverName
  where
    cleanUp name = do
        catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing)
                  (removeFile name)
                  (\_ -> return ())

prop_raw_bearer_send_and_receive_iosim :: Int -> Int -> Message -> Property
prop_raw_bearer_send_and_receive_iosim clientInt serverInt msg =
  (clientInt /= serverInt) ==>
  iosimProperty $
    SimSnocket.withSnocket
      nullTracer
      (toBearerInfo absNoAttenuation)
      mempty $ \snocket _observe -> do
        rawBearerSendAndReceive
          snocket
          (TestAddress clientInt)
          (TestAddress serverInt)
          msg

newtype Message = Message { messageBytes :: ByteString }
  deriving (Show, Eq, Ord)

instance Arbitrary Message where
  shrink = filter (not . BS.null . messageBytes) . fmap (Message . BS.pack) . shrink . BS.unpack . messageBytes
  arbitrary = Message . BS.pack <$> listOf1 arbitrary

newtype TestError =
  TestError String
  deriving (Show)

instance Exception TestError where

rawBearerSendAndReceive :: forall m fd addr
                         . ( MonadST m
                           , MonadThrow m
                           , MonadAsync m
                           -- , MonadTimer m
                           , MonadMVar m
                           , MonadSay m
                           , ToRawBearer m fd
                           )
                        => Snocket m fd addr
                        -> addr
                        -> addr
                        -> Message
                        -> m Property
rawBearerSendAndReceive snocket clientAddr serverAddr msg = 
  withLiftST $ \liftST -> do
    let io = liftST . unsafeIOToST
    let size = BS.length (messageBytes msg)
    retVar <- newEmptyMVar
    senderDone <- newEmptyMVar
    let sender = bracket (openToConnect snocket clientAddr) (close snocket) $ \s -> do
                    say "sender: connecting"
                    connect snocket s serverAddr
                    say "sender: connected"
                    bearer <- toRawBearer s
                    bracket (io $ mallocBytes size) (io . free) $ \srcBuf -> do
                      io $ BS.useAsCStringLen (messageBytes msg)
                            (uncurry (copyBytes srcBuf))
                      let go _ 0 = do
                            say "sender: done"
                            return ()
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
                      bearer <- toRawBearer s'
                      retval <- bracket (io $ mallocBytes size) (io . free) $ \dstBuf -> do
                        let go _ 0 = do
                              say "receiver: done receiving"
                              return ()
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

