{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Ouroboros.Network.RawBearer where

import Ouroboros.Network.IOManager
import Ouroboros.Network.RawBearer
import Ouroboros.Network.RawBearer.Test.Utils
import Ouroboros.Network.Snocket

import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadThrow (catchJust, finally)
import Control.Tracer (Tracer (..), nullTracer)
import Data.Word (Word32)
import Network.Socket qualified as Socket
import System.Directory (removeFile)
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import System.IO.Unsafe

import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Ouroboros.Network.RawBearer"
  [ testProperty "raw bearer send receive inet socket" prop_raw_bearer_send_and_receive_inet
#if !defined(mingw32_HOST_OS)
  , testProperty "raw bearer send receive local socket" prop_raw_bearer_send_and_receive_local
  , testProperty "raw bearer send receive unix socket" prop_raw_bearer_send_and_receive_unix
#endif
  ]

ioTracer :: Tracer IO String
ioTracer = nullTracer

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
