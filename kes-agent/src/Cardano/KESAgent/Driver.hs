{-#OPTIONS_GHC -Wno-deprecations #-}
{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.KESAgent.Driver
where

import Foreign (Ptr, plusPtr)
import Foreign.C.Types (CSize)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import System.Socket
import System.Socket.Unsafe
import Control.Monad (void, when)
import Data.Proxy
import Text.Printf
import Control.Concurrent.MVar
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode, decode)
import Data.Typeable

import Cardano.Crypto.KES.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Binary

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Logging
import Cardano.KESAgent.OCert

-- | Logging messages that the Driver may send
data DriverTrace
  = DriverSendingVersionID VersionIdentifier
  | DriverReceivingVersionID
  | DriverReceivedVersionID VersionIdentifier
  | DriverSendingKey
  | DriverSentKey
  | DriverReceivingKey
  | DriverReceivedKey
  | DriverConnectionClosed
  deriving (Show)

driver :: forall c f t p
        . Crypto c
       => Typeable c
       => VersionedProtocol (KESProtocol c)
       => KESSignAlgorithm IO (KES c)
       => DirectDeserialise (SignKeyKES (KES c))
       => DirectSerialise (SignKeyKES (KES c))
       => Socket f t p -- | A socket to read from
       -> Tracer IO DriverTrace
       -> Driver (KESProtocol c) () IO
driver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(KESProtocol c))
        traceWith tracer (DriverSendingVersionID $ VersionIdentifier v)
        void $ send s v msgNoSignal
      (ServerAgency TokIdle, KeyMessage (SignKeyWithPeriodKES sk t) oc) -> do
        traceWith tracer DriverSendingKey
        directSerialise (\buf bufSize -> do
              n <- unsafeSend s buf bufSize opt
              when (fromIntegral n /= bufSize) (error "AAAAA")
            ) sk
        let serializedOC = serialize' oc
        sendWord32 s (fromIntegral $ t) opt
        sendWord32 s (fromIntegral $ BS.length serializedOC) opt
        send s serializedOC opt
        traceWith tracer DriverSentKey
      (ServerAgency TokIdle, EndMessage) -> do
        return ()

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer DriverReceivingVersionID
        v <- VersionIdentifier <$> receive s versionIdentifierLength msgNoSignal
        traceWith tracer $ DriverReceivedVersionID v
        let expectedV = versionIdentifier (Proxy @(KESProtocol c))
        if v == expectedV then
          return (SomeMessage VersionMessage, ())
        else
          fail $ "DRIVER: Invalid version, expected " ++ show expectedV ++ ", but got " ++ show v
      (ServerAgency TokIdle) -> do
        traceWith tracer DriverReceivingKey
        noReadVar <- newEmptyMVar
        sk <- directDeserialise
                (\buf bufSize -> unsafeReceiveN s buf bufSize opt >>= \case
                    0 -> putMVar noReadVar ()
                    n -> when (fromIntegral n /= bufSize) (error "BBBBBB")
                )
        t <- fromIntegral <$> (
              maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s opt
             )
        l <- fromIntegral <$> (
              maybe (putMVar noReadVar () >> return 0) return =<< receiveWord32 s opt
             )
        oc <- unsafeDeserialize' <$> receive s l opt

        succeeded <- isEmptyMVar noReadVar
        if succeeded then do
          traceWith tracer DriverReceivedKey
          return (SomeMessage (KeyMessage (SignKeyWithPeriodKES sk t) oc), ())
        else do
          -- We're not actually returning the key, because it hasn't been
          -- loaded properly, however it has been allocated nonetheless,
          -- so we must forget it, otherwise we will leak mlocked memory.
          forgetSignKeyKES sk
          traceWith tracer DriverConnectionClosed
          return (SomeMessage EndMessage, ())

  , startDState = ()
  }
  where
    opt = msgNoSignal <> msgWaitAll

receiveWord32 :: Socket f t p -> MessageFlags -> IO (Maybe Word32)
receiveWord32 s opt =
  receive s 4 opt >>= \case
    "" -> return Nothing
    xs -> (return . Just . fromIntegral) (decode @Word32 $ LBS.fromStrict xs)

sendWord32 :: Socket f t p -> Word32 -> MessageFlags -> IO ()
sendWord32 s val opt =
  void $ send s (LBS.toStrict $ encode val) opt

unsafeReceiveN s buf bufSize opt = do
  n <- unsafeReceive s buf bufSize opt
  if fromIntegral n == bufSize then
    return bufSize
  else if n == 0 then
    return 0
  else do
    n' <- unsafeReceiveN s (plusPtr buf (fromIntegral n)) (bufSize - fromIntegral n) opt
    if n' == 0 then
      return 0
    else
      return bufSize
