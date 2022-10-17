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

import Foreign (Ptr)
import Foreign.C.Types (CSize)
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import System.Socket
import System.Socket.Unsafe
import Control.Monad (void)
import Data.Proxy
import Text.Printf
import Control.Concurrent.MVar

import Cardano.Crypto.KES.Class
import Cardano.Binary

import Cardano.KESAgent.Protocol
import Cardano.KESAgent.Logging
import Cardano.Crypto.DirectSerialise

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

driver :: forall k f t p
        . DirectDeserialise (SignKeyKES k)
       => DirectSerialise (SignKeyKES k)
       => KESSignAlgorithm IO k
       => VersionedProtocol (KESProtocol k)
       => Socket f t p -- | A socket to read from
       -> Tracer IO DriverTrace
       -> Driver (KESProtocol k) () IO
driver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(KESProtocol k))
        traceWith tracer (DriverSendingVersionID $ VersionIdentifier v)
        void $ send s v msgNoSignal
      (ServerAgency TokIdle, KeyMessage sk) -> do
        traceWith tracer DriverSendingKey
        directSerialise (\buf bufSize -> void $ unsafeSend s buf bufSize (msgNoSignal <> msgWaitAll)) sk
        traceWith tracer DriverSentKey
      (ServerAgency TokIdle, EndMessage) -> do
        return ()

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer DriverReceivingVersionID
        v <- VersionIdentifier <$> receive s versionIdentifierLength msgNoSignal
        traceWith tracer $ DriverReceivedVersionID v
        let expectedV = versionIdentifier (Proxy @(KESProtocol k))
        if v == expectedV then
          return (SomeMessage VersionMessage, ())
        else
          fail $ "DRIVER: Invalid version, expected " ++ show expectedV ++ ", but got " ++ show v
      (ServerAgency TokIdle) -> do
        traceWith tracer DriverReceivingKey
        noReadVar <- newEmptyMVar
        sk <- directDeserialise
                (\buf bufSize -> unsafeReceive s buf bufSize (msgNoSignal <> msgWaitAll) >>= \case
                    0 -> putMVar noReadVar ()
                    _ -> return ()
                )
        succeeded <- isEmptyMVar noReadVar
        if succeeded then do
          traceWith tracer DriverReceivedKey
          return (SomeMessage (KeyMessage sk), ())
        else do
          -- We're not actually returning the key, because it hasn't been
          -- loaded properly, however it has been allocated nonetheless,
          -- so we must forget it, otherwise we will leak mlocked memory.
          forgetSignKeyKES sk
          traceWith tracer DriverConnectionClosed
          return (SomeMessage EndMessage, ())

  , startDState = ()
  }
