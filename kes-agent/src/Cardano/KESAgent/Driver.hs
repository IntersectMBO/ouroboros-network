{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
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

import Cardano.Crypto.KES.Class
import Cardano.Binary

import Cardano.KESAgent.Protocol
import Cardano.Crypto.DirectSerialise

driver :: forall k f t p
        . DirectDeserialise (SignKeyKES k)
       => DirectSerialise (SignKeyKES k)
       => VersionedProtocol (KESProtocol k)
       => Socket f t p -- | A socket to read from
       -> Driver (KESProtocol k) () IO
driver s = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokIdle, KeyMessage sk) -> do
        directSerialise (\buf bufSize -> void $ unsafeSend s buf bufSize msgNoSignal) sk
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(KESProtocol k))
        void $ send s v msgNoSignal

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokIdle) -> do
        sk <- directDeserialise (\buf bufSize -> void (unsafeReceive s buf bufSize msgNoSignal))
        return (SomeMessage (KeyMessage sk), ())
      (ServerAgency TokInitial) -> do
        v <- VersionIdentifier <$> receive s 32 msgNoSignal
        let expectedV = versionIdentifier (Proxy @(KESProtocol k))
        if v == expectedV then
          return (SomeMessage VersionMessage, ())
        else
          fail $ "Invalid version, expected " ++ show expectedV ++ ", but got " ++ show v

  , startDState = ()
  }
