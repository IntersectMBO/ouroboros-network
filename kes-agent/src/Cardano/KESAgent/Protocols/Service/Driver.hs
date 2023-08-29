{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.KESAgent.Protocols.Service.Driver
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Service.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium.Memory
  ( allocaBytes
  , copyMem
  , packByteStringCStringLen
  , unpackByteStringCStringLen
  )

import Ouroboros.Network.RawBearer

import Control.Monad ( void, when )
import Control.Monad.Trans ( lift )
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy
import Data.Typeable
import Data.Word
import Foreign ( Ptr, castPtr, plusPtr )
import Foreign.C.Types ( CChar, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Text.Printf

-- | Logging messages that the Driver may send
data ServiceDriverTrace
  = ServiceDriverSendingVersionID !VersionIdentifier
  | ServiceDriverReceivingVersionID
  | ServiceDriverReceivedVersionID !VersionIdentifier
  | ServiceDriverInvalidVersion !VersionIdentifier !VersionIdentifier
  | ServiceDriverSendingKey !Word64
  | ServiceDriverSentKey !Word64
  | ServiceDriverReceivingKey
  | ServiceDriverReceivedKey !Word64
  | ServiceDriverConfirmingKey
  | ServiceDriverConfirmedKey
  | ServiceDriverDecliningKey !RecvResult
  | ServiceDriverDeclinedKey
  | ServiceDriverConnectionClosed
  | ServiceDriverCRefEvent !CRefEvent
  | ServiceDriverProtocolError !String
  | ServiceDriverMisc String
  | ServiceDriverPing
  | ServiceDriverPong
  deriving (Show)

instance Pretty ServiceDriverTrace where
  pretty (ServiceDriverMisc x) = x
  pretty x = drop (strLength "ServiceDriver") (show x)

serviceDriver :: forall c m f t p
               . Crypto c
              => Typeable c
              => VersionedProtocol (ServiceProtocol m c)
              => KESAlgorithm (KES c)
              => DirectDeserialise m (SignKeyKES (KES c))
              => DirectSerialise m (SignKeyKES (KES c))
              => MonadThrow m
              => MonadSTM m
              => MonadMVar m
              => MonadFail m
              => MonadST m
              => RawBearer m
              -> Tracer m ServiceDriverTrace
              -> Driver (ServiceProtocol m c) () m
serviceDriver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ServiceProtocol m c))
        sendVersion (Proxy @(ServiceProtocol m c)) s (ServiceDriverSendingVersionID >$< tracer)
      (ServerAgency TokInitial, AbortMessage) -> do
        return ()

      (ServerAgency TokIdle, KeyMessage skpRef oc) -> do
        traceWith tracer $ ServiceDriverSendingKey (ocertN oc)
        sendWord32 s 1
        sendBundle s skpRef oc
        traceWith tracer $ ServiceDriverSentKey (ocertN oc)

      (ServerAgency TokIdle, NoKeyYetMessage) -> do
        -- traceWith tracer ServiceDriverPing
        sendWord32 s 0

      (ServerAgency TokIdle, PingMessage) -> do
        -- traceWith tracer ServiceDriverPing
        sendWord32 s 0

      (ServerAgency TokIdle, EndMessage) -> do
        return ()

      (ServerAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency TokWaitForPong, PongMessage) -> do
        -- traceWith tracer ServiceDriverPong
        sendWord32 s 0
        return ()

      (ClientAgency TokWaitForConfirmation, RecvResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ServiceDriverConfirmingKey
        else
          traceWith tracer $ ServiceDriverDecliningKey reason
        sendRecvResult s reason

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer ServiceDriverReceivingVersionID
        result <- receiveVersion (Proxy @(ServiceProtocol m c)) s (ServiceDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->  
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage AbortMessage, ())
      (ServerAgency TokIdle) -> do
        result <- runReadResultT $ do
          i <- ReadResultT (receiveWord32 s)
          case i of
            0 -> do
              -- lift $ traceWith tracer ServiceDriverPing
              return (SomeMessage PingMessage, ())
            1 -> do
              lift $ traceWith tracer ServiceDriverReceivingKey
              (skpVar, oc) <- ReadResultT $ receiveBundle s (ServiceDriverMisc >$< tracer)
              lift $ traceWith tracer $ ServiceDriverReceivedKey (ocertN oc)
              return (SomeMessage (KeyMessage skpVar oc), ())
            n -> readResultT $ ReadMalformed "ping"
        case result of
          ReadOK msg ->
            return msg
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage EndMessage, ())

      (ClientAgency TokWaitForPong) -> do
        r <- receiveWord32 s
        case r of
          ReadOK 0 -> do
            -- traceWith tracer ServiceDriverPong
            return (SomeMessage PongMessage, ())
          ReadOK n -> do
            return (SomeMessage ProtocolErrorMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())


      (ClientAgency TokWaitForConfirmation) -> do
        result <- receiveRecvResult s
        case result of
          ReadOK response -> do
            case response of
              RecvOK ->
                traceWith tracer ServiceDriverConfirmedKey
              err ->
                traceWith tracer ServiceDriverDeclinedKey
            return (SomeMessage (RecvResultMessage response), ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())

  , startDState = ()
  }

readErrorToServiceDriverTrace :: ReadResult a -> ServiceDriverTrace
readErrorToServiceDriverTrace (ReadOK _) =
  ServiceDriverMisc "This should not happen"
readErrorToServiceDriverTrace ReadEOF =
  ServiceDriverConnectionClosed
readErrorToServiceDriverTrace (ReadMalformed what) =
  ServiceDriverProtocolError what
readErrorToServiceDriverTrace (ReadVersionMismatch expected actual) =
  ServiceDriverInvalidVersion expected actual

