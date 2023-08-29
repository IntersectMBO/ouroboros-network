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
  = ServiceDriverSendingVersionID VersionIdentifier
  | ServiceDriverReceivingVersionID
  | ServiceDriverReceivedVersionID VersionIdentifier
  | ServiceDriverInvalidVersion
  | ServiceDriverSendingKey Word64
  | ServiceDriverSentKey Word64
  | ServiceDriverReceivingKey
  | ServiceDriverReceivedKey Word64
  | ServiceDriverConfirmingKey
  | ServiceDriverConfirmedKey
  | ServiceDriverDecliningKey RecvResult
  | ServiceDriverDeclinedKey
  | ServiceDriverConnectionClosed
  | ServiceDriverCRefEvent CRefEvent
  | ServiceDriverMisc String
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
        sendBundle s skpRef oc
        traceWith tracer $ ServiceDriverSentKey (ocertN oc)
      (ServerAgency TokIdle, EndMessage) -> do
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
          Right _ ->  
            return (SomeMessage VersionMessage, ())
          Left (expectedV, v) -> do
            traceWith tracer $ ServiceDriverInvalidVersion
            return (SomeMessage AbortMessage, ())
      (ServerAgency TokIdle) -> do
        traceWith tracer ServiceDriverReceivingKey
        result <- receiveBundle s (ServiceDriverMisc >$< tracer)
        case result of
          Just (skpVar, oc) -> do
            traceWith tracer $ ServiceDriverReceivedKey (ocertN oc)
            return (SomeMessage (KeyMessage skpVar oc), ())
          Nothing -> do
            traceWith tracer ServiceDriverConnectionClosed
            return (SomeMessage EndMessage, ())
      (ClientAgency TokWaitForConfirmation) -> do
        result <- receiveRecvResult s
        if result == RecvOK then
          traceWith tracer ServiceDriverConfirmedKey
        else
          traceWith tracer ServiceDriverDeclinedKey
        return (SomeMessage (RecvResultMessage result), ())

  , startDState = ()
  }
