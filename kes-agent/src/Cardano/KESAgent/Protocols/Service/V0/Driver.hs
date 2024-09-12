{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.KESAgent.Protocols.Service.V0.Driver
  where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.Service.V0.Protocol
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
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

import Control.Concurrent.Class.MonadSTM
import Control.Concurrent.Class.MonadSTM.TChan
import Control.Monad ( void, when, forever, forM_ )
import Control.Monad.Class.MonadAsync
import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket, throwIO, Exception )
import Control.Monad.ST.Unsafe ( unsafeIOToST )
import Control.Monad.Trans ( lift )
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Functor.Contravariant ( (>$<) )
import Data.Proxy
import Data.SerDoc.Class ( ViaEnum (..) )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.Info ( Description (..) )
import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.TH (deriveSerDoc)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Data.Word
import Foreign ( Ptr, castPtr, plusPtr, poke )
import Foreign.C.Types ( CChar, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Text.Printf

serviceDriver :: forall c m f t p
               . Crypto c
              => Typeable c
              => VersionedProtocol (ServiceProtocol m c)
              => KESAlgorithm (KES c)
              => HasInfo (DirectCodec m) (SignKeyKES (KES c))
              => HasInfo (DirectCodec m) (VerKeyKES (KES c))
              => DirectDeserialise (SignKeyKES (KES c))
              => DirectSerialise (SignKeyKES (KES c))
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

      (ServerAgency TokIdle, KeyMessage bundle) -> do
        traceWith tracer $ ServiceDriverSendingKey (ocertN (bundleOC bundle))
        sendItem s bundle
        traceWith tracer $ ServiceDriverSentKey (ocertN (bundleOC bundle))

      (ServerAgency TokIdle, ServerDisconnectMessage) -> do
        return ()

      (ServerAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency TokWaitForConfirmation, RecvResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ServiceDriverConfirmingKey
        else
          traceWith tracer $ ServiceDriverDecliningKey reason
        sendRecvResult s reason

      (ClientAgency TokWaitForConfirmation, ClientDisconnectMessage) -> do
        return ()


  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer ServiceDriverReceivingVersionID
        result <- checkVersion (Proxy @(ServiceProtocol m c)) s (ServiceDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage AbortMessage, ())
      (ServerAgency TokIdle) -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ServiceDriverReceivingKey
          bundle <- receiveItem s
          lift $ traceWith tracer $ ServiceDriverReceivedKey (ocertN (bundleOC bundle))
          return (SomeMessage (KeyMessage bundle), ())
        case result of
          ReadOK msg ->
            return msg
          ReadEOF ->
            return (SomeMessage ServerDisconnectMessage, ())
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
          ReadEOF ->
            return (SomeMessage ClientDisconnectMessage, ())
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

instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) InitialState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ spVersionIdentifier (Proxy @(ServiceProtocol m c))) ++
              ",InitialState,IdleState" ++
              ">")
            (info codec (Proxy @VersionIdentifier))
instance ( NamedCrypto c
         , Crypto c
         , Typeable c
         , HasInfo (DirectCodec m) (SignKeyKES (KES c))
         , HasInfo (DirectCodec m) (VerKeyKES (KES c))
         ) => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) IdleState WaitForConfirmationState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ spVersionIdentifier (Proxy @(ServiceProtocol m c))) ++
              ",IdleState,WaitForConfirmationState" ++
              ">")
            (info codec (Proxy @(Bundle m c)))
instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) WaitForConfirmationState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ spVersionIdentifier (Proxy @(ServiceProtocol m c))) ++
              ",WaitForConfirmationState,IdleState" ++
              ">")
            (info codec (Proxy @RecvResult))
instance NamedCrypto c => HasInfo (DirectCodec m) (Message (ServiceProtocol m c) _st EndState) where
  info codec _ = annField "This message is signalled by terminating the network connection, hence the encoding takes zero bytes." $
            aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ spVersionIdentifier (Proxy @(ServiceProtocol m c))) ++
              ",st,EndState" ++
              ">")
            (info codec (Proxy @()))
