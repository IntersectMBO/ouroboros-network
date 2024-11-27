{-# OPTIONS_GHC -Wno-deprecations #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}

module Cardano.KESAgent.Protocols.Service.V1.Driver
  where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.BearerUtil
import Cardano.KESAgent.Protocols.Service.V1.Protocol
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.DirectCodec
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

import Data.SerDoc.Info ( Description (..), aliasField, annField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)
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
import Data.SerDoc.Info ( Description (..) )
import Data.SerDoc.Class ( ViaEnum (..) )

serviceDriver :: forall m f t p pr
               . VersionedProtocol (ServiceProtocol m)
              => HasInfo (DirectCodec m) (SignKeyKES (KES StandardCrypto))
              => HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
              => DirectDeserialise (SignKeyKES (KES StandardCrypto))
              => DirectSerialise (SignKeyKES (KES StandardCrypto))
              => MonadThrow m
              => MonadSTM m
              => MonadMVar m
              => MonadFail m
              => MonadST m
              => RawBearer m
              -> Tracer m ServiceDriverTrace
              -> Driver (ServiceProtocol m) pr () m
serviceDriver s tracer =
  Driver { sendMessage, recvMessage, initialDState = () }
  where
    sendMessage :: forall (st :: ServiceProtocol m) (st' :: ServiceProtocol m)
                 . ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st) WeHaveAgency (Relative pr (StateAgency st))
                -> Message (ServiceProtocol m) st st'
                -> m ()
    sendMessage = \_ msg -> case (stateToken @st, msg) of
      (SInitialState, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ServiceProtocol m))
        sendVersion (Proxy @(ServiceProtocol m)) s (ServiceDriverSendingVersionID >$< tracer)
      (SInitialState, AbortMessage) -> do
        return ()

      (SIdleState, KeyMessage bundle) -> do
        traceWith tracer $ ServiceDriverSendingKey (ocertN (bundleOC bundle))
        sendItem s bundle
        traceWith tracer $ ServiceDriverSentKey (ocertN (bundleOC bundle))

      (SIdleState, ServerDisconnectMessage) -> do
        return ()

      (_, ProtocolErrorMessage) -> do
        return ()

      (SWaitForConfirmationState, RecvResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ServiceDriverConfirmingKey
        else
          traceWith tracer $ ServiceDriverDecliningKey reason
        sendRecvResult s reason

      (SWaitForConfirmationState, ClientDisconnectMessage) -> do
        return ()


    recvMessage :: forall (st :: ServiceProtocol m)
                 . ( StateTokenI st
                   , ActiveState st
                   )
                => ReflRelativeAgency (StateAgency st) TheyHaveAgency (Relative pr (StateAgency st))
                -> ()
                -> m (SomeMessage st, ())
    recvMessage = \agency () -> case stateToken @st of
      SInitialState -> do
        traceWith tracer ServiceDriverReceivingVersionID
        result <- checkVersion (Proxy @(ServiceProtocol m)) s (ServiceDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToServiceDriverTrace err
            return (SomeMessage AbortMessage, ())
      SIdleState -> do
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

      SWaitForConfirmationState -> do
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

      SEndState -> error "This cannot happen"

readErrorToServiceDriverTrace :: ReadResult a -> ServiceDriverTrace
readErrorToServiceDriverTrace (ReadOK _) =
  ServiceDriverMisc "This should not happen"
readErrorToServiceDriverTrace ReadEOF =
  ServiceDriverConnectionClosed
readErrorToServiceDriverTrace (ReadMalformed what) =
  ServiceDriverProtocolError what
readErrorToServiceDriverTrace (ReadVersionMismatch expected actual) =
  ServiceDriverInvalidVersion expected actual

instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) InitialState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m))) ++
              ",InitialState,IdleState" ++
              ">")
            (info codec (Proxy @VersionIdentifier))
instance ( HasInfo (DirectCodec m) (SignKeyKES (KES StandardCrypto))
         , HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
         ) => HasInfo (DirectCodec m) (Message (ServiceProtocol m) IdleState WaitForConfirmationState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m))) ++
              ",IdleState,WaitForConfirmationState" ++
              ">")
            (info codec (Proxy @(Bundle m StandardCrypto)))
instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) WaitForConfirmationState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m))) ++
              ",WaitForConfirmationState,IdleState" ++
              ">")
            (info codec (Proxy @RecvResult))
instance HasInfo (DirectCodec m) (Message (ServiceProtocol m) _st EndState) where
  info codec _ = annField "This message is signalled by terminating the network connection, hence the encoding takes zero bytes." $
            aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ versionIdentifier (Proxy @(ServiceProtocol m))) ++
              ",st,EndState" ++
              ">")
            (info codec (Proxy @()))
