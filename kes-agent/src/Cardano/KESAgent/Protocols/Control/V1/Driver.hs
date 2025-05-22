{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cardano.KESAgent.Protocols.Control.V1.Driver
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Serialization.RawUtil

import Cardano.Crypto.KES.Class

import Ouroboros.Network.RawBearer

import Control.Concurrent.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow (MonadThrow)
import Control.Monad.Trans (lift)
import Control.Tracer (Tracer, traceWith)
import Data.Bits ((.&.))
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Data.Maybe (isJust)
import Data.Proxy
import Data.SerDoc.Class (
  Codec (..),
  HasInfo (..),
  Serializable (..),
  decodeEnum,
  encodeEnum,
  enumInfo,
 )
import Data.SerDoc.Info (aliasField)
import Data.SerDoc.TH (deriveSerDoc)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8)
import Data.Word
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver

flagHasBundle, flagHasStagedKey :: Word8
flagHasBundle = 0x01
flagHasStagedKey = 0x02

has :: rec -> (rec -> Maybe a) -> Bool
has rec getter = isJust $ getter rec

flagWhen :: Bool -> Word8 -> Word8
flagWhen True f = f
flagWhen False _ = 0

whenFlag :: Applicative m => Word8 -> Word8 -> m a -> m (Maybe a)
whenFlag flag flags action =
  if (flags .&. flag) == flag
    then
      Just <$> action
    else
      pure Nothing

instance
  ( Codec codec
  , HasInfo codec Word8
  , HasInfo codec (DefEnumEncoding codec)
  ) =>
  HasInfo codec ConnectionStatus
  where
  info codec _ = enumInfo codec (Proxy @ConnectionStatus) (Proxy @Word8)

instance
  ( Codec codec
  , Serializable codec Word8
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) =>
  Serializable codec ConnectionStatus
  where
  encode codec = encodeEnum codec (Proxy @Word8)
  decode codec = decodeEnum codec (Proxy @Word8)

deriving newtype instance
  (Codec codec, HasInfo codec (VerKeyKES (KES StandardCrypto))) => HasInfo codec KeyInfo

instance
  ( Codec codec
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  , Serializable codec (VerKeyKES (KES StandardCrypto))
  ) =>
  Serializable codec KeyInfo
  where
  encode codec (KeyInfo k) = Data.SerDoc.Class.encode codec k
  decode codec = KeyInfo <$> Data.SerDoc.Class.decode codec

$(deriveSerDoc ''DirectCodec [] ''BootstrapInfo)
$(deriveSerDoc ''DirectCodec [] ''BundleInfo)
$(deriveSerDoc ''DirectCodec [] ''AgentInfo)

controlDriver ::
  forall (m :: Type -> Type) f t p pr.
  VersionedProtocol (ControlProtocol m) =>
  Serializable (DirectCodec m) AgentInfo =>
  MonadThrow m =>
  MonadSTM m =>
  MonadMVar m =>
  MonadFail m =>
  MonadST m =>
  RawBearer m ->
  Tracer m ControlDriverTrace ->
  Driver (ControlProtocol m) pr () m
controlDriver s tracer =
  Driver {sendMessage, recvMessage, initialDState = ()}
  where
    sendMessage ::
      forall (st :: ControlProtocol m) (st' :: ControlProtocol m).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) WeHaveAgency (Relative pr (StateAgency st)) ->
      Message (ControlProtocol m) st st' ->
      m ()
    sendMessage _ msg = case (stateToken @st, msg) of
      (SInitialState, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ControlProtocol m))
        sendVersion (Proxy @(ControlProtocol m)) s (ControlDriverSendingVersionID >$< tracer)
      (_, AbortMessage) ->
        return ()
      (SIdleState, GenStagedKeyMessage) -> do
        sendItem s GenStagedKeyCmd
      (SIdleState, QueryStagedKeyMessage) -> do
        sendItem s QueryStagedKeyCmd
      (SIdleState, DropStagedKeyMessage) -> do
        sendItem s DropStagedKeyCmd
      (SIdleState, RequestInfoMessage) -> do
        sendItem s RequestInfoCmd
      (SIdleState, InstallKeyMessage oc) -> do
        sendItem s InstallKeyCmd
        sendItem s oc
      (SIdleState, EndMessage) -> do
        return ()
      (_, ProtocolErrorMessage) -> do
        return ()
      (SWaitForPublicKeyState, PublicKeyMessage vkeyMay) -> do
        case vkeyMay of
          Nothing -> traceWith tracer ControlDriverNoPublicKeyToReturn
          Just _ -> traceWith tracer ControlDriverReturningPublicKey
        sendItem s vkeyMay
      (SWaitForConfirmationState, InstallResultMessage reason) -> do
        if reason == RecvOK
          then
            traceWith tracer ControlDriverConfirmingKey
          else
            traceWith tracer ControlDriverDecliningKey
        sendItem s reason
      (SWaitForInfoState, InfoMessage info) -> do
        sendItem s info

    recvMessage ::
      forall (st :: ControlProtocol m).
      ( StateTokenI st
      , ActiveState st
      ) =>
      ReflRelativeAgency (StateAgency st) TheyHaveAgency (Relative pr (StateAgency st)) ->
      () ->
      m (SomeMessage st, ())
    recvMessage = \agency () -> case stateToken @st of
      SInitialState -> do
        traceWith tracer ControlDriverReceivingVersionID
        result <- checkVersion (Proxy @(ControlProtocol m)) s (ControlDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage AbortMessage, ())
      SIdleState -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ControlDriverReceivingCommand
          cmd <- receiveItem s
          lift $ traceWith tracer (ControlDriverReceivedCommand cmd)
          case cmd of
            GenStagedKeyCmd ->
              return (SomeMessage GenStagedKeyMessage, ())
            QueryStagedKeyCmd ->
              return (SomeMessage QueryStagedKeyMessage, ())
            DropStagedKeyCmd ->
              return (SomeMessage DropStagedKeyMessage, ())
            DropKeyCmd ->
              return (SomeMessage ProtocolErrorMessage, ())
            InstallKeyCmd -> do
              oc <- receiveItem s
              return (SomeMessage (InstallKeyMessage oc), ())
            RequestInfoCmd -> do
              return (SomeMessage RequestInfoMessage, ())
        case result of
          ReadOK msg ->
            return msg
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage EndMessage, ())
      SWaitForPublicKeyState -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ControlDriverReceivingKey
          vkMay <- receiveItem s
          lift $
            traceWith
              tracer
              ( maybe
                  ControlDriverNoPublicKeyToReturn
                  (ControlDriverReceivedKey . rawSerialiseVerKeyKES)
                  vkMay
              )
          return (SomeMessage (PublicKeyMessage vkMay), ())
        case result of
          ReadOK msg ->
            return msg
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())
      SWaitForConfirmationState -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK reason ->
            return (SomeMessage (InstallResultMessage reason), ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())
      SWaitForInfoState -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK info ->
            return (SomeMessage (InfoMessage info), ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())
      SEndState -> error "This cannot happen"

readErrorToControlDriverTrace :: ReadResult a -> ControlDriverTrace
readErrorToControlDriverTrace (ReadOK _) =
  ControlDriverMisc "This should not happen"
readErrorToControlDriverTrace ReadEOF =
  ControlDriverConnectionClosed
readErrorToControlDriverTrace (ReadMalformed what) =
  ControlDriverProtocolError what
readErrorToControlDriverTrace (ReadVersionMismatch expected actual) =
  ControlDriverInvalidVersion expected actual

instance HasInfo (DirectCodec m) (Message (ControlProtocol m) InitialState IdleState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",InitialState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @VersionIdentifier))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForPublicKeyState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",IdleState,WaitForPublicKeyState"
          ++ ">"
      )
      (info codec (Proxy @Command))

instance
  HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto)) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForPublicKeyState IdleState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",WaitForPublicKeyState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @(Maybe (VerKeyKES (KES StandardCrypto)))))
instance
  HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto)) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForConfirmationState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",IdleState,WaitForConfirmationState"
          ++ ">"
      )
      (info codec (Proxy @(OCert StandardCrypto)))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForConfirmationState IdleState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",WaitForConfirmationState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @RecvResult))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForInfoState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",IdleState,WaitForInfoState"
          ++ ">"
      )
      (info codec (Proxy @()))
instance
  HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto)) =>
  HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForInfoState IdleState)
  where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",WaitForInfoState,IdleState"
          ++ ">"
      )
      (info codec (Proxy @AgentInfo))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) _st EndState) where
  info codec _ =
    aliasField
      ( "Message<"
          ++ (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier)
          ++ ",st,EndState"
          ++ ">"
      )
      (info codec (Proxy @()))
