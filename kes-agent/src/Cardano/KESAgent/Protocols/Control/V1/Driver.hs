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

module Cardano.KESAgent.Protocols.Control.V1.Driver
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Types
import Cardano.KESAgent.Protocols.Control.V1.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.DirectCodec
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DirectSerialise
import Cardano.Crypto.KES.Class
import Cardano.Crypto.Libsodium.Memory
  ( allocaBytes
  , copyMem
  , packByteStringCStringLen
  , unpackByteStringCStringLen
  )

import Ouroboros.Network.RawBearer

import Control.Monad ( void, when, replicateM )
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Monad.Extra ( whenJust )
import Control.Monad.Trans (lift)
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.Bits ( (.|.), (.&.) )
import Data.ByteString ( ByteString )
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce
import Data.Functor.Contravariant ( (>$<) )
import Data.Maybe ( isJust )
import Data.Proxy
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word
import Foreign ( Ptr, castPtr, plusPtr )
import Foreign.C.Types ( CChar, CSize )
import Foreign.Marshal.Alloc ( free, mallocBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Network.TypedProtocol.Core
import Network.TypedProtocol.Driver
import Text.Printf
import Data.SerDoc.Info ( Description (..), aliasField )
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import qualified Data.SerDoc.Info
import Data.SerDoc.TH (deriveSerDoc)

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
  if (flags .&. flag) == flag then
    Just <$> action
  else
    pure Nothing


instance
  ( Codec codec
  , HasInfo codec Word8
  , HasInfo codec (DefEnumEncoding codec)
  ) => HasInfo codec ConnectionStatus where
    info codec _ = enumInfo codec (Proxy @ConnectionStatus) (Proxy @Word8)

instance
  ( Codec codec
  , Serializable codec Word8
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) => Serializable codec ConnectionStatus where
      encode codec = encodeEnum codec (Proxy @Word8)
      decode codec = decodeEnum codec (Proxy @Word8)

deriving newtype instance (Codec codec, HasInfo codec (VerKeyKES (KES StandardCrypto))) => HasInfo codec KeyInfo

instance
  ( Codec codec
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  , Serializable codec (VerKeyKES (KES StandardCrypto))
  )
  => Serializable codec KeyInfo where
        encode codec (KeyInfo k) = Data.SerDoc.Class.encode codec k
        decode codec = KeyInfo <$> Data.SerDoc.Class.decode codec

$(deriveSerDoc ''DirectCodec [] ''BootstrapInfo)
$(deriveSerDoc ''DirectCodec [] ''BundleInfo)
$(deriveSerDoc ''DirectCodec [] ''AgentInfo)

controlDriver :: forall m f t p
               . VersionedProtocol (ControlProtocol m)
              => HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
              => HasInfo (DirectCodec m) AgentInfo
              => Serializable (DirectCodec m) AgentInfo
              => MonadThrow m
              => MonadSTM m
              => MonadMVar m
              => MonadFail m
              => MonadST m
              => RawBearer m
              -> Tracer m ControlDriverTrace
              -> Driver (ControlProtocol m) () m
controlDriver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ControlProtocol m))
        sendVersion (Proxy @(ControlProtocol m)) s (ControlDriverSendingVersionID >$< tracer)
      (ServerAgency TokInitial, AbortMessage) ->
        return ()

      (ServerAgency TokIdle, GenStagedKeyMessage) -> do
        sendItem s GenStagedKeyCmd
      (ServerAgency TokIdle, QueryStagedKeyMessage) -> do
        sendItem s QueryStagedKeyCmd
      (ServerAgency TokIdle, DropStagedKeyMessage) -> do
        sendItem s DropStagedKeyCmd
      (ServerAgency TokIdle, RequestInfoMessage) -> do
        sendItem s RequestInfoCmd

      (ServerAgency TokIdle, InstallKeyMessage oc) -> do
        sendItem s InstallKeyCmd
        sendItem s oc

      (ServerAgency TokIdle, EndMessage) -> do
        return ()

      (ServerAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency _, ProtocolErrorMessage) -> do
        return ()

      (ClientAgency TokWaitForPublicKey, PublicKeyMessage vkeyMay) -> do
        case vkeyMay of
          Nothing -> traceWith tracer ControlDriverNoPublicKeyToReturn
          Just _ -> traceWith tracer ControlDriverReturningPublicKey
        sendItem s vkeyMay

      (ClientAgency TokWaitForConfirmation, InstallResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ControlDriverConfirmingKey
        else
          traceWith tracer ControlDriverDecliningKey
        sendItem s reason

      (ClientAgency TokWaitForInfo, InfoMessage info) -> do
        sendItem s info

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer ControlDriverReceivingVersionID
        result <- checkVersion (Proxy @(ControlProtocol m)) s (ControlDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage AbortMessage, ())

      (ServerAgency TokIdle) -> do
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


      (ClientAgency TokWaitForPublicKey) -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ControlDriverReceivingKey
          vkMay <- receiveItem s
          lift $ traceWith tracer
            (maybe
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

      (ClientAgency TokWaitForConfirmation) -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK reason ->
            return (SomeMessage (InstallResultMessage reason), ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())

      (ClientAgency TokWaitForInfo) -> do
        result <- runReadResultT $ receiveItem s
        case result of
          ReadOK info ->
            return (SomeMessage (InfoMessage info), ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage ProtocolErrorMessage, ())

  , startDState = ()
  }

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
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",InitialState,IdleState" ++
              ">")
            (info codec (Proxy @VersionIdentifier))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForPublicKeyState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",IdleState,WaitForPublicKeyState" ++
              ">")
            (info codec (Proxy @Command))

instance (HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))) => HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForPublicKeyState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",WaitForPublicKeyState,IdleState" ++
              ">")
            (info codec (Proxy @(Maybe (VerKeyKES (KES StandardCrypto)))))
instance (HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))) => HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForConfirmationState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",IdleState,WaitForConfirmationState" ++
              ">")
            (info codec (Proxy @(OCert StandardCrypto)))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForConfirmationState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",WaitForConfirmationState,IdleState" ++
              ">")
            (info codec (Proxy @RecvResult))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) IdleState WaitForInfoState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",IdleState,WaitForInfoState" ++
              ">")
            (info codec (Proxy @()))
instance ( HasInfo (DirectCodec m) (VerKeyKES (KES StandardCrypto))
         ) => HasInfo (DirectCodec m) (Message (ControlProtocol m) WaitForInfoState IdleState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",WaitForInfoState,IdleState" ++
              ">")
            (info codec (Proxy @AgentInfo))
instance HasInfo (DirectCodec m) (Message (ControlProtocol m) _st EndState) where
  info codec _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier) ++
              ",st,EndState" ++
              ">")
            (info codec (Proxy @()))
