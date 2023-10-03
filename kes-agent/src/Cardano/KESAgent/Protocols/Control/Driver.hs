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

module Cardano.KESAgent.Protocols.Control.Driver
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.Protocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Serialization.RawUtil
import Cardano.KESAgent.Serialization.Spec
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

-- | Logging messages that the Driver may send
data ControlDriverTrace
  = ControlDriverSendingVersionID !VersionIdentifier
  | ControlDriverReceivingVersionID
  | ControlDriverReceivedVersionID !VersionIdentifier
  | ControlDriverInvalidVersion !VersionIdentifier !VersionIdentifier
  | ControlDriverSendingCommand !Command
  | ControlDriverSentCommand !Command
  | ControlDriverReceivingKey
  | ControlDriverReceivedKey !ByteString
  | ControlDriverInvalidKey
  | ControlDriverReceivingCommand
  | ControlDriverReceivedCommand !Command
  | ControlDriverConfirmingKey
  | ControlDriverConfirmedKey
  | ControlDriverDecliningKey
  | ControlDriverDeclinedKey
  | ControlDriverNoPublicKeyToReturn
  | ControlDriverReturningPublicKey
  | ControlDriverConnectionClosed
  | ControlDriverCRefEvent CRefEvent
  | ControlDriverInvalidCommand
  | ControlDriverProtocolError String
  | ControlDriverMisc String
  deriving (Show)

instance Pretty ControlDriverTrace where
  pretty (ControlDriverMisc x) = x
  pretty x = drop (strLength "ControlDriver") (show x)

data Command
  = GenStagedKeyCmd
  | QueryStagedKeyCmd
  | DropStagedKeyCmd
  | InstallKeyCmd
  | RequestInfoCmd
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

deriving via (ViaEnum Command)
  instance HasSerInfo Command

deriving via (ViaEnum Command)
  instance ( forall x y. Coercible x y => Coercible (m x) (m y)
           , MonadThrow m
           , MonadST m
           ) => IsSerItem m Command

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

deriving via (ViaEnum ConnectionStatus)
  instance HasSerInfo ConnectionStatus
deriving via (ViaEnum ConnectionStatus)
  instance ( forall x y. Coercible x y => Coercible (m x) (m y)
           , MonadThrow m
           , MonadST m
           , MonadSTM m
           ) => IsSerItem m ConnectionStatus

deriving newtype instance
  ( HasSerInfo (VerKeyKES (KES c))
  , KESAlgorithm (KES c)
  )
  => HasSerInfo (KeyInfo c)

deriving newtype instance
  ( (forall x y. Coercible x y => Coercible (m x) (m y))
  , HasSerInfo (VerKeyKES (KES c))
  , KESAlgorithm (KES c)
  , MonadThrow m
  , MonadST m
  , MonadSTM m
  )
  => IsSerItem m (KeyInfo c)

$(deriveSer ''BootstrapInfo)
$(deriveSerWithCrypto ''BundleInfo)
$(deriveSerWithCrypto ''AgentInfo)

controlDriver :: forall c m f t p
               . (forall x y. Coercible x y => Coercible (m x) (m y))
              => Crypto c
              => Typeable c
              => VersionedProtocol (ControlProtocol m c)
              => KESAlgorithm (KES c)
              => HasSerInfo (VerKeyKES (KES c))
              => DirectDeserialise m (SignKeyKES (KES c))
              => DirectSerialise m (SignKeyKES (KES c))
              => MonadThrow m
              => MonadSTM m
              => MonadMVar m
              => MonadFail m
              => MonadST m
              => RawBearer m
              -> Tracer m ControlDriverTrace
              -> Driver (ControlProtocol m c) () m
controlDriver s tracer = Driver
  { sendMessage = \agency msg -> case (agency, msg) of
      (ServerAgency TokInitial, VersionMessage) -> do
        let VersionIdentifier v = versionIdentifier (Proxy @(ControlProtocol m c))
        sendVersion (Proxy @(ControlProtocol m c)) s (ControlDriverSendingVersionID >$< tracer)
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
        result <- checkVersion (Proxy @(ControlProtocol m c)) s (ControlDriverReceivedVersionID >$< tracer)
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


instance NamedCrypto c => HasSerInfo (Message (ControlProtocol m c) InitialState IdleState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",InitialState,IdleState" ++
              ">")
            (info (Proxy @VersionIdentifier))
instance NamedCrypto c => HasSerInfo (Message (ControlProtocol m c) IdleState WaitForPublicKeyState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",IdleState,WaitForPublicKeyState" ++
              ">")
            (info (Proxy @Command))
instance (NamedCrypto c, HasSerInfo (VerKeyKES (KES c))) => HasSerInfo (Message (ControlProtocol m c) WaitForPublicKeyState IdleState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",WaitForPublicKeyState,IdleState" ++
              ">")
            (info (Proxy @(Maybe (VerKeyKES (KES c)))))
instance (NamedCrypto c, HasSerInfo (VerKeyKES (KES c)), KESAlgorithm (KES c), DSIGNAlgorithm (DSIGN c)) => HasSerInfo (Message (ControlProtocol m c) IdleState WaitForConfirmationState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",IdleState,WaitForConfirmationState" ++
              ">")
            (info (Proxy @(OCert c)))
instance (NamedCrypto c) => HasSerInfo (Message (ControlProtocol m c) WaitForConfirmationState IdleState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",WaitForConfirmationState,IdleState" ++
              ">")
            (info (Proxy @RecvResult))
instance (NamedCrypto c) => HasSerInfo (Message (ControlProtocol m c) IdleState WaitForInfoState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",IdleState,WaitForInfoState" ++
              ">")
            (info (Proxy @()))
instance (NamedCrypto c, KESAlgorithm (KES c), DSIGNAlgorithm (DSIGN c), HasSerInfo (VerKeyKES (KES c))) => HasSerInfo (Message (ControlProtocol m c) WaitForInfoState IdleState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",WaitForInfoState,IdleState" ++
              ">")
            (info (Proxy @(AgentInfo c)))
instance (NamedCrypto c) => HasSerInfo (Message (ControlProtocol m c) _st EndState) where
  info _ = aliasField
            ("Message<" ++
              (Text.unpack . decodeUtf8 . unVersionIdentifier $ cpVersionIdentifier (Proxy @(ControlProtocol m c))) ++
              ",st,EndState" ++
              ">")
            (info (Proxy @()))
