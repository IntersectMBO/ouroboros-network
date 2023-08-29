{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.KESAgent.Protocols.Control.Driver
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.Control.Protocol
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
import Control.Monad.Trans (lift)
import Control.Monad.Class.MonadMVar
import Control.Monad.Class.MonadST
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow ( MonadThrow, bracket )
import Control.Tracer ( Tracer, traceWith )
import Data.Binary ( decode, encode )
import Data.ByteString ( ByteString )
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
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

sendCommand :: ( MonadST m
               , MonadThrow m
               )
            => RawBearer m
            -> Tracer m ControlDriverTrace
            -> Command
            -> m ()
sendCommand s tracer cmd = do
  traceWith tracer $ ControlDriverSendingCommand cmd
  sendWord32 s $ fromIntegral $ fromEnum cmd
  traceWith tracer $ ControlDriverSentCommand cmd

receiveCommand :: ( MonadST m
                  , MonadThrow m
                  )
               => RawBearer m
               -> m (ReadResult Command)
receiveCommand s = runReadResultT $ do
  w <- fromIntegral <$> ReadResultT (receiveWord32 s)
  if w > fromEnum (maxBound :: Command) then
    readResultT (ReadMalformed "Command")
  else
    return $ toEnum w

sendVKeyMay :: ( MonadST m
               , MonadThrow m
               , KESAlgorithm k
               )
            => RawBearer m
            -> Maybe (VerKeyKES k)
            -> m ()
sendVKeyMay s (Just vk) = do
  let keyBytes = rawSerialiseVerKeyKES vk
  sendWord32 s (fromIntegral $ BS.length keyBytes)
  void $ sendBS s keyBytes
sendVKeyMay s Nothing = do
  sendWord32 s 0

receiveVKeyMay :: ( MonadST m
               , MonadThrow m
               , KESAlgorithm k
               )
            => RawBearer m
            -> m (ReadResult (Maybe (VerKeyKES k)))
receiveVKeyMay s = runReadResultT $ do
  l <- fromIntegral <$> ReadResultT (receiveWord32 s)
  if l == 0 then
    return Nothing
  else do
    keyBytes <- ReadResultT (receiveBS s l)
    case rawDeserialiseVerKeyKES keyBytes of
      Nothing -> readResultT (ReadMalformed "KES VerKey")
      Just vk -> return (Just vk)

controlDriver :: forall c m f t p
               . Crypto c
              => Typeable c
              => VersionedProtocol (ControlProtocol m c)
              => KESAlgorithm (KES c)
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
        sendCommand s tracer GenStagedKeyCmd
      (ServerAgency TokIdle, QueryStagedKeyMessage) -> do
        sendCommand s tracer QueryStagedKeyCmd
      (ServerAgency TokIdle, DropStagedKeyMessage) -> do
        sendCommand s tracer DropStagedKeyCmd

      (ServerAgency TokIdle, InstallKeyMessage oc) -> do
        sendCommand s tracer InstallKeyCmd
        sendOC s oc

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
        sendVKeyMay s vkeyMay

      (ClientAgency TokWaitForConfirmation, InstallResultMessage reason) -> do
        if reason == RecvOK then
          traceWith tracer ControlDriverConfirmingKey
        else
          traceWith tracer ControlDriverDecliningKey
        sendRecvResult s reason

  , recvMessage = \agency () -> case agency of
      (ServerAgency TokInitial) -> do
        traceWith tracer ControlDriverReceivingVersionID
        result <- receiveVersion (Proxy @(ControlProtocol m c)) s (ControlDriverReceivedVersionID >$< tracer)
        case result of
          ReadOK _ ->  
            return (SomeMessage VersionMessage, ())
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage AbortMessage, ())

      (ServerAgency TokIdle) -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ControlDriverReceivingCommand
          cmd <- ReadResultT $ receiveCommand s
          lift $ traceWith tracer (ControlDriverReceivedCommand cmd)
          case cmd of
            GenStagedKeyCmd ->
              return (SomeMessage GenStagedKeyMessage, ())
            QueryStagedKeyCmd ->
              return (SomeMessage QueryStagedKeyMessage, ())
            DropStagedKeyCmd ->
              return (SomeMessage DropStagedKeyMessage, ())
            InstallKeyCmd -> do
              oc <- ReadResultT $ receiveOC s (ControlDriverMisc >$< tracer)
              return (SomeMessage (InstallKeyMessage oc), ())
        case result of
          ReadOK msg ->
            return msg
          err -> do
            traceWith tracer $ readErrorToControlDriverTrace err
            return (SomeMessage EndMessage, ())


      (ClientAgency TokWaitForPublicKey) -> do
        result <- runReadResultT $ do
          lift $ traceWith tracer ControlDriverReceivingKey
          vkMay <- ReadResultT $ receiveVKeyMay s
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
        result <- receiveRecvResult s
        case result of
          ReadOK reason ->
            return (SomeMessage (InstallResultMessage reason), ())
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

