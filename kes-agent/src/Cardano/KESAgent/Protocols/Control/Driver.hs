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
data ControlDriverTrace
  = ControlDriverSendingVersionID VersionIdentifier
  | ControlDriverReceivingVersionID
  | ControlDriverReceivedVersionID VersionIdentifier
  | ControlDriverInvalidVersion
  | ControlDriverSendingCommand Command
  | ControlDriverSentCommand Command
  | ControlDriverReceivingKey
  | ControlDriverReceivedKey Word64
  | ControlDriverConfirmingKey
  | ControlDriverConfirmedKey
  | ControlDriverDecliningKey
  | ControlDriverDeclinedKey
  | ControlDriverNoPublicKeyToReturn
  | ControlDriverReturningPublicKey
  | ControlDriverConnectionClosed
  | ControlDriverCRefEvent CRefEvent
  | ControlDriverInvalidCommand
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
               -> m (Maybe Command)
receiveCommand s = do
  wMay <- receiveWord32 s
  return $ do
    w <- fromIntegral <$> wMay
    if w > fromEnum (maxBound :: Command) then
      Nothing
    else
      Just $ toEnum w

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
            -> m (Maybe (VerKeyKES k))
receiveVKeyMay s = do
  lMay <- fmap fromIntegral <$> receiveWord32 s
  case lMay of
    Nothing -> return Nothing
    Just 0 -> return Nothing
    Just l -> do
      keyBytes <- receiveBS s l
      return $ rawDeserialiseVerKeyKES keyBytes

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
          Right _ ->  
            return (SomeMessage VersionMessage, ())
          Left (expectedV, v) -> do
            traceWith tracer ControlDriverInvalidVersion
            return (SomeMessage AbortMessage, ())

      (ServerAgency TokIdle) -> do
        cmdMay <- receiveCommand s
        case cmdMay of
          Nothing -> do
            traceWith tracer ControlDriverInvalidCommand
            return (SomeMessage EndMessage, ())
          Just GenStagedKeyCmd ->
            return (SomeMessage GenStagedKeyMessage, ())
          Just QueryStagedKeyCmd ->
            return (SomeMessage QueryStagedKeyMessage, ())
          Just DropStagedKeyCmd ->
            return (SomeMessage DropStagedKeyMessage, ())
          Just InstallKeyCmd -> do
            oc <- receiveOC s (ControlDriverMisc >$< tracer)
            return (SomeMessage (InstallKeyMessage oc), ())

      (ClientAgency TokWaitForPublicKey) -> do
        vkeyMay <- receiveVKeyMay s
        return (SomeMessage (PublicKeyMessage vkeyMay), ())

      (ClientAgency TokWaitForConfirmation) -> do
        reason <- receiveRecvResult s
        return (SomeMessage (InstallResultMessage reason), ())

  , startDState = ()
  }
