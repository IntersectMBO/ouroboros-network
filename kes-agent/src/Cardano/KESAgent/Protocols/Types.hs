{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.Types
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Util.HexBS
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class (KESAlgorithm, rawSerialiseVerKeyKES)
import Data.ByteString (ByteString)
import Data.Proxy
import Data.SerDoc.Class (
  Codec (..),
  HasInfo (..),
  Serializable (..),
  ViaEnum (..),
  decodeEnum,
  encodeEnum,
 )
import Data.Word
import Text.Printf

data KeyTrace
  = KeyTrace
  { keyIdentSerial :: !Word64
  , keyIdentVKHex :: !ByteString
  }
  deriving (Show)

instance Pretty KeyTrace where
  pretty (KeyTrace n vk) =
    printf "#%u:%s..." n (take 10 $ hexShowBS vk)

data KeyMutationTrace
  = KeyMutationTrace
  { keyMutationTimestamp :: !Timestamp
  , keyMutationKey :: !(Maybe KeyTrace)
  }
  deriving (Show)

mkKeyMutationTrace ::
  KESAlgorithm (KES c) =>
  Timestamp ->
  Maybe (Bundle m c) ->
  KeyMutationTrace
mkKeyMutationTrace ts bundle =
  KeyMutationTrace
    ts
    (mkKeyTrace <$> bundle)

mkKeyTrace ::
  KESAlgorithm (KES c) =>
  Bundle m c ->
  KeyTrace
mkKeyTrace bundle =
  KeyTrace
    (ocertN (bundleOC bundle))
    (rawSerialiseVerKeyKES $ ocertVkHot (bundleOC bundle))

instance Pretty KeyMutationTrace where
  pretty (KeyMutationTrace ts keyMay) =
    pretty ts ++ " " ++ maybe "<DROP KEY>" pretty keyMay

-- | Logging messages that the ControlDriver may send
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
  | ControlDriverConfirmingKeyDrop
  | ControlDriverConfirmedKeyDrop
  | ControlDriverDecliningKeyDrop
  | ControlDriverDeclinedKeyDrop
  | ControlDriverNoPublicKeyToReturn
  | ControlDriverNoPublicKeyToDrop
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
  | DropKeyCmd
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

deriving via
  (ViaEnum Command)
  instance
    ( Codec codec
    , HasInfo codec (DefEnumEncoding codec)
    , Integral (DefEnumEncoding codec)
    ) =>
    HasInfo codec Command

instance
  ( Codec codec
  , Serializable codec (DefEnumEncoding codec)
  , Integral (DefEnumEncoding codec)
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) =>
  Serializable codec Command
  where
  encode codec = encodeEnum codec (Proxy @(DefEnumEncoding codec))
  decode codec = decodeEnum codec (Proxy @(DefEnumEncoding codec))

-- | Logging messages that the ServiceDriver may send
data ServiceDriverTrace
  = ServiceDriverSendingVersionID !VersionIdentifier
  | ServiceDriverReceivingVersionID
  | ServiceDriverReceivedVersionID !VersionIdentifier
  | ServiceDriverInvalidVersion !VersionIdentifier !VersionIdentifier
  | ServiceDriverSendingKey !KeyMutationTrace
  | ServiceDriverSentKey !KeyMutationTrace
  | ServiceDriverReceivingKey
  | ServiceDriverReceivedKey !KeyMutationTrace
  | ServiceDriverConfirmingKey
  | ServiceDriverConfirmedKey
  | ServiceDriverDecliningKey !RecvResult
  | ServiceDriverDeclinedKey !RecvResult
  | ServiceDriverRequestingKeyDrop !Timestamp
  | ServiceDriverRequestedKeyDrop !Timestamp
  | ServiceDriverReceivingKeyDrop
  | ServiceDriverReceivedKeyDrop !Timestamp
  | ServiceDriverConnectionClosed
  | ServiceDriverCRefEvent !CRefEvent
  | ServiceDriverProtocolError !String
  | ServiceDriverMisc String
  deriving (Show)

instance Pretty ServiceDriverTrace where
  pretty (ServiceDriverSendingVersionID v) = "SendingVersionID " ++ pretty v
  pretty (ServiceDriverReceivedVersionID v) = "ReceivedVersionID " ++ pretty v
  pretty (ServiceDriverInvalidVersion v1 v2) = "InvalidVersion " ++ pretty v1 ++ " " ++ pretty v2
  pretty (ServiceDriverSendingKey k) = "SendingKey " ++ pretty k
  pretty (ServiceDriverSentKey k) = "SentKey " ++ pretty k
  pretty (ServiceDriverReceivedKey k) = "ReceivedKey " ++ pretty k
  pretty (ServiceDriverConfirmingKey) = "ConfirmingKey"
  pretty (ServiceDriverConfirmedKey) = "ConfirmedKey"
  pretty (ServiceDriverDecliningKey r) = "DecliningKey " ++ pretty r
  pretty (ServiceDriverDeclinedKey r) = "DeclinedKey " ++ pretty r
  pretty (ServiceDriverRequestingKeyDrop ts) = "RequestingKeyDrop " ++ pretty ts
  pretty (ServiceDriverRequestedKeyDrop ts) = "RequestedKeyDrop " ++ pretty ts
  pretty (ServiceDriverReceivedKeyDrop ts) = "ReceivedKeyDrop " ++ pretty ts
  pretty (ServiceDriverCRefEvent e) = "CRefEvent " ++ pretty e
  pretty (ServiceDriverProtocolError err) = "ProtocolError " ++ err
  pretty (ServiceDriverMisc x) = x
  pretty x = drop (strLength "ServiceDriver") (show x)
