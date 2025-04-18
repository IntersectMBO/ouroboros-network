{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Common types used in all/most protocols.
module Cardano.KESAgent.Protocols.Types
where

import Cardano.KESAgent.KES.Bundle
import Cardano.KESAgent.KES.Crypto (Crypto (..))
import Cardano.KESAgent.KES.OCert (OCert (..))
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Cardano.Crypto.KES.Class (KESAlgorithm, rawSerialiseVerKeyKES)
import Data.ByteString (ByteString)
import Data.Char (toLower)
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
import Text.Casing
import Text.Printf

-- | Representation of a key bundle in trace logs. Reflects the key's serial
-- number and verification key (we're not logging the key itself, because that
-- would potentially leak it to unsafe memory and/or persistent storage).
data BundleTrace
  = BundleTrace
  { keyIdentSerial :: !Word64
  , keyIdentVKHex :: !ByteString
  }
  deriving (Show)

instance Pretty BundleTrace where
  pretty (BundleTrace n vk) =
    printf "#%u:%s" n (pretty vk)

-- | Representation of a tagged key bundle in trace logs. We always trace the
-- timestamp, plus, if present the actual key bundle.
data TaggedBundleTrace
  = TaggedBundleTrace
  { keyMutationTimestamp :: !Timestamp
  , keyMutationKey :: !(Maybe BundleTrace)
  }
  deriving (Show)

mkTaggedBundleTrace ::
  KESAlgorithm (KES c) =>
  Timestamp ->
  Maybe (Bundle m c) ->
  TaggedBundleTrace
mkTaggedBundleTrace ts bundle =
  TaggedBundleTrace
    ts
    (mkBundleTrace <$> bundle)

mkBundleTrace ::
  KESAlgorithm (KES c) =>
  Bundle m c ->
  BundleTrace
mkBundleTrace bundle =
  BundleTrace
    (ocertN (bundleOC bundle))
    (rawSerialiseVerKeyKES $ ocertVkHot (bundleOC bundle))

instance Pretty TaggedBundleTrace where
  pretty (TaggedBundleTrace ts keyMay) =
    maybe "<DROP KEY>" pretty keyMay ++ "[" ++ pretty ts ++ "]"

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
  pretty (ControlDriverSendingVersionID x) = "sending version ID " ++ pretty x
  pretty (ControlDriverReceivedVersionID x) = "received version ID " ++ pretty x
  pretty (ControlDriverInvalidVersion v1 v2) = "invalid version " ++ pretty v1 ++ " " ++ pretty v2
  pretty (ControlDriverSendingCommand x) = "sending command " ++ pretty x
  pretty (ControlDriverSentCommand x) = "sent command" ++ pretty x
  pretty (ControlDriverReceivedKey x) = "received key " ++ pretty x
  pretty (ControlDriverReceivedCommand x) = "received command " ++ pretty x
  pretty (ControlDriverMisc x) = x
  pretty x = prettify (drop (strLength "ControlDriver") (show x))
    where
      prettify str =
        case words str of
          [] -> ""
          x : xs -> unwords ((map toLower . toWords . fromAny) x : xs)

-- | Commands that a control client can send
data Command
  = GenStagedKeyCmd
  | QueryStagedKeyCmd
  | DropStagedKeyCmd
  | InstallKeyCmd
  | RequestInfoCmd
  | DropKeyCmd
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance Pretty Command where
  pretty = map toLower . unwords . init . words . toWords . fromAny . show

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
  | ServiceDriverSendingKey !TaggedBundleTrace
  | ServiceDriverSentKey !TaggedBundleTrace
  | ServiceDriverReceivingKey
  | ServiceDriverReceivedKey !TaggedBundleTrace
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
  pretty (ServiceDriverSendingVersionID v) = "sending version ID " ++ pretty v
  pretty (ServiceDriverReceivedVersionID v) = "received version ID " ++ pretty v
  pretty (ServiceDriverInvalidVersion v1 v2) = "invalid version " ++ pretty v1 ++ " " ++ pretty v2
  pretty (ServiceDriverSendingKey k) = "sending key " ++ pretty k
  pretty (ServiceDriverSentKey k) = "sent key " ++ pretty k
  pretty (ServiceDriverReceivedKey k) = "received key " ++ pretty k
  pretty (ServiceDriverConfirmingKey) = "confirming key"
  pretty (ServiceDriverConfirmedKey) = "confirmed key"
  pretty (ServiceDriverDecliningKey r) = "declining key " ++ pretty r
  pretty (ServiceDriverDeclinedKey r) = "declined key " ++ pretty r
  pretty (ServiceDriverRequestingKeyDrop ts) = "requesting key drop " ++ pretty ts
  pretty (ServiceDriverRequestedKeyDrop ts) = "requested key drop " ++ pretty ts
  pretty (ServiceDriverReceivedKeyDrop ts) = "received key drop " ++ pretty ts
  pretty (ServiceDriverCRefEvent e) = "CRef event " ++ pretty e
  pretty (ServiceDriverProtocolError err) = "protocol error " ++ err
  pretty (ServiceDriverMisc x) = x
  pretty x = prettify (drop (strLength "ServiceDriver") (show x))
    where
      prettify str =
        case words str of
          [] -> ""
          x : xs -> unwords ((map toLower . toWords . fromAny) x : xs)
