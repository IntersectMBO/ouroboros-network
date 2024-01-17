{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.Types
where

import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Util.Pretty
import Cardano.KESAgent.Util.RefCounting

import Data.ByteString (ByteString)
import Data.SerDoc.Class ( ViaEnum (..), Codec (..), HasInfo (..), Serializable (..), encodeEnum, decodeEnum, enumInfo )
import Data.Proxy

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
  instance
    ( Codec codec
    , HasInfo codec (DefEnumEncoding codec)
    , Integral (DefEnumEncoding codec)
    ) => HasInfo codec Command

instance
  ( Codec codec
  , Serializable codec (DefEnumEncoding codec)
  , Integral (DefEnumEncoding codec)
  , Monad (MonadEncode codec)
  , Monad (MonadDecode codec)
  ) => Serializable codec Command where
    encode codec = encodeEnum codec (Proxy @(DefEnumEncoding codec))
    decode codec = decodeEnum codec (Proxy @(DefEnumEncoding codec))

