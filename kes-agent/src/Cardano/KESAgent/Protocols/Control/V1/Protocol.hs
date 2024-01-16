{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.KESAgent.Protocols.Control.V1.Protocol
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Util.RefCounting

import Cardano.Binary
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Mock
import Cardano.Crypto.KES.Single
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.Util ( SignableRepresentation (..) )
import Cardano.Crypto.Hash.Class

import Data.ByteString ( ByteString )
import Data.Text ( Text )
import Data.ByteString qualified as BS
import Data.Proxy ( Proxy (..) )
import Data.Typeable
import Data.Word
import GHC.Generics ( Generic )
import Network.TypedProtocol.Core
import NoThunks.Class ( NoThunks (..) )
import Quiet
import Data.Time (UTCTime)
import Data.SerDoc.Info ( Description (..) )
import Data.SerDoc.Class ( ViaEnum (..) )

data AgentInfo =
  AgentInfo
    { agentInfoCurrentBundle :: !(Maybe BundleInfo)
    , agentInfoStagedKey :: !(Maybe KeyInfo)
    , agentInfoCurrentTime :: !UTCTime
    , agentInfoCurrentKESPeriod :: !KESPeriod
    , agentInfoBootstrapConnections :: ![BootstrapInfo]
    }
    deriving (Show)

deriving instance Eq (VerKeyKES (KES StandardCrypto)) => Eq AgentInfo

data BootstrapInfo =
  BootstrapInfo
    { bootstrapInfoAddress :: !Text
    , bootstrapInfoStatus :: !ConnectionStatus
    }
    deriving (Show, Eq)

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data BundleInfo =
  BundleInfo
    { bundleInfoEvolution :: !Word32
    , bundleInfoStartKESPeriod :: !KESPeriod
    , bundleInfoOCertN :: !Word64
    , bundleInfoVK :: !(VerKeyKES (KES StandardCrypto))
    , bundleInfoSigma :: !(SignedDSIGN (DSIGN StandardCrypto) (OCertSignable StandardCrypto))
    }
    deriving (Show, Eq)

newtype KeyInfo =
  KeyInfo
    { keyInfoVK :: VerKeyKES (KES StandardCrypto)
    }
    deriving (Show)

instance Eq (VerKeyKES (KES StandardCrypto)) => Eq KeyInfo where
  KeyInfo a == KeyInfo b = a == b

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
-- All pushes are confirmed from the receiving end, to make sure they have gone
-- through. This allows the control client to report success to the user, but it
-- also helps make things more predictable in testing, because it means that
-- sending keys is now synchronous.
data ControlProtocol (m :: * -> *) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: ControlProtocol m

  -- | System is idling, waiting for the server to push the next key.
  IdleState :: ControlProtocol m

  -- | Client has requested a new KES key to be generated in the staging area.
  WaitForPublicKeyState :: ControlProtocol m

  -- | Client has requested agent information
  WaitForInfoState :: ControlProtocol m

  -- | An OpCert has been pushed, client must now confirm that it has been
  -- received, and that it matches the staged KES key.
  WaitForConfirmationState :: ControlProtocol m

  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: ControlProtocol m

{-# ANN VersionMessage (Description ["Announce the protocol version."]) #-}
{-# ANN GenStagedKeyMessage
    (Description
      [ "Ask the agent to generate a fresh KES sign key and store it in the staging area."
      , "Corresponds to the @gen-staged-key@ command."
      ]) #-}
{-# ANN QueryStagedKeyMessage
    (Description
      [ "Ask the agent to return the staged KES key, if any."
      , "Only the KES verification key will be returned, in order to guarantee forward security."
      , "Corresponds to the @export-staged-key@ command."
      ]) #-}
{-# ANN DropStagedKeyMessage
    (Description
      [ "Ask the agent to delete the staged KES key, if any."
      , "Corresponds to the @drop-staged-key@ command."
      ]) #-}
{-# ANN PublicKeyMessage
    (Description
      [ "Returned by the KES agent in response to an @export-staged-key@ request."
      ]) #-}
{-# ANN InstallKeyMessage
    (Description
      [ "Upload an OpCert to the KES agent, and ask it to bundle it with a staged KES key and install it."
      , "Corresponds to the @install-key@ command."
      ]) #-}
{-# ANN InstallResultMessage
    (Description
      [ "Returned by the KES agent in response to an @install-key@ command."
      ]) #-}
{-# ANN RequestInfoMessage
    (Description
      [ "Ask the KES agent to report its current state."
      , "Corresponds to the @info@ command."
      ]) #-}
{-# ANN InfoMessage
    (Description
      [ "Returned by the KES agent in response to an @info@ command."
      ]) #-}
{-# ANN AbortMessage
    (Description
      [ "Signals a failed version handshake."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]) #-}
{-# ANN EndMessage
    (Description
      [ "Signals an orderly end of a session."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]) #-}
{-# ANN ProtocolErrorMessage
    (Description
      [ "Signals a fatal protocol error that causes the session to end prematurely."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted, or an unrecoverable error occurs."
      ]) #-}

instance Protocol (ControlProtocol m) where
  data Message (ControlProtocol m) st st' where

          VersionMessage :: Message (ControlProtocol m) InitialState IdleState

          GenStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState

          QueryStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState

          DropStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState

          PublicKeyMessage :: Maybe (VerKeyKES (KES StandardCrypto))
                           -> Message (ControlProtocol m) WaitForPublicKeyState IdleState

          InstallKeyMessage :: OCert StandardCrypto
                            -> Message (ControlProtocol m) IdleState WaitForConfirmationState

          InstallResultMessage :: RecvResult
                               -> Message (ControlProtocol m) WaitForConfirmationState IdleState

          RequestInfoMessage :: Message (ControlProtocol m) IdleState WaitForInfoState

          InfoMessage :: AgentInfo
                      -> Message (ControlProtocol m) WaitForInfoState IdleState

          AbortMessage :: Message (ControlProtocol m) InitialState EndState
          EndMessage :: Message (ControlProtocol m) IdleState EndState
          ProtocolErrorMessage :: Message (ControlProtocol m) a EndState

  -- | Server always has agency, except between sending a key and confirming it
  data ServerHasAgency st where
    TokInitial :: ServerHasAgency InitialState
    TokIdle :: ServerHasAgency IdleState

  -- | Client only has agency between sending a key and confirming it
  data ClientHasAgency st where
    TokWaitForConfirmation :: ClientHasAgency WaitForConfirmationState
    TokWaitForPublicKey :: ClientHasAgency WaitForPublicKeyState
    TokWaitForInfo :: ClientHasAgency WaitForInfoState

  -- | Someone, i.e., the server, always has agency
  data NobodyHasAgency st where
    TokEnd :: NobodyHasAgency EndState

  exclusionLemma_ClientAndServerHaveAgency tok1 tok2 =
    case tok1 of
      TokWaitForConfirmation ->
        case tok2 of {}
      TokWaitForPublicKey ->
        case tok2 of {}
      TokWaitForInfo ->
        case tok2 of {}
  exclusionLemma_NobodyAndClientHaveAgency _ _ = undefined
  exclusionLemma_NobodyAndServerHaveAgency _ _ = undefined

instance VersionedProtocol (ControlProtocol m) where
  versionIdentifier _ = cpVersionIdentifier

cpVersionIdentifier :: VersionIdentifier
cpVersionIdentifier = mkVersionIdentifier "Control:1.0"
