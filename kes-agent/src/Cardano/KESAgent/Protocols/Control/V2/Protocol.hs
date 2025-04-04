{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.Control.V2.Protocol
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.StandardCrypto
import Cardano.KESAgent.Protocols.VersionedProtocol

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class

import Data.Kind (Type)
import Data.SerDoc.Info (Description (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word
import Network.TypedProtocol.Core

data AgentInfo
  = AgentInfo
  { agentInfoCurrentBundle :: !(Maybe TaggedBundleInfo)
  , agentInfoStagedKey :: !(Maybe KeyInfo)
  , agentInfoCurrentTime :: !UTCTime
  , agentInfoCurrentKESPeriod :: !KESPeriod
  , agentInfoBootstrapConnections :: ![BootstrapInfo]
  }
  deriving (Show)

deriving instance Eq (VerKeyKES (KES StandardCrypto)) => Eq AgentInfo

data BootstrapInfo
  = BootstrapInfo
  { bootstrapInfoAddress :: !Text
  , bootstrapInfoStatus :: !ConnectionStatus
  }
  deriving (Show, Eq)

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data TaggedBundleInfo
  = TaggedBundleInfo
  { taggedBundleInfo :: Maybe BundleInfo
  , taggedBundleInfoTimestamp :: Maybe UTCTime
  }
  deriving (Show, Eq)

data BundleInfo
  = BundleInfo
  { bundleInfoEvolution :: !Word32
  , bundleInfoStartKESPeriod :: !KESPeriod
  , bundleInfoOCertN :: !Word64
  , bundleInfoVK :: !(VerKeyKES (KES StandardCrypto))
  , bundleInfoSigma :: !(SignedDSIGN (DSIGN StandardCrypto) (OCertSignable StandardCrypto))
  }
  deriving (Show, Eq)

newtype KeyInfo
  = KeyInfo
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
data ControlProtocol (m :: Type -> Type) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: ControlProtocol m
  -- | System is idling, waiting for the server to push the next key.
  IdleState :: ControlProtocol m
  -- | Client has made a request that will result in a public key being
  -- returned.
  WaitForPublicKeyState :: ControlProtocol m
  -- | Client has requested an installed key to be dropped.
  WaitForDropConfirmationState :: ControlProtocol m
  -- | Client has requested agent information
  WaitForInfoState :: ControlProtocol m
  -- | An OpCert has been pushed, client must now confirm that it has been
  -- received, and that it matches the staged KES key.
  WaitForKeyConfirmationState :: ControlProtocol m
  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: ControlProtocol m

{-# ANN VersionMessage (Description ["Announce the protocol version."]) #-}
{-# ANN
  GenStagedKeyMessage
  ( Description
      [ "Ask the agent to generate a fresh KES sign key and store it in the staging area."
      , "Corresponds to the @gen-staged-key@ command."
      ]
  )
  #-}
{-# ANN
  QueryStagedKeyMessage
  ( Description
      [ "Ask the agent to return the staged KES key, if any."
      , "Only the KES verification key will be returned, in order to guarantee forward security."
      , "Corresponds to the @export-staged-key@ command."
      ]
  )
  #-}
{-# ANN
  DropStagedKeyMessage
  ( Description
      [ "Ask the agent to delete the staged KES key, if any."
      , "Corresponds to the @drop-staged-key@ command."
      ]
  )
  #-}
{-# ANN
  PublicKeyMessage
  ( Description
      [ "Returned by the KES agent in response to an @export-staged-key@ request."
      ]
  )
  #-}
{-# ANN
  InstallKeyMessage
  ( Description
      [ "Upload an OpCert to the KES agent, and ask it to bundle it with a staged KES key and install it."
      , "Corresponds to the @install-key@ command."
      ]
  )
  #-}
{-# ANN
  InstallResultMessage
  ( Description
      [ "Returned by the KES agent in response to an @install-key@ command."
      ]
  )
  #-}
{-# ANN
  DropKeyMessage
  ( Description
      [ "Ask the KES agent to drop the currently active key."
      , "Corresponds to the @drop-key@ command."
      ]
  )
  #-}
{-# ANN
  DropKeyResultMessage
  ( Description
      [ "Returned by the KES agent in response to a @drop-key@ request."
      ]
  )
  #-}
{-# ANN
  RequestInfoMessage
  ( Description
      [ "Ask the KES agent to report its current state."
      , "Corresponds to the @info@ command."
      ]
  )
  #-}
{-# ANN
  InfoMessage
  ( Description
      [ "Returned by the KES agent in response to an @info@ command."
      ]
  )
  #-}
{-# ANN
  AbortMessage
  ( Description
      [ "Signals a failed version handshake."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]
  )
  #-}
{-# ANN
  EndMessage
  ( Description
      [ "Signals an orderly end of a session."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted."
      ]
  )
  #-}
{-# ANN
  ProtocolErrorMessage
  ( Description
      [ "Signals a fatal protocol error that causes the session to end prematurely."
      , "No data is actually sent for this message, instead it is generated when the network connection is interrupted, or an unrecoverable error occurs."
      ]
  )
  #-}

instance Protocol (ControlProtocol m) where
  data Message (ControlProtocol m) st st' where
    VersionMessage :: Message (ControlProtocol m) InitialState IdleState
    GenStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState
    QueryStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState
    DropStagedKeyMessage :: Message (ControlProtocol m) IdleState WaitForPublicKeyState
    PublicKeyMessage ::
      Maybe (VerKeyKES (KES StandardCrypto)) ->
      Message (ControlProtocol m) WaitForPublicKeyState IdleState
    InstallKeyMessage ::
      OCert StandardCrypto ->
      Message (ControlProtocol m) IdleState WaitForKeyConfirmationState
    InstallResultMessage ::
      RecvResult ->
      Message (ControlProtocol m) WaitForKeyConfirmationState IdleState
    DropKeyMessage ::
      Message (ControlProtocol m) IdleState WaitForDropConfirmationState
    DropKeyResultMessage ::
      RecvResult ->
      Message (ControlProtocol m) WaitForDropConfirmationState IdleState
    RequestInfoMessage :: Message (ControlProtocol m) IdleState WaitForInfoState
    InfoMessage ::
      AgentInfo ->
      Message (ControlProtocol m) WaitForInfoState IdleState
    AbortMessage :: Message (ControlProtocol m) InitialState EndState
    EndMessage :: Message (ControlProtocol m) IdleState EndState
    ProtocolErrorMessage :: Message (ControlProtocol m) a EndState

  type StateAgency InitialState = ServerAgency
  type StateAgency IdleState = ServerAgency

  type StateAgency WaitForKeyConfirmationState = ClientAgency
  type StateAgency WaitForDropConfirmationState = ClientAgency
  type StateAgency WaitForPublicKeyState = ClientAgency
  type StateAgency WaitForInfoState = ClientAgency

  type StateAgency EndState = NobodyAgency

  type StateToken = SControlProtocol

data SControlProtocol (st :: ControlProtocol m) where
  SInitialState :: SControlProtocol InitialState
  SIdleState :: SControlProtocol IdleState
  SWaitForKeyConfirmationState :: SControlProtocol WaitForKeyConfirmationState
  SWaitForDropConfirmationState :: SControlProtocol WaitForDropConfirmationState
  SWaitForPublicKeyState :: SControlProtocol WaitForPublicKeyState
  SWaitForInfoState :: SControlProtocol WaitForInfoState
  SEndState :: SControlProtocol EndState

instance StateTokenI InitialState where stateToken = SInitialState
instance StateTokenI IdleState where stateToken = SIdleState
instance StateTokenI WaitForKeyConfirmationState where stateToken = SWaitForKeyConfirmationState
instance StateTokenI WaitForPublicKeyState where stateToken = SWaitForPublicKeyState
instance StateTokenI WaitForDropConfirmationState where stateToken = SWaitForDropConfirmationState
instance StateTokenI WaitForInfoState where stateToken = SWaitForInfoState
instance StateTokenI EndState where stateToken = SEndState

instance VersionedProtocol (ControlProtocol m) where
  versionIdentifier _ = cpVersionIdentifier

cpVersionIdentifier :: VersionIdentifier
cpVersionIdentifier = mkVersionIdentifier "Control:2.0"
