{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.KESAgent.Protocols.Control.V0.Protocol
where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Protocols.VersionedProtocol

import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.KES.Class

import Control.DeepSeq (NFData)
import Data.Kind (Type)
import Data.SerDoc.Info (Description (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable
import Data.Word
import GHC.Generics
import Network.TypedProtocol.Core

data AgentInfo c
  = AgentInfo
  { agentInfoCurrentBundle :: !(Maybe (BundleInfo c))
  , agentInfoStagedKey :: !(Maybe (KeyInfo c))
  , agentInfoCurrentTime :: !UTCTime
  , agentInfoCurrentKESPeriod :: !KESPeriod
  , agentInfoBootstrapConnections :: ![BootstrapInfo]
  }
  deriving (Generic)

instance
  ( NFData (VerKeyKES kes)
  , kes ~ KES c
  , NFData (SigDSIGN dsign)
  , dsign ~ DSIGN c
  ) =>
  NFData (AgentInfo c)

deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) =>
  Show (AgentInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) =>
  Eq (AgentInfo c)

data BootstrapInfo
  = BootstrapInfo
  { bootstrapInfoAddress :: !Text
  , bootstrapInfoStatus :: !ConnectionStatus
  }
  deriving (Show, Eq, Generic, NFData)

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic, NFData)

data BundleInfo c
  = BundleInfo
  { bundleInfoEvolution :: !Word32
  , bundleInfoStartKESPeriod :: !KESPeriod
  , bundleInfoOCertN :: !Word64
  , bundleInfoVK :: !(VerKeyKES (KES c))
  , bundleInfoSigma :: !(SignedDSIGN (DSIGN c) (OCertSignable c))
  }
  deriving (Generic)

instance
  ( NFData (VerKeyKES kes)
  , kes ~ KES c
  , NFData (SignedDSIGN dsign (OCertSignable c))
  , dsign ~ DSIGN c
  ) =>
  NFData (BundleInfo c)

deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) =>
  Show (BundleInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) =>
  Eq (BundleInfo c)

newtype KeyInfo c
  = KeyInfo
  { keyInfoVK :: VerKeyKES (KES c)
  }
  deriving (Generic)

deriving instance
  KESAlgorithm (KES c) =>
  Show (KeyInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) =>
  Eq (KeyInfo c)

instance
  ( NFData (VerKeyKES kes)
  , kes ~ KES c
  ) =>
  NFData (KeyInfo c)

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
data ControlProtocol (m :: Type -> Type) (k :: Type) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: ControlProtocol m k
  -- | System is idling, waiting for the server to push the next key.
  IdleState :: ControlProtocol m k
  -- | Client has requested a new KES key to be generated in the staging area.
  WaitForPublicKeyState :: ControlProtocol m k
  -- | Client has requested agent information
  WaitForInfoState :: ControlProtocol m k
  -- | An OpCert has been pushed, client must now confirm that it has been
  -- received, and that it matches the staged KES key.
  WaitForConfirmationState :: ControlProtocol m k
  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: ControlProtocol m k

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

instance Protocol (ControlProtocol m c) where
  data Message (ControlProtocol m c) st st' where
    VersionMessage :: Message (ControlProtocol m c) InitialState IdleState
    GenStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState
    QueryStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState
    DropStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState
    PublicKeyMessage ::
      Maybe (VerKeyKES (KES c)) ->
      Message (ControlProtocol m c) WaitForPublicKeyState IdleState
    InstallKeyMessage ::
      OCert c ->
      Message (ControlProtocol m c) IdleState WaitForConfirmationState
    InstallResultMessage ::
      RecvResult ->
      Message (ControlProtocol m c) WaitForConfirmationState IdleState
    RequestInfoMessage :: Message (ControlProtocol m c) IdleState WaitForInfoState
    InfoMessage ::
      AgentInfo c ->
      Message (ControlProtocol m c) WaitForInfoState IdleState
    AbortMessage :: Message (ControlProtocol m c) InitialState EndState
    EndMessage :: Message (ControlProtocol m c) IdleState EndState
    ProtocolErrorMessage :: Message (ControlProtocol m c) a EndState

  type StateAgency InitialState = ServerAgency
  type StateAgency IdleState = ServerAgency

  type StateAgency WaitForConfirmationState = ClientAgency
  type StateAgency WaitForPublicKeyState = ClientAgency
  type StateAgency WaitForInfoState = ClientAgency
  type StateAgency EndState = NobodyAgency

  type StateToken = SControlProtocol

data SControlProtocol (st :: ControlProtocol m c) where
  SInitialState :: SControlProtocol InitialState
  SIdleState :: SControlProtocol IdleState
  SWaitForConfirmationState :: SControlProtocol WaitForConfirmationState
  SWaitForPublicKeyState :: SControlProtocol WaitForPublicKeyState
  SWaitForInfoState :: SControlProtocol WaitForInfoState
  SEndState :: SControlProtocol EndState

instance StateTokenI InitialState where stateToken = SInitialState
instance StateTokenI IdleState where stateToken = SIdleState
instance StateTokenI WaitForConfirmationState where stateToken = SWaitForConfirmationState
instance StateTokenI WaitForPublicKeyState where stateToken = SWaitForPublicKeyState
instance StateTokenI WaitForInfoState where stateToken = SWaitForInfoState
instance StateTokenI EndState where stateToken = SEndState

instance NamedCrypto c => VersionedProtocol (ControlProtocol m c) where
  versionIdentifier = cpVersionIdentifier

cpVersionIdentifier ::
  forall m c. NamedCrypto c => Proxy (ControlProtocol m c) -> VersionIdentifier
cpVersionIdentifier _ =
  mkVersionIdentifier $
    "Control:" <> unCryptoName (cryptoName (Proxy @c)) <> ":0.5"
