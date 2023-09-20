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

module Cardano.KESAgent.Protocols.Control.Protocol
  where

import Cardano.KESAgent.KES.Crypto
import Cardano.KESAgent.KES.OCert
import Cardano.KESAgent.Protocols.VersionedProtocol
import Cardano.KESAgent.Protocols.RecvResult
import Cardano.KESAgent.Serialization.Spec
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

data AgentInfo c =
  AgentInfo
    { agentInfoCurrentBundle :: !(Maybe (BundleInfo c))
    , agentInfoStagedKey :: !(Maybe (KeyInfo c))
    , agentInfoCurrentTime :: !UTCTime
    , agentInfoCurrentKESPeriod :: !KESPeriod
    , agentInfoBootstrapConnections :: ![BootstrapInfo]
    }

deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) => Show (AgentInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) => Eq (AgentInfo c)

data BootstrapInfo =
  BootstrapInfo
    { bootstrapAddress :: !Text
    , bootstrapStatus :: !ConnectionStatus
    }
    deriving (Show, Eq)

data ConnectionStatus
  = ConnectionUp
  | ConnectionConnecting
  | ConnectionDown
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

data BundleInfo c =
  BundleInfo
    { bundleInfoEvolution :: !Word32
    , bundleInfoStartKESPeriod :: !KESPeriod
    , bundleInfoOCertN :: !Word64
    , bundleInfoVK :: !(VerKeyKES (KES c))
    , bundleInfoSigma :: !(SignedDSIGN (DSIGN c) (OCertSignable c))
    }

deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) => Show (BundleInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) => Eq (BundleInfo c)

newtype KeyInfo c =
  KeyInfo
    { keyInfoVK :: VerKeyKES (KES c)
    }

deriving instance
  ( KESAlgorithm (KES c)
  ) => Show (KeyInfo c)
deriving instance
  ( DSIGNAlgorithm (DSIGN c)
  , KESAlgorithm (KES c)
  ) => Eq (KeyInfo c)

data ControlProtocol (m :: * -> *) (k :: *) where
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
--
instance Protocol (ControlProtocol m c) where
  data Message (ControlProtocol m c) st st' where
          VersionMessage :: Message (ControlProtocol m c) InitialState IdleState

          -- | Request the agent to generate a fresh KES sign key and store it
          -- in the staging area.
          GenStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          -- | Query the agent for the key currently stored in the staging area.
          QueryStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          -- | Request that the agent erase the key currently stored in the
          -- staging area.
          DropStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          -- | Respond to a request for a staged key. Only the public key (vkey)
          -- will be returned however.
          PublicKeyMessage :: Maybe (VerKeyKES (KES c))
                           -> Message (ControlProtocol m c) WaitForPublicKeyState IdleState

          -- | Upload an OpCert, and request that the agent bundle it with the
          -- key in the staging are and install it.
          InstallKeyMessage :: OCert c
                            -> Message (ControlProtocol m c) IdleState WaitForConfirmationState

          -- | Report the result of installing an OpCert + KES key back from the
          -- agent.
          InstallResultMessage :: RecvResult
                               -> Message (ControlProtocol m c) WaitForConfirmationState IdleState

          RequestInfoMessage :: Message (ControlProtocol m c) IdleState WaitForInfoState

          InfoMessage :: AgentInfo c
                      -> Message (ControlProtocol m c) WaitForInfoState IdleState

          AbortMessage :: Message (ControlProtocol m c) InitialState EndState
          EndMessage :: Message (ControlProtocol m c) IdleState EndState
          ProtocolErrorMessage :: Message (ControlProtocol m c) a EndState

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

instance NamedCrypto c => VersionedProtocol (ControlProtocol m c) where
  versionIdentifier = cpVersionIdentifier

cpVersionIdentifier :: forall m c. NamedCrypto c => Proxy (ControlProtocol m c) -> VersionIdentifier
cpVersionIdentifier _ =
  mkVersionIdentifier $
    "Control:" <> unCryptoName (cryptoName (Proxy @c)) <> ":0.5"
