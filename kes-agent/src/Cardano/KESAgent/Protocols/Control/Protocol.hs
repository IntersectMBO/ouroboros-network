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
import Data.SerDoc.Info ( Description (..) )
import Data.SerDoc.Class ( ViaEnum (..) )

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
    { bootstrapInfoAddress :: !Text
    , bootstrapInfoStatus :: !ConnectionStatus
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

instance Protocol (ControlProtocol m c) where
  data Message (ControlProtocol m c) st st' where

          VersionMessage :: Message (ControlProtocol m c) InitialState IdleState

          GenStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          QueryStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          DropStagedKeyMessage :: Message (ControlProtocol m c) IdleState WaitForPublicKeyState

          PublicKeyMessage :: Maybe (VerKeyKES (KES c))
                           -> Message (ControlProtocol m c) WaitForPublicKeyState IdleState

          InstallKeyMessage :: OCert c
                            -> Message (ControlProtocol m c) IdleState WaitForConfirmationState

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
