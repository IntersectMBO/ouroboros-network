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
{-# LANGUAGE TypeFamilies #-}
module Cardano.KESAgent.Protocol
  where

import Cardano.KESAgent.OCert
import Cardano.KESAgent.RefCounting

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
import Data.ByteString qualified as BS
import Data.Proxy ( Proxy (..) )
import Data.Typeable
import Data.Word
import GHC.Generics ( Generic )
import Network.TypedProtocol.Core
import NoThunks.Class ( NoThunks (..) )
import Quiet

data KESProtocol (m :: * -> *) (k :: *) where
  -- | Default state after connecting, but before the protocol version has been
  -- negotiated.
  InitialState :: KESProtocol m k

  -- | System is idling, waiting for the server to push the next key.
  IdleState :: KESProtocol m k

  -- | A new key has been pushed, client must now confirm that the key has been
  -- received.
  WaitForConfirmationState :: KESProtocol m k

  -- | The server has closed the connection, thus signalling the end of the
  -- session.
  EndState :: KESProtocol m k

class VersionedProtocol (p :: *) where
  versionIdentifier :: Proxy p -> VersionIdentifier

newtype VersionIdentifier =
  VersionIdentifier { unVersionIdentifier :: ByteString }
  deriving newtype (Show, Eq)

data StandardCrypto

data SingleCrypto

data MockCrypto

instance Crypto StandardCrypto where
  type KES StandardCrypto = Sum6KES Ed25519DSIGN Blake2b_256
  type DSIGN StandardCrypto = Ed25519DSIGN

instance Crypto SingleCrypto where
  type KES SingleCrypto = SingleKES Ed25519DSIGN
  type DSIGN SingleCrypto = Ed25519DSIGN

instance Crypto MockCrypto where
  type KES MockCrypto = MockKES 128
  type DSIGN MockCrypto = Ed25519DSIGN

versionIdentifierLength :: Num a => a
versionIdentifierLength = 32

mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

instance VersionedProtocol (KESProtocol m StandardCrypto) where
  versionIdentifier _ =
    mkVersionIdentifier "StandardCrypto:0.3"

instance VersionedProtocol (KESProtocol m SingleCrypto) where
  versionIdentifier _ =
    mkVersionIdentifier "SingleCrypto:0.3"

instance VersionedProtocol (KESProtocol m MockCrypto) where
  versionIdentifier _ =
    mkVersionIdentifier "MockCrypto:0.3"

data RecvResult
  = RecvOK
  | RecvErrorKeyOutdated -- ^ Newer key is already present
  | RecvErrorInvalidOpCert -- ^ OpCert did not validate
  | RecvErrorUnknown -- ^ Something else went wrong, we don't know what
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Node acts as the Client, the Agent acts as a Server
-- - When a Node connects, the Agent will push the current key
-- - When the Agent generates a new key, it will push the new key
--
-- **OR:**
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
instance Protocol (KESProtocol m c) where
  data Message (KESProtocol m c) st st' where
          VersionMessage :: Message (KESProtocol m c) InitialState IdleState
          KeyMessage :: CRef m (SignKeyWithPeriodKES (KES c))
                     -> OCert c
                     -> Message (KESProtocol m c) IdleState WaitForConfirmationState
          RecvResultMessage :: RecvResult
                            -> Message (KESProtocol m c) WaitForConfirmationState IdleState
          EndMessage :: Message (KESProtocol m c) IdleState EndState

  -- | Server always has agency, except between sending a key and confirming it
  data ServerHasAgency st where
    TokInitial :: ServerHasAgency InitialState
    TokIdle :: ServerHasAgency IdleState

  -- | Client only has agency between sending a key and confirming it
  data ClientHasAgency st where
    TokWaitForConfirmation :: ClientHasAgency WaitForConfirmationState

  -- | Someone, i.e., the server, always has agency
  data NobodyHasAgency st where
    TokEnd :: NobodyHasAgency EndState

  exclusionLemma_ClientAndServerHaveAgency tok1 tok2 =
    case tok1 of
      TokWaitForConfirmation ->
        case tok2 of {}
  exclusionLemma_NobodyAndClientHaveAgency _ _ = undefined
  exclusionLemma_NobodyAndServerHaveAgency _ _ = undefined
