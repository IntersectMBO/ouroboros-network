{-#LANGUAGE GADTs #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE DataKinds #-}
{-#LANGUAGE EmptyCase #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE PolyKinds #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DerivingStrategies #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE DerivingVia #-}
{-#LANGUAGE DeriveAnyClass #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE TypeApplications #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Cardano.KESAgent.Protocol
where

import Cardano.KESAgent.OCert

import Cardano.Crypto.KES.Class
import Cardano.Crypto.KES.Sum
import Cardano.Crypto.KES.Mock
import Cardano.Crypto.DSIGN.Class
import Cardano.Crypto.DSIGN.Ed25519ML
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Crypto.Hash.Blake2b
import Cardano.Crypto.Util (SignableRepresentation (..))
import Cardano.Binary

import Network.TypedProtocol.Core
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Data.Word
import NoThunks.Class (NoThunks (..))
import Quiet
import Data.Typeable

data KESProtocol (k :: *) where
  InitialState :: KESProtocol k
  IdleState :: KESProtocol k
  EndState :: KESProtocol k

class VersionedProtocol (p :: *) where
  versionIdentifier :: Proxy p -> VersionIdentifier

data VersionIdentifier =
  VersionIdentifier ByteString
  deriving (Show, Eq)

data StandardCrypto

data MockCrypto

instance Crypto StandardCrypto where
  type KES StandardCrypto = Sum6KES Ed25519DSIGNM Blake2b_256
  type DSIGN StandardCrypto = Ed25519DSIGN

instance Crypto MockCrypto where
  type KES MockCrypto = MockKES 128
  type DSIGN MockCrypto = Ed25519DSIGN

versionIdentifierLength :: Num a => a
versionIdentifierLength = 32

mkVersionIdentifier :: ByteString -> VersionIdentifier
mkVersionIdentifier raw =
  VersionIdentifier $ BS.take versionIdentifierLength $ raw <> BS.replicate versionIdentifierLength 0

instance VersionedProtocol (KESProtocol StandardCrypto) where
  versionIdentifier _ =
    mkVersionIdentifier "StandardCrypto:0.1"

instance VersionedProtocol (KESProtocol MockCrypto) where
  versionIdentifier _ =
    mkVersionIdentifier "MockCrypto:0.1"

-- | The protocol for pushing KES keys.
--
-- Intended use:
--
-- - The Node acts as the Client, the Agent acts as a Server
-- - When a Node connects, the Agent will push the current key
-- - When the Agent generates a new key, it will push the new key
--
-- Hence, the Agent always has agency, and there is only one protocol state,
-- the 'IdleState'. From there, the Agent can always push keys, and the Node
-- will always accept new keys.
--
-- **OR:**
--
-- - The Agent acts as the Client, and the Control Server as a Server
-- - When the Control Server connects, it pushes a key to the Agent
-- - The Agent stores the key locally in memory and pushes it to any connected
--   Nodes.
--
instance Protocol (KESProtocol c) where
  data Message (KESProtocol c) st st' where
          VersionMessage :: Message (KESProtocol c) InitialState IdleState
          KeyMessage :: SignKeyWithPeriodKES (KES c)
                     -> OCert c
                     -> Message (KESProtocol c) IdleState IdleState
          EndMessage :: Message (KESProtocol c) IdleState EndState

  -- | Server always has agency
  data ServerHasAgency st where
    TokInitial :: ServerHasAgency InitialState
    TokIdle :: ServerHasAgency IdleState

  -- | Client never has agency
  data ClientHasAgency st where

  -- | Someone, i.e., the server, always has agency
  data NobodyHasAgency st where
    TokEnd :: NobodyHasAgency EndState

  exclusionLemma_ClientAndServerHaveAgency tok _ = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency _ _ = undefined
  exclusionLemma_NobodyAndServerHaveAgency _ _ = undefined
