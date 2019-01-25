{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.PermBFT (
  ) where

import           Data.Void
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Util.Condense

data PermBft c

instance PermBftCrypto c => OuroborosTag (PermBft c) where
  -- | The BFT payload is just the signature
  newtype Payload (PermBft c) ph = PermBftPayload {
        something :: SignedDSIGN (PermBftDSIGN c) ph
      }
    deriving (Generic)

  data NodeConfig (PermBft c) = PermBftNodeConfig {
      unNodeConfig :: Void
    }

  type ValidationErr  (PermBft c) = PermBftValidationErr
  type SupportedBlock (PermBft c) = HasPayload (PermBft c)
  type NodeState      (PermBft c) = ()
  type LedgerView     (PermBft c) = ()
  type IsLeader       (PermBft c) = ()
  type ChainState     (PermBft c) = ()

deriving instance PermBftCrypto c => Show     (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Eq       (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Ord      (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Condense (Payload (PermBft c) ph)

instance PermBftCrypto c => Serialise (Payload (PermBft c) ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data PermBftValidationErr = PermBftInvalidSignature
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class DSIGNAlgorithm (PermBftDSIGN c) => PermBftCrypto c where
  type family PermBftDSIGN c :: *

data PermBftStandardCrypto
data PermBftMockCrypto

instance PermBftCrypto PermBftStandardCrypto where
  type PermBftDSIGN PermBftStandardCrypto = Ed448DSIGN

instance PermBftCrypto PermBftMockCrypto where
  type PermBftDSIGN PermBftMockCrypto = MockDSIGN
