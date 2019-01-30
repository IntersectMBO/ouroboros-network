{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.PermBFT (
  ) where

import           Control.Monad.Trans.Except (except)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Network.Serialise

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Util.Condense

import           Chain.Blockchain (BlockchainEnv(..), KeyToQMap, T)
import           Chain.GenesisBlock (genesisBlock)
import           Control.State.Transition (applySTS, PredicateFailure, Signal, State, IRC(IRC), TRC(TRC))
import           Ledger.Core (SlotCount(SlotCount))
import           Ledger.Delegation (DIEnv)
import           Types (BC, Block(GBlock, RBlock), ProtParams(..))


data PermBft c

instance PermBftCrypto c => OuroborosTag (PermBft c) where
  -- | The BFT payload is just the signature
  newtype Payload (PermBft c) ph = PermBftPayload {
        permBftSignature :: SignedDSIGN (PermBftDSIGN c) ph
      }
    deriving (Generic)

  data NodeConfig (PermBft c) = PermBftNodeConfig
    { permBftSignKey :: SignKeyDSIGN (PermBftDSIGN c)
    , protParams     :: ProtParams
    , kSize          :: SlotCount
    , tRatio         :: T
    }

  type ValidationErr  (PermBft c) = [PredicateFailure BC]
  type SupportedBlock (PermBft c) = HasPayload (PermBft c)
  type NodeState      (PermBft c) = ()
  type LedgerView     (PermBft c) = DIEnv
  type IsLeader       (PermBft c) = ()
  type ChainState     (PermBft c) = State BC

  mkPayload PermBftNodeConfig{..} _ preheader = do
    signature <- signedDSIGN preheader permBftSignKey
    return $ PermBftPayload { permBftSignature = signature }

  checkIsLeader _ _ _ _ = return Nothing

  applyChainState PermBftNodeConfig{..} lv b chainSt@(_, prevBlock, _) =
    except $
      if prevBlock == genesisBlock
        then applySTS $ IRC bcEnv
        else applySTS $ TRC (bcEnv, chainSt, b)
   where
    bcEnv :: BlockchainEnv
    bcEnv = MkBlockChainEnv
      { bcEnvPp    = protParams
      , bcEnvDIEnv = lv
      , bcEnvK     = kSize
      , bcEnvT     = tRatio
      }


deriving instance PermBftCrypto c => Show     (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Eq       (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Ord      (Payload (PermBft c) ph)
deriving instance PermBftCrypto c => Condense (Payload (PermBft c) ph)

instance PermBftCrypto c => Serialise (Payload (PermBft c) ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  Permissive BFT specific types
-------------------------------------------------------------------------------}

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
