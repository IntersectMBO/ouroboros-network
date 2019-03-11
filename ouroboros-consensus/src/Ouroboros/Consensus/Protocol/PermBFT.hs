{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

module Ouroboros.Consensus.Protocol.PermBFT (
    PermBft
    -- * Classes
  , PermBftCrypto(..)
  , PermBftStandardCrypto
  , PermBftMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise)
import           Control.Lens ((^.))
import           Control.Monad.Trans.Except (except)
import           Data.Map.Strict (Map)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Protocol.Abstract

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId, NumCoreNodes(..))
import           Ouroboros.Consensus.Util.Condense

import           Control.State.Transition (applySTS, Environment, PredicateFailure, State, IRC(IRC), TRC(TRC))
import           Ledger.Core (SlotCount, Slot(Slot))
import           Ledger.Delegation (DIEnv, allowedDelegators, slot)
import           Ledger.Update (PParams)
import qualified Cardano.Spec.Chain.STS.Block as CBM -- concrete block module
import           Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import           Cardano.Spec.Consensus.Block


data PermBft c

instance PermBftCrypto c => OuroborosTag (PermBft c) where
  -- | The BFT payload is just the signature
  newtype Payload (PermBft c) ph = PermBftPayload {
        permBftSignature :: SignedDSIGN (PermBftDSIGN c) ph
      }
    deriving (Generic)

  data NodeConfig (PermBft c) = PermBftNodeConfig
    { permBftNodeId       :: NodeId
    , permBftSignKey      :: SignKeyDSIGN (PermBftDSIGN c)
    , permBftNumCoreNodes :: NumCoreNodes
    , permBftVerKeys      :: Map NodeId (VerKeyDSIGN (PermBftDSIGN c))
    , permBftProtParams   :: PParams
    , permBftKSize        :: SlotCount
    }

  type ValidationErr  (PermBft c) = [PredicateFailure CHAIN]
  type SupportedBlock (PermBft c) = Block
  type NodeState      (PermBft c) = ()
  type LedgerView     (PermBft c) = DIEnv
  type IsLeader       (PermBft c) = ()
  type ChainState     (PermBft c) = State CHAIN

  mkPayload PermBftNodeConfig{..} _ preheader = do
    signature <- signedDSIGN preheader permBftSignKey
    return $ PermBftPayload { permBftSignature = signature }

  checkIsLeader _ _ _ _ = return $ Just ()

  applyChainState PermBftNodeConfig{..} lv b chainSt =
    except $
      if lv ^. slot == Slot 0 -- the genesis block
        then applySTS $ IRC bcEnv
        else applySTS $ TRC (bcEnv, chainSt, concretiseBlock b)
   where
    bcEnv :: Environment CHAIN
    bcEnv =
      ( lv ^. slot
      , lv ^. allowedDelegators
      , permBftProtParams
      )


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
