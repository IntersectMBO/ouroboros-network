{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


module Ouroboros.Consensus.Protocol.PermBFT (
    PermBft
    -- * Classes
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           Codec.Serialise (Serialise)
import           Control.Lens ((^.))
import           Control.Monad.Trans.Except (except)
import           Data.Map.Strict (Map)
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Tags (PermBft)

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Node (NodeId, NumCoreNodes(..))
import           Ouroboros.Consensus.Util.Condense

import           Control.State.Transition (applySTS, Environment, PredicateFailure, State, IRC(IRC), TRC(TRC))
import           Ledger.Core (SlotCount, Slot(Slot), SKey, VKey, Sig, sign, VKeyGenesis, Epoch)
import           Ledger.Delegation (DIEnv, allowedDelegators, slot)
import           Ledger.Update (PParams)
import qualified Cardano.Spec.Chain.STS.Block as CBM -- concrete block module
import           Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import           Cardano.Spec.Consensus.Block


instance ( Show (ChainState PermBft)
         , Show (ValidationErr PermBft) ) => OuroborosTag PermBft where
  -- | The BFT payload is just the signature
  newtype Payload PermBft ph = PermBftPayload {
        permBftSignature :: Sig ph
      }
    deriving (Generic)

  data NodeConfig PermBft = PermBftNodeConfig
    { permBftNodeId       :: NodeId
    , permBftSignKey      :: SKey
    , permBftNumCoreNodes :: NumCoreNodes
    , permBftVerKeys      :: Map NodeId VKey
    , permBftProtParams   :: PParams
    , permBftKSize        :: SlotCount
    , permBftGenesisKeys  :: Set VKeyGenesis
    , permBftEpoch        :: Epoch
    , permBftSlot         :: Slot
    , permBftLiveness     :: SlotCount
    }

  type ValidationErr  PermBft = [PredicateFailure CHAIN]
  type SupportedBlock PermBft = Block
  type NodeState      PermBft = ()
  type LedgerView     PermBft = DIEnv
  type IsLeader       PermBft = ()
  type ChainState     PermBft = State CHAIN

  mkPayload PermBftNodeConfig{..} _ preheader = do
    let signature = sign permBftSignKey preheader
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


deriving instance Show ph           => Show     (Payload PermBft ph)
deriving instance Eq ph             => Eq       (Payload PermBft ph)
deriving instance Ord ph            => Ord      (Payload PermBft ph)
deriving instance Condense (Sig ph) => Condense (Payload PermBft ph)

instance Serialise (Sig ph) => Serialise (Payload PermBft ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  Permissive BFT specific types
-------------------------------------------------------------------------------}

-- {-------------------------------------------------------------------------------
--   Crypto models
-- -------------------------------------------------------------------------------}

-- -- | Crypto primitives required by BFT
-- class DSIGNAlgorithm (PermBftDSIGN c) => PermBftCrypto c where
--   type family PermBftDSIGN c :: *

-- data PermBftStandardCrypto
-- data PermBftMockCrypto

-- instance PermBftCrypto PermBftStandardCrypto where
--   type PermBftDSIGN PermBftStandardCrypto = Ed448DSIGN

-- instance PermBftCrypto PermBftMockCrypto where
--   type PermBftDSIGN PermBftMockCrypto = MockDSIGN
