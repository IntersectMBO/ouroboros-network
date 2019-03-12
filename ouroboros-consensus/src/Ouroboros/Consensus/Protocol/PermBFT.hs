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


data PermBft

instance PermBftCrypto c => OuroborosTag PermBft where
  -- | The BFT payload is just the signature
  newtype Payload PermBft ph = PermBftPayload {
        permBftSignature :: SignedDSIGN (PermBftDSIGN c) ph
      }
    deriving (Generic)

  data NodeConfig PermBft = PermBftNodeConfig
    { permBftNodeId       :: NodeId
    , permBftSignKey      :: SignKeyDSIGN (PermBftDSIGN c)
    , permBftNumCoreNodes :: NumCoreNodes
    , permBftVerKeys      :: Map NodeId (VerKeyDSIGN (PermBftDSIGN c))
    , permBftProtParams   :: PParams
    , permBftKSize        :: SlotCount
    }

  type ValidationErr  PermBft = [PredicateFailure CHAIN]
  type SupportedBlock PermBft = Block
  type NodeState      PermBft = ()
  type LedgerView     PermBft = DIEnv
  type IsLeader       PermBft = ()
  type ChainState     PermBft = State CHAIN

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


instance Serialise (Payload PermBft ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  Permissive BFT specific types
-------------------------------------------------------------------------------}
