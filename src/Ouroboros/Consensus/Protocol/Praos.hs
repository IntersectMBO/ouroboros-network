{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE EmptyDataDeriving    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Protocol.Praos (
    Praos
    -- * Tags
  , PraosCrypto(..)
  , PraosStandardCrypto
  , PraosMockCrypto
    -- * Type instances
  , NodeConfig(..)
  , Payload(..)
  ) where

import           GHC.Generics (Generic)
import           Numeric.Natural

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Crypto.VRF.Mock (MockVRF)
import           Ouroboros.Consensus.Crypto.VRF.Simple (SimpleVRF)
import           Ouroboros.Consensus.Node (NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.HList (HList)
import           Ouroboros.Network.Block (Slot)
import           Ouroboros.Network.Serialise (Serialise)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

instance PraosCrypto c => OuroborosTag (Praos c) where
  data Payload (Praos c) ph = PraosPayload {
        praosSignature   :: Signed (PraosDSIGN c) (ph, PraosExtraFields c)
      , praosExtraFields :: PraosExtraFields c
      }
    deriving (Generic)

  data NodeConfig (Praos c) = PraosNodeConfig
      -- just a placeholder for now

  type NodeState      (Praos c) = PraosNodeState c
  type LedgerView     (Praos c) = PraosLedgerView c
  type IsLeader       (Praos c) = PraosProof c
  type ValidationErr  (Praos c) = PraosValidationError
  type SupportedBlock (Praos c) = HasPayload (Praos c)
  type ChainState     (Praos c) = ()

  mkPayload PraosNodeConfig PraosProof{..} preheader = do
      PraosNodeState{..} <- getNodeState
      let extraFields = PraosExtraFields {
            praosCreator = praosNodeId
          , praosRho     = praosProofRho
          , praosY       = praosProofY
          }
      signature <- signed (preheader, extraFields) praosSignKeyDSIGN
      return $ PraosPayload {
          praosSignature   = signature
        , praosExtraFields = extraFields
        }

  checkIsLeader PraosNodeConfig slot PraosLedgerView{..} _cs = do
      PraosNodeState{..} <- getNodeState
      let (rho', y', t) = praosRhoYT slot (CoreId praosNodeId)
      rho <- evalCertified rho' praosSignKeyVRF
      y   <- evalCertified y'   praosSignKeyVRF
      return $ if fromIntegral (certifiedNatural y) < t
          then Just PraosProof {
                   praosProofRho = rho
                 , praosProofY   = y
                 , praosLeaderId = praosNodeId
                 }
          else Nothing

  applyChainState PraosNodeConfig PraosLedgerView{..} _b _cs =
    return () -- TODO (Lars)

deriving instance (PraosCrypto c, Show ph) => Show (Payload (Praos c) ph)
deriving instance (PraosCrypto c, Eq   ph) => Eq   (Payload (Praos c) ph)

instance PraosCrypto c => Condense (Payload (Praos c) ph) where
    condense (PraosPayload sig _) = condense sig

instance (PraosCrypto c, Serialise ph) => Serialise (Payload (Praos c) ph) where
  -- use generic instance

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data PraosExtraFields c = PraosExtraFields {
      praosCreator :: Int
    , praosRho     :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
    , praosY       :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
    }
  deriving (Generic)

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)

instance VRFAlgorithm (PraosVRF c) => Serialise (PraosExtraFields c)
  -- use Generic instance for now

data PraosNodeState c = PraosNodeState {
      praosNodeId       :: Int
    , praosSignKeyDSIGN :: SignKeyDSIGN (PraosDSIGN c) -- TODO: should be KES
    , praosSignKeyVRF   :: SignKeyVRF (PraosVRF c)
    }

data PraosProof c = PraosProof {
      praosProofRho :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
    , praosProofY   :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
    , praosLeaderId :: Int
    }

data PraosLedgerView c = PraosLedgerView {
    praosRhoYT :: Slot
               -> NodeId
               -> ( HList '[Natural, Slot, VRFType]
                  , HList '[Natural, Slot, VRFType]
                  , Double
                  )
  }

data PraosValidationError
  deriving (Show)

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic)

instance Serialise VRFType
  -- use generic instance


{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( DSIGNAlgorithm (PraosDSIGN c)
      , VRFAlgorithm   (PraosVRF   c)
      ) => PraosCrypto c where
  type family PraosDSIGN c :: * --  TODO: Should be KES
  type family PraosVRF   c :: *

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosDSIGN PraosStandardCrypto = Ed448DSIGN
  type PraosVRF   PraosStandardCrypto = SimpleVRF

instance PraosCrypto PraosMockCrypto where
  type PraosDSIGN PraosMockCrypto = MockDSIGN
  type PraosVRF   PraosMockCrypto = MockVRF
