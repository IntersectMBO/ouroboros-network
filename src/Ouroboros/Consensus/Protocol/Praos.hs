{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Ouroboros.Consensus.Protocol.Praos (
    -- * Tags
    Praos
  , PraosStandardCrypto
  , PraosMockCrypto
    -- * Classes
  , PraosLedgerView(..)
  , PraosCrypto(..)
  ) where

import           GHC.Generics (Generic)
import           Numeric.Natural

import           Ouroboros.Consensus.Crypto.DSIGN.Class
import           Ouroboros.Consensus.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Ouroboros.Consensus.Crypto.DSIGN.Mock (MockDSIGN)
import           Ouroboros.Consensus.Crypto.VRF.Class
import           Ouroboros.Consensus.Crypto.VRF.Mock (MockVRF)
import           Ouroboros.Consensus.Crypto.VRF.Simple (SimpleVRF)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.HList (HList)
import           Ouroboros.Network.Block (Slot)
import           Ouroboros.Network.Node (NodeId (..))
import           Ouroboros.Network.Serialise (Serialise)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

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

instance PraosCrypto c => OuroborosTag (Praos c) where
  data OuroborosPayload (Praos c) ph = PraosPayload {
        praosSignature   :: Signed (PraosDSIGN c) (ph, PraosExtraFields c)
      , praosExtraFields :: PraosExtraFields c
      }
    deriving (Generic)

  data OuroborosState (Praos c) = PraosState {
        praosNodeId       :: Int
      , praosSignKeyDSIGN :: SignKeyDSIGN (PraosDSIGN c) -- TODO: should be KES
      , praosSignKeyVRF   :: SignKeyVRF (PraosVRF c)
      }

  data ProofIsLeader (Praos c) = PraosProof {
        praosProofRho :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
      , praosProofY   :: CertifiedVRF (PraosVRF c) (HList [Natural, Slot, VRFType])
      , praosLeaderId :: Int
      }

  -- This is a placeholder for now (Lars).
  data OuroborosLedgerState (Praos c) = PraosLedgerState deriving Show

  mkOuroborosPayload PraosProof{..} preheader = do
      PraosState{..} <- getOuroborosState
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

  applyOuroborosLedgerState _ _ = PraosLedgerState

deriving instance (PraosCrypto c, Show ph) => Show (OuroborosPayload (Praos c) ph)
deriving instance (PraosCrypto c, Eq   ph) => Eq   (OuroborosPayload (Praos c) ph)

instance ( PraosCrypto c
         , Condense ph
         )
         => Condense (OuroborosPayload (Praos c) ph) where
    condense (PraosPayload sig _) = condense sig

instance (PraosCrypto c, Serialise ph) => Serialise (OuroborosPayload (Praos c) ph) where
  -- use generic instance

instance (PraosCrypto c, PraosLedgerView l) => RunOuroboros (Praos c) l where
    checkIsLeader slot l = do
        PraosState{..} <- getOuroborosState
        let (rho', y', t) = praosRhoYT l slot (CoreId praosNodeId)
        rho <- evalCertified rho' praosSignKeyVRF
        y   <- evalCertified y'   praosSignKeyVRF
        return $ if fromIntegral (certifiedNatural y) < t
            then Just PraosProof {
                     praosProofRho = rho
                   , praosProofY   = y
                   , praosLeaderId = praosNodeId
                   }
            else Nothing

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic)

instance Serialise VRFType
  -- use generic instance

{-------------------------------------------------------------------------------
  Ledger view
-------------------------------------------------------------------------------}

class PraosLedgerView l where
  praosRhoYT :: l
             -> Slot
             -> NodeId
             -> ( HList '[Natural, Slot, VRFType]
                , HList '[Natural, Slot, VRFType]
                , Double
                )

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( DSIGNAlgorithm (PraosDSIGN c)
      , VRFAlgorithm   (PraosVRF   c)
      ) => PraosCrypto (c :: *) where
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
