{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Ouroboros.Consensus.Protocol.PBFT.Crypto (
    PBftCrypto(..)
  , PBftMockCrypto
  , PBftCardanoCrypto
  ) where

import           Data.Typeable

import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Util.Condense

-- | Crypto primitives required by BFT
--
-- Cardano stores a map of stakeholder IDs rather than the verification key
-- directly. We make this family injective for convenience - whilst it's
-- _possible_ that there could be non-injective instances, the chances of there
-- being more than the two instances here are basically non-existent.
class ( Typeable c
      , DSIGNAlgorithm (PBftDSIGN c)
      , Condense (SigDSIGN (PBftDSIGN c))
      , Show (PBftVerKeyHash c)
      , Ord  (PBftVerKeyHash c)
      , Eq   (PBftVerKeyHash c)
      , Show (PBftVerKeyHash c)
      , NoUnexpectedThunks (PBftVerKeyHash c)
      , NoUnexpectedThunks (PBftDelegationCert c)
      ) => PBftCrypto c where
  type family PBftDSIGN          c :: *
  type family PBftDelegationCert c = (d :: *) | d -> c
  type family PBftVerKeyHash     c = (d :: *) | d -> c

  dlgCertGenVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)
  dlgCertDlgVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)
  hashVerKey       :: VerKeyDSIGN (PBftDSIGN c) -> PBftVerKeyHash c

data PBftMockCrypto

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN          PBftMockCrypto = MockDSIGN
  type PBftDelegationCert PBftMockCrypto = (VerKeyDSIGN MockDSIGN, VerKeyDSIGN MockDSIGN)
  type PBftVerKeyHash     PBftMockCrypto = VerKeyDSIGN MockDSIGN

  dlgCertGenVerKey = fst
  dlgCertDlgVerKey = snd
  hashVerKey       = id

data PBftCardanoCrypto

instance PBftCrypto PBftCardanoCrypto where
  type PBftDSIGN          PBftCardanoCrypto = CardanoDSIGN
  type PBftDelegationCert PBftCardanoCrypto = CC.Delegation.Certificate
  type PBftVerKeyHash     PBftCardanoCrypto = CC.Common.KeyHash

  dlgCertGenVerKey = VerKeyCardanoDSIGN . CC.Delegation.issuerVK
  dlgCertDlgVerKey = VerKeyCardanoDSIGN . CC.Delegation.delegateVK
  hashVerKey (VerKeyCardanoDSIGN pk) = CC.Common.hashKey pk
