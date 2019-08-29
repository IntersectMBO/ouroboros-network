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

import           Data.Constraint
import           Data.Reflection (Given (..), give)
import           Data.Typeable

import           Cardano.Binary (Decoded)
import qualified Cardano.Chain.Common as CC.Common
import qualified Cardano.Chain.Delegation as CC.Delegation
import           Cardano.Crypto (ProtocolMagicId)
import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Crypto.DSIGN.Cardano
import           Ouroboros.Consensus.Ledger.Byron.Orphans ()
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

-- | Crypto primitives required by BFT
class ( Typeable c
      , DSIGNAlgorithm (PBftDSIGN c)
      , Condense (SigDSIGN (PBftDSIGN c))
      , Show (PBftVerKeyHash c)
      , Ord (PBftVerKeyHash c)
      , Eq (PBftVerKeyHash c)
      , Show (PBftVerKeyHash c)
      , NoUnexpectedThunks (PBftVerKeyHash c)
      ) => PBftCrypto c where
  type family PBftDSIGN c :: *

  type family PBftDelegationCert c = (d :: *) | d -> c

  dlgCertGenVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)
  dlgCertDlgVerKey :: PBftDelegationCert c -> VerKeyDSIGN (PBftDSIGN c)

  -- Cardano stores a map of stakeholder IDs rather than the verification key
  -- directly. We make this family injective for convenience - whilst it's
  -- _possible_ that there could be non-injective instances, the chances of there
  -- being more than the two instances here are basically non-existent.
  type family PBftVerKeyHash c = (d :: *) | d -> c

  type family PBftSigningConstraints c hdr :: Constraint

  hashVerKey :: VerKeyDSIGN (PBftDSIGN c) -> PBftVerKeyHash c

  -- Abstracted version of `verifySignedDSIGN`
  --
  -- Since our signing constraints differ, we abstract this here such that we can
  -- correctly assemble the constraints in the real crypto case. See the
  -- documentation in Crypto/DSIGN/Cardano for more details.
  verifyPBftSigned :: forall hdr proxy. (PBftSigningConstraints c hdr)
                   => proxy (c, hdr)
                   -> VerKeyDSIGN (PBftDSIGN c) -- Genesis key - only used in the real impl
                   -> VerKeyDSIGN (PBftDSIGN c)
                   -> Signed hdr
                   -> SignedDSIGN (PBftDSIGN c) (Signed hdr) -> Either String ()

data PBftMockCrypto

instance PBftCrypto PBftMockCrypto where
  type PBftDSIGN      PBftMockCrypto = MockDSIGN

  type PBftDelegationCert PBftMockCrypto = (VerKeyDSIGN MockDSIGN, VerKeyDSIGN MockDSIGN)

  dlgCertGenVerKey = fst
  dlgCertDlgVerKey = snd

  type PBftVerKeyHash PBftMockCrypto = VerKeyDSIGN MockDSIGN

  type PBftSigningConstraints PBftMockCrypto hdr = Signable MockDSIGN (Signed hdr)

  hashVerKey = id

  verifyPBftSigned _ _ = verifySignedDSIGN

data PBftCardanoCrypto

instance (Given ProtocolMagicId) => PBftCrypto PBftCardanoCrypto where
  type PBftDSIGN PBftCardanoCrypto      = CardanoDSIGN

  type PBftDelegationCert PBftCardanoCrypto = CC.Delegation.Certificate

  dlgCertGenVerKey = VerKeyCardanoDSIGN . CC.Delegation.issuerVK
  dlgCertDlgVerKey = VerKeyCardanoDSIGN . CC.Delegation.delegateVK

  type PBftVerKeyHash PBftCardanoCrypto = CC.Common.KeyHash

  type PBftSigningConstraints PBftCardanoCrypto hdr
    = ( Decoded (Signed hdr)
      , Given (VerKeyDSIGN CardanoDSIGN) :=> HasSignTag (Signed hdr)
      )

  hashVerKey (VerKeyCardanoDSIGN pk) = CC.Common.hashKey pk

  -- This uses some hackery from the 'constraints' package to assemble a
  -- `HasSignTag` constraint from a `Given` constraint and a reified instance of
  -- the instance head/body relationship between the two.
  --
  -- See
  -- https://hackage.haskell.org/package/constraints-0.10.1/docs/Data-Constraint.html#v:-92--92-
  -- for details.
  verifyPBftSigned (_ :: proxy (PBftCardanoCrypto, hdr)) gkVerKey issuer hSig sig
    = give gkVerKey $
      (verifySignedDSIGN
        issuer
        hSig
        sig \\
        (ins :: Given (VerKeyDSIGN CardanoDSIGN) :- HasSignTag (Signed hdr) ))
