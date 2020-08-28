{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.MockCrypto (
    BlockCompatByron
  , TPraosMockCryptoCompatByron
  ) where

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash (Blake2b_224, HashAlgorithm)
import           Cardano.Crypto.KES (MockKES)

import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

import           Shelley.Spec.Ledger.Crypto (Crypto (..))

-- | A mock replacement for 'TPraosStandardCrypto' that is compatible with
-- bootstrapping from Byron
--
-- This mocks more components than does
-- 'Test.Consensus.Shelley.MockCrypto.TPraosMockCrypto'. This prevents the
-- @cardano-ledger-specs@ generators from being re-used. Currently, this is not
-- an obstacle for example in the Cardano ThreadNet tests.
--
-- NOTE: The "Ouroboros.Consensus.Cardano.CanHardFork" translation currently
-- assumes that @ADDRHASH@ has the same bit size as Byron address hashes (ie
-- 224); that's why we use 'Blake2b_224' here.
--
-- NOTE: The @shelley-spec-ledger@ package currently requires that @'DSIGN' ~
-- 'Ed25519DSIGN' in order to use Byron bootstrap witnesses.
data TPraosMockCryptoCompatByron h

instance HashAlgorithm h => Crypto (TPraosMockCryptoCompatByron h) where
  type ADDRHASH (TPraosMockCryptoCompatByron h) = Blake2b_224
  type DSIGN    (TPraosMockCryptoCompatByron h) = Ed25519DSIGN
  type HASH     (TPraosMockCryptoCompatByron h) = h
  type KES      (TPraosMockCryptoCompatByron h) = MockKES 10
  type VRF      (TPraosMockCryptoCompatByron h) = FakeVRF

instance HashAlgorithm h => TPraosCrypto (TPraosMockCryptoCompatByron h)

type BlockCompatByron h = ShelleyBlock (TPraosMockCryptoCompatByron h)
