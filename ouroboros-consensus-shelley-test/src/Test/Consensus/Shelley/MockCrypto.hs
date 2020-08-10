{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.MockCrypto (
    TPraosMockCrypto
  , Block
  ) where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (HashAlgorithm)
import           Cardano.Crypto.KES (MockKES)

import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

import           Shelley.Spec.Ledger.Crypto (Crypto (..))

-- | A mock replacement for 'TPraosStandardCrypto'
--
-- We run the tests with this mock crypto, as it is easier to generate and
-- debug things. The code is parametric in the crypto, so it shouldn't make
-- much of a difference. This also has the important advantage
-- that we can reuse the generators from cardano-ledger-specs.
data TPraosMockCrypto h

instance HashAlgorithm h => Crypto (TPraosMockCrypto h) where
  type ADDRHASH (TPraosMockCrypto h) = h
  type DSIGN    (TPraosMockCrypto h) = MockDSIGN
  type HASH     (TPraosMockCrypto h) = h
  type KES      (TPraosMockCrypto h) = MockKES 10
  type VRF      (TPraosMockCrypto h) = FakeVRF

instance HashAlgorithm h => TPraosCrypto (TPraosMockCrypto h)

type Block h = ShelleyBlock (TPraosMockCrypto h)
