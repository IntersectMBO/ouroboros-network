{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.MockCrypto (
    TPraosMockCrypto
  , Block
  ) where

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)

-- | Use 'ConcreteCrypto' from cardano-ledger-specs as our mock crypto. This
-- means we can reuse all their examples and generators.
--
-- We call it 'TPraosMockCrypto', because the name 'ConcreteCrypto' is
-- confusing, \"concrete\" gives the impression it is the real crypto, but
-- it's not (that's our 'TPraosStandardCrypto').
type TPraosMockCrypto h = ConcreteCrypto h

instance HashAlgorithm h => TPraosCrypto (TPraosMockCrypto h)

-- | We run the tests with mock crypto, as it is easier to generate and debug
-- things. The code is parametric in the crypto, so it shouldn't make much of
-- a difference. This also has the important advantage that we can reuse the
-- generators from cardano-ledger-specs.
type Block h = ShelleyBlock (TPraosMockCrypto h)
