{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto) where

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (ConcreteCrypto)

import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

-- Use 'ConcreteCrypto' from cardano-ledger-specs as our mock crypto. This
-- means we can reuse all their examples and generators.
--
-- We call it 'TPraosMockCrypto', because the name 'ConcreteCrypto' is
-- confusing, \"concrete\" gives the impression it is the real crypto, but
-- it's not (that's our 'TPraosStandardCrypto').
type TPraosMockCrypto = ConcreteCrypto

instance TPraosCrypto TPraosMockCrypto
