{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.MockCrypto (
    BlockCompatByron
  , MockShelleyCompatByron
  , MockCryptoCompatByron
  ) where

import           Cardano.Crypto.DSIGN (Ed25519DSIGN)
import           Cardano.Crypto.Hash (Blake2b_224, Blake2b_256)
import           Cardano.Crypto.KES (MockKES)

import           Cardano.Ledger.Crypto (Crypto (..))
import           Shelley.Spec.Ledger.BaseTypes (Shelley)
import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

-- | A replacement for 'Test.Consensus.Shelley.MockCrypto' that is compatible
-- with bootstrapping from Byron.
--
-- * The "Ouroboros.Consensus.Cardano.CanHardFork" translation requires that
--   @ADDRHASH@ has the same bit size as Byron address hashes (ie 224); that's why
--   we use 'Blake2b_224' here.
--
-- * Similarly, @HASH@ has to have the same bit size as Byron header hashes (ie
--   256), that's why we use 'Blake2b_256' here.
--
-- * The @shelley-spec-ledger@ package currently requires that @'DSIGN' ~
--   'Ed25519DSIGN' in order to use Byron bootstrap witnesses.
--
-- * We can still use mock KES and mock VRF.
--
-- Note that many Shelley generators are not instantiated to 'MockShelley' but
-- are constrained by @'CanMock' era@. 'MockShelleyCompatByron' satisfies this
-- constraint, allowing us to reuse these generators for Cardano.
data MockCryptoCompatByron

instance Crypto MockCryptoCompatByron where
  type ADDRHASH MockCryptoCompatByron = Blake2b_224
  type DSIGN    MockCryptoCompatByron = Ed25519DSIGN
  type HASH     MockCryptoCompatByron = Blake2b_256
  type KES      MockCryptoCompatByron = MockKES 10
  type VRF      MockCryptoCompatByron = FakeVRF

type MockShelleyCompatByron = Shelley MockCryptoCompatByron

instance TPraosCrypto MockShelleyCompatByron

type BlockCompatByron = ShelleyBlock MockShelleyCompatByron
