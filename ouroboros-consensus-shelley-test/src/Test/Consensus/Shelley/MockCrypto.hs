{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.MockCrypto (
    MockCrypto
  , MockShelley
  , Block
  , CanMock
  ) where

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (HashAlgorithm)
import           Cardano.Crypto.KES (MockKES)

import           Cardano.Ledger.Crypto (Crypto (..))
import qualified Cardano.Ledger.Era as Era (Era (Crypto))
import           Cardano.Ledger.Shelley (Shelley)
import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as SL

import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (TPraosCrypto)

-- | A mock replacement for 'StandardCrypto'
--
-- We run the tests with this mock crypto, as it is easier to generate and
-- debug things. The code is parametric in the crypto, so it shouldn't make
-- much of a difference. This also has the important advantage
-- that we can reuse the generators from cardano-ledger-specs.
data MockCrypto h

instance HashAlgorithm h => Crypto (MockCrypto h) where
  type ADDRHASH (MockCrypto h) = h
  type DSIGN    (MockCrypto h) = MockDSIGN
  type HASH     (MockCrypto h) = h
  type KES      (MockCrypto h) = MockKES 10
  type VRF      (MockCrypto h) = FakeVRF

type MockShelley h = Shelley (MockCrypto h)

instance HashAlgorithm h => TPraosCrypto (MockShelley h)

type Block h = ShelleyBlock (MockShelley h)

-- | Cryptography that can easily be mocked
type CanMock era = (Era.Era era, SL.Mock (Era.Crypto era))
