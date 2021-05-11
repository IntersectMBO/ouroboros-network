{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Shelley.MockCrypto (
    Block
  , CanMock
  , MockCrypto
  , MockShelley
  ) where

import           Test.QuickCheck (Arbitrary)

import           Cardano.Crypto.DSIGN (MockDSIGN)
import           Cardano.Crypto.Hash (HashAlgorithm)
import           Cardano.Crypto.KES (MockKES)

import qualified Cardano.Ledger.Core as Core
import           Cardano.Ledger.Crypto (Crypto (..))
import qualified Cardano.Ledger.Era as Core
import           Control.State.Transition.Extended (PredicateFailure)
import qualified Shelley.Spec.Ledger.API as SL
import qualified Shelley.Spec.Ledger.BlockChain as SL
import qualified Shelley.Spec.Ledger.Tx as SL (ValidateScript)

import           Test.Cardano.Crypto.VRF.Fake (FakeVRF)
import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as SL (Mock)
import qualified Test.Shelley.Spec.Ledger.Generator.EraGen as SL (EraGen)

import           Ouroboros.Consensus.Shelley.Eras (EraCrypto, ShelleyBasedEra,
                     ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (PraosCrypto)

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

type MockShelley h = ShelleyEra (MockCrypto h)

instance HashAlgorithm h => PraosCrypto (MockCrypto h)

type Block h = ShelleyBlock (MockShelley h)

-- | Cryptography that can easily be mocked
type CanMock era =
  ( ShelleyBasedEra era
  , SL.EraGen era
  , SL.Mock (EraCrypto era)
  , SL.ValidateScript era
  , Arbitrary (Core.AuxiliaryData era)
  , Arbitrary (Core.PParams era)
  , Arbitrary (Core.Script era)
  , Arbitrary (Core.TxBody era)
  , Arbitrary (Core.TxInBlock era)
  , Arbitrary (Core.TxOut era)
  , Arbitrary (Core.Value era)
  , Arbitrary (PredicateFailure (SL.UTXOW era))
  , Core.TxSeq era ~ SL.TxSeq era
  )
