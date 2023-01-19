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
import           Cardano.Crypto.VRF (MockVRF)

import           Cardano.Ledger.Crypto (Crypto (..))
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Protocol.TPraos.API as SL
import           Control.State.Transition.Extended (PredicateFailure)

import qualified Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes as SL (Mock)

import           Cardano.Ledger.Shelley.LedgerState (StashedAVVMAddresses)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Protocol.Praos as Praos
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras (EraCrypto, ShelleyEra)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock,
                     ShelleyCompatible)

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
  type VRF      (MockCrypto h) = MockVRF

type MockShelley h = ShelleyEra (MockCrypto h)

instance HashAlgorithm h => SL.PraosCrypto (MockCrypto h)
instance HashAlgorithm h => Praos.PraosCrypto (MockCrypto h)

type Block h = ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h)

-- | Cryptography that can easily be mocked
type CanMock proto era =
  ( ShelleyCompatible proto era
  , LedgerSupportsProtocol (ShelleyBlock proto era)
  , SL.Mock (EraCrypto era)
  , Praos.PraosCrypto (EraCrypto era)
  , Core.EraTx era
  , Arbitrary (Core.TxAuxData era)
  , Arbitrary (Core.PParams era)
  , Arbitrary (Core.PParamsUpdate era)
  , Arbitrary (Core.Script era)
  , Arbitrary (Core.TxBody era)
  , Arbitrary (Core.Tx era)
  , Arbitrary (Core.TxOut era)
  , Arbitrary (Core.Value era)
  , Arbitrary (PredicateFailure (SL.ShelleyUTXOW era))
  , Arbitrary (Core.TxWits era)
  , Arbitrary (StashedAVVMAddresses era)
  , Arbitrary (Core.GovernanceState era)
  )
