{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Coherence (tests) where

import           Data.Word (Word32)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Cardano.Crypto.Hash (ShortHash)

import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits

import           Cardano.Ledger.Alonzo.Scripts (ExUnits, pointWiseExUnits)
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra)
import           Ouroboros.Consensus.Shelley.Ledger.Block (ShelleyBlock)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (AlonzoMeasure (..))

import           Test.Consensus.Shelley.MockCrypto (MockCrypto)

tests :: TestTree
tests = testGroup "Shelley coherences" [
      testProperty "TxLimits.lessEq uses pointWiseExUnits (<=)" lessEqCoherence
    ]

-- | 'TxLimits.lessEq' and @'pointWiseExUnits' (<=)@ must agree
lessEqCoherence :: Word32 -> ExUnits -> ExUnits -> Property
lessEqCoherence w eu1 eu2 =
    actual === expected
  where
    inj eu = AlonzoMeasure (TxLimits.ByteSize w) eu

    actual =
      TxLimits.lessEq
        @(ShelleyBlock (AlonzoEra (MockCrypto ShortHash)))
        (inj eu1)
        (inj eu2)

    expected = pointWiseExUnits (<=) eu1 eu2
