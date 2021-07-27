{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Coherence (tests) where

import           Data.Word (Word32)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import qualified Ouroboros.Consensus.Mempool.TxLimits as TxLimits

import           Cardano.Ledger.Alonzo.Scripts (ExUnits, pointWiseExUnits)
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()

import           Ouroboros.Consensus.Shelley.Ledger.Mempool (AlonzoMeasure (..))

tests :: TestTree
tests = testGroup "Shelley coherences" [
      testProperty "TxLimits.<= uses pointWiseExUnits (<=)" leqCoherence
    ]

-- | 'TxLimits.<=' and @'pointWiseExUnits' (<=)@ must agree
leqCoherence :: Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w eu1 eu2 =
    actual === expected
  where
    inj eu = AlonzoMeasure (TxLimits.ByteSize w) eu

    actual   = inj eu1 TxLimits.<= inj eu2
    expected = pointWiseExUnits (<=) eu1 eu2
