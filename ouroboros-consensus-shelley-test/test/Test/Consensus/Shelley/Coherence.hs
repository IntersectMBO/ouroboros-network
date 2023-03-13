{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Shelley.Coherence (tests) where

import           Cardano.Ledger.Alonzo.Scripts (ExUnits, pointWiseExUnits)
import           Data.Word (Word32)
import qualified Ouroboros.Consensus.Mempool.Capacity as MempoolCapacity
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (AlonzoMeasure (..),
                     fromExUnits)
import           Test.Cardano.Ledger.Alonzo.Serialisation.Generators ()
import           Test.Tasty
import           Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "Shelley coherences" [
      testProperty "MempoolCapacity.<= uses pointWiseExUnits (<=)" leqCoherence
    ]

-- | 'MempoolCapacity.<=' and @'pointWiseExUnits' (<=)@ must agree
leqCoherence :: Word32 -> ExUnits -> ExUnits -> Property
leqCoherence w eu1 eu2 =
    actual === expected
  where
    inj eu = AlonzoMeasure (MempoolCapacity.ByteSize w) (fromExUnits eu)

    actual   = inj eu1 MempoolCapacity.<= inj eu2
    expected = pointWiseExUnits (<=) eu1 eu2
