{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Consensus.Cardano.Translation (tests) where

import qualified Cardano.Chain.Common as CC
import qualified Cardano.Chain.UTxO as CC

import           Ouroboros.Consensus.Cardano.CanHardFork
import           Ouroboros.Consensus.Shelley.Protocol

import qualified Shelley.Spec.Ledger.Address as SL
import qualified Shelley.Spec.Ledger.Coin as SL
import qualified Shelley.Spec.Ledger.TxData as SL

import           Test.QuickCheck.Hedgehog (hedgehog)
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Test.Cardano.Chain.UTxO.Gen (genCompactTxOut)

{------------------------------------------------------------------------------
  Top-level tests
------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Translation" [
      testProperty "translateTxOut correctness" prop_translateTxOut_correctness
    ]

{------------------------------------------------------------------------------
  Properties
------------------------------------------------------------------------------}

prop_translateTxOut_correctness :: CC.CompactTxOut -> Property
prop_translateTxOut_correctness compactTxOut =
        translateTxOutByronToShelley
          @TPraosStandardCrypto
          (CC.fromCompactTxOut compactTxOut)
    === translateCompactTxOutByronToShelley compactTxOut

{------------------------------------------------------------------------------
  Reference implementation
------------------------------------------------------------------------------}

translateTxOutByronToShelley :: forall sc. Crypto sc => CC.TxOut -> SL.TxOut sc
translateTxOutByronToShelley (CC.TxOut addr amount) =
    SL.TxOut (translateAddr addr) (translateAmount amount)
  where
    translateAmount :: CC.Lovelace -> SL.Coin
    translateAmount = SL.Coin . CC.lovelaceToInteger

    translateAddr :: CC.Address -> SL.Addr sc
    translateAddr = SL.AddrBootstrap . SL.BootstrapAddress

{------------------------------------------------------------------------------
  Generators
------------------------------------------------------------------------------}

instance Arbitrary CC.CompactTxOut where
  arbitrary = hedgehog genCompactTxOut
