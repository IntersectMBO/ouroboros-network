{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.Shelley.Orphans () where

import qualified Cardano.Ledger.Crypto as CryptoClass

import           Ouroboros.Consensus.Shelley.Eras (AlonzoEra)

import           Test.Cardano.Ledger.Mary ()
import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (Mock)
import           Test.Shelley.Spec.Ledger.Generator.EraGen (EraGen (..))
import           Test.Shelley.Spec.Ledger.Generator.ScriptClass
                     (ScriptClass (..))

instance (CryptoClass.Crypto c, Mock c) => EraGen (AlonzoEra c) where
  genGenesisValue = undefined
  genEraTxBody _ge = undefined
  genEraAuxiliaryData = undefined
  updateEraTxBody = undefined

instance (CryptoClass.Crypto c) => ScriptClass (AlonzoEra c) where
  isKey = undefined
  basescript = undefined
  quantify = undefined
  unQuantify = undefined
