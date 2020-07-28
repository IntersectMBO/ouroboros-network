{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley (
    ShelleyTxGenExtra(..)
  , WhetherToGeneratePPUs(..)
  , genTx
  , mkGenEnv
  ) where

import           Control.Monad.Except (runExcept)

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as STS

import           Ouroboros.Consensus.Shelley.Ledger

import           Test.QuickCheck

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as CSL
import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Presets
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as Gen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.Infra.Shelley

data ShelleyTxGenExtra c = ShelleyTxGenExtra
  { -- | Generator environment.
    stgeGenEnv  :: Gen.GenEnv c
    -- | Generate no transactions before this slot.
  , stgeStartAt :: SlotNo
  }

instance CSL.Mock c => TxGen (ShelleyBlock c) where

  type TxGenExtra (ShelleyBlock c) = ShelleyTxGenExtra c

  testGenTxs _numCoreNodes curSlotNo cfg extra lst
      | stgeStartAt > curSlotNo = pure []
      | otherwise               = do
      n <- choose (0, 20)
      go [] n $ applyChainTick (configLedger cfg) curSlotNo lst
    where
      ShelleyTxGenExtra
        { stgeGenEnv
        , stgeStartAt
        } = extra

      go :: [GenTx (ShelleyBlock c)]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (ShelleyBlock c)
         -> Gen [GenTx (ShelleyBlock c)]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        tx <- genTx cfg curSlotNo st stgeGenEnv
        case runExcept $ applyTx (configLedger cfg) curSlotNo tx st of
          -- We don't mind generating invalid transactions
          Left  _   -> go (tx:acc) (n - 1) st
          Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: forall h. CSL.Mock c
  => TopLevelConfig (ShelleyBlock c)
  -> SlotNo
  -> TickedLedgerState (ShelleyBlock c)
  -> Gen.GenEnv c
  -> Gen (GenTx (ShelleyBlock c))
genTx _cfg slotNo TickedShelleyLedgerState { tickedShelleyState } genEnv =
    mkShelleyTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (utxoSt, dpState)
  where
    epochState :: CSL.EpochState c
    epochState = SL.nesEs tickedShelleyState

    ledgerEnv :: STS.LedgerEnv
    ledgerEnv = STS.LedgerEnv {
        ledgerSlotNo   = slotNo
      , ledgerIx       = 0 -- TODO Ix
      , ledgerPp       = SL.esPp epochState
      , ledgerAccount  = SL.esAccountState epochState
      }

    utxoSt :: CSL.UTxOState c
    utxoSt =
        SL._utxoState
      . SL.esLState
      $ epochState

    dpState :: CSL.DPState c
    dpState =
        SL._delegationState
      . SL.esLState
      $ epochState

data WhetherToGeneratePPUs = DoNotGeneratePPUs | DoGeneratePPUs
  deriving (Show)

mkGenEnv :: forall c. CSL.Mock c
         => WhetherToGeneratePPUs
         -> [CoreNode c]
         -> Gen.GenEnv c
mkGenEnv whetherPPUs coreNodes = Gen.GenEnv keySpace constants
  where
    -- Configuration of the transaction generator
    constants :: Gen.Constants
    constants =
        setCerts $
        setPPUs $
        Gen.defaultConstants{ Gen.frequencyMIRCert = 0 }
      where
        -- Testing with certificates requires additional handling in the
        -- testing framework, because, for example, they may transfer block
        -- issuance rights from one node to another, and we must have the
        -- relevant nodes brought online at that point.
        setCerts cs = cs{ Gen.maxCertsPerTx = 0 }

        setPPUs cs = case whetherPPUs of
            DoGeneratePPUs    -> cs
            DoNotGeneratePPUs -> cs{ Gen.frequencyTxUpdates = 0 }

    keySpace :: Gen.KeySpace c
    keySpace =
      Gen.KeySpace
        (cnkiCoreNode <$> cn)
        ksGenesisDelegates
        ksStakePools
        (ksKeyPairs <> (cnkiKeyPair <$> cn))
        ksMSigScripts
      where
        cn = coreNodeKeys <$> coreNodes
        Gen.KeySpace_
          { ksKeyPairs,
            ksMSigScripts,
            ksGenesisDelegates,
            ksStakePools
          } =
            Gen.Presets.keySpace constants
