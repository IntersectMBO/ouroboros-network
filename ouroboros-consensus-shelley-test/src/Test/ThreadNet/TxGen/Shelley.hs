{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley (
    ShelleyTxGenExtra (..)
  , WhetherToGeneratePPUs (..)
  , genTx
  , mkGenEnv
  ) where

import           Control.Monad.Except (runExcept)

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Cardano.Ledger.Shelley.API as SL

import           Ouroboros.Consensus.Shelley.Ledger

import           Test.QuickCheck

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Cardano.Ledger.Shelley.Generator.Constants as Gen
import qualified Test.Cardano.Ledger.Shelley.Generator.Core as Gen
import           Test.Cardano.Ledger.Shelley.Generator.EraGen
                     (EraGen (genEraTwoPhase2Arg, genEraTwoPhase3Arg))
import qualified Test.Cardano.Ledger.Shelley.Generator.Presets as Gen.Presets
import           Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import qualified Test.Cardano.Ledger.Shelley.Generator.Utxo as Gen

import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.HFEras ()
import           Test.Consensus.Shelley.MockCrypto (MockCrypto, MockShelley)
import           Test.ThreadNet.Infra.Shelley

data ShelleyTxGenExtra h = ShelleyTxGenExtra
  { -- | Generator environment.
    stgeGenEnv  :: Gen.GenEnv (MockShelley h)
    -- | Generate no transactions before this slot.
  , stgeStartAt :: SlotNo
  }

instance HashAlgorithm h => TxGen (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h)) where

  type TxGenExtra (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h)) = ShelleyTxGenExtra h

  testGenTxs _coreNodeId _numCoreNodes curSlotNo cfg extra lst
      | stgeStartAt > curSlotNo = pure []

      -- TODO Temporarily disable the transaction generator until we fix the
      -- failing assertion in TxSubmission.Inbound, see #2680.
      --
      -- When fixed, remove the line below to re-enable the transaction
      -- generator.
      | otherwise               = pure []

      | otherwise               = do
      n <- choose (0, 20)
      go [] n $ applyChainTick lcfg curSlotNo lst
    where
      ShelleyTxGenExtra
        { stgeGenEnv
        , stgeStartAt
        } = extra

      lcfg :: LedgerConfig (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))
      lcfg = configLedger cfg

      go :: [GenTx (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))
         -> Gen [GenTx (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        mbTx <- genTx cfg curSlotNo st stgeGenEnv
        case mbTx of
          Nothing -> return (reverse acc)  -- cannot afford more transactions
          Just tx -> case runExcept $ fst <$> applyTx lcfg DoNotIntervene curSlotNo tx st of
              -- We don't mind generating invalid transactions
              Left  _   -> go (tx:acc) (n - 1) st
              Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: forall h. HashAlgorithm h
  => TopLevelConfig (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))
  -> SlotNo
  -> TickedLedgerState (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))
  -> Gen.GenEnv (MockShelley h)
  -> Gen (Maybe (GenTx (ShelleyBlock (TPraos (MockCrypto h)) (MockShelley h))))
genTx _cfg slotNo TickedShelleyLedgerState { tickedShelleyLedgerState } genEnv =
    Just . mkShelleyTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (SL.LedgerState utxoSt dpState)
  where
    epochState :: SL.EpochState (MockShelley h)
    epochState = SL.nesEs tickedShelleyLedgerState

    ledgerEnv :: SL.LedgerEnv (MockShelley h)
    ledgerEnv = SL.LedgerEnv {
        ledgerSlotNo   = slotNo
      , ledgerIx       = minBound
      , ledgerPp       = SL.esPp epochState
      , ledgerAccount  = SL.esAccountState epochState
      }

    utxoSt :: SL.UTxOState (MockShelley h)
    utxoSt =
        SL.lsUTxOState
      . SL.esLState
      $ epochState

    dpState :: SL.DPState (MockCrypto h)
    dpState =
        SL.lsDPState
      . SL.esLState
      $ epochState

data WhetherToGeneratePPUs = DoNotGeneratePPUs | DoGeneratePPUs
  deriving (Show)

mkGenEnv ::
     forall h. HashAlgorithm h
  => WhetherToGeneratePPUs
  -> [CoreNode (MockCrypto h)]
  -> Gen.GenEnv (MockShelley h)
mkGenEnv whetherPPUs coreNodes = Gen.GenEnv keySpace scriptSpace constants
  where
    -- Configuration of the transaction generator
    constants :: Gen.Constants
    constants =
        setCerts $
        setPPUs $
        Gen.defaultConstants
          { Gen.frequencyMIRCert = 0
          , Gen.genTxStableUtxoSize = 100
          , Gen.genTxUtxoIncrement = 3
          }
      where
        -- Testing with certificates requires additional handling in the
        -- testing framework, because, for example, they may transfer block
        -- issuance rights from one node to another, and we must have the
        -- relevant nodes brought online at that point.
        setCerts cs = cs{ Gen.maxCertsPerTx = 0 }

        setPPUs cs = case whetherPPUs of
            DoGeneratePPUs    -> cs
            DoNotGeneratePPUs -> cs{ Gen.frequencyTxUpdates = 0 }

    keySpace :: Gen.KeySpace (MockShelley h)
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
            Gen.Presets.keySpace @(MockShelley h) constants

    scriptSpace :: Gen.ScriptSpace (MockShelley h)
    scriptSpace =
      Gen.Presets.scriptSpace @(MockShelley h)
           (genEraTwoPhase3Arg @(MockShelley h))
           (genEraTwoPhase2Arg @(MockShelley h))
