{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE ViewPatterns             #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.ThreadNet.TxGen.Shelley (
    ShelleyTxGenExtra(..)
  , genTx
  , mkGenEnv
  , quickCheckAdapter
  ) where

import           Control.Monad.Except (runExcept)
import           Crypto.Number.Generate (generateBetween, generateMax)
import           Crypto.Random (MonadRandom)
import qualified Data.Sequence.Strict as Seq

import           Cardano.Crypto.Hash (HashAlgorithm)

import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool

import qualified Shelley.Spec.Ledger.LedgerState as SL
import qualified Shelley.Spec.Ledger.STS.Ledger as STS
import qualified Shelley.Spec.Ledger.Tx as SL

import           Ouroboros.Consensus.Shelley.Ledger

import           Test.QuickCheck.Gen (Gen (..), suchThat)
import           Test.QuickCheck.Random (mkQCGen)

import           Test.ThreadNet.TxGen (TxGen (..))

import qualified Test.Shelley.Spec.Ledger.ConcreteCryptoTypes as CSL
import qualified Test.Shelley.Spec.Ledger.Generator.Constants as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Core as Gen
import qualified Test.Shelley.Spec.Ledger.Generator.Presets as Gen.Presets
import qualified Test.Shelley.Spec.Ledger.Generator.Utxo as Gen

import           Test.Consensus.Shelley.MockCrypto (TPraosMockCrypto)
import           Test.ThreadNet.Infra.Shelley

data ShelleyTxGenExtra h = ShelleyTxGenExtra
  { -- | Generator environment.
    stgeGenEnv :: Gen.GenEnv h
  }

instance HashAlgorithm h => TxGen (ShelleyBlock (TPraosMockCrypto h)) where

  type TxGenExtra (ShelleyBlock (TPraosMockCrypto h)) = ShelleyTxGenExtra h

  -- TODO #1823
  testGenTxs _numCoreNodes curSlotNo cfg (ShelleyTxGenExtra genEnv) lst = do
      n <- generateBetween 0 20
      go [] n $ applyChainTick (configLedger cfg) curSlotNo lst
    where
      go :: MonadRandom m
         => [GenTx (ShelleyBlock (TPraosMockCrypto h))]  -- ^ Accumulator
         -> Integer  -- ^ Number of txs to still produce
         -> TickedLedgerState (ShelleyBlock (TPraosMockCrypto h))
         -> m [GenTx (ShelleyBlock (TPraosMockCrypto h))]
      go acc 0 _  = return (reverse acc)
      go acc n st = do
        tx <- quickCheckAdapter $ genTx cfg st genEnv
        case runExcept $ applyTx (configLedger cfg) tx st of
          -- We don't mind generating invalid transactions
          Left  _   -> go (tx:acc) (n - 1) st
          Right st' -> go (tx:acc) (n - 1) st'

genTx
  :: forall h. HashAlgorithm h
  => TopLevelConfig (ShelleyBlock (TPraosMockCrypto h))
  -> TickedLedgerState (ShelleyBlock (TPraosMockCrypto h))
  -> Gen.GenEnv h
  -> Gen (GenTx (ShelleyBlock (TPraosMockCrypto h)))
genTx _cfg Ticked { tickedSlotNo, tickedLedgerState } genEnv =
    mkShelleyTx <$> Gen.genTx
      genEnv
      ledgerEnv
      (utxoSt, dpState) `suchThat` isSimpleTx
  where
    -- Filter (for the moment) to "simple" transactions - in particular, we
    -- filter all transactions which have certificates. Testing with
    -- certificates requires additional handling in the testing framework,
    -- because, for example, they may transfer block issuance rights from one
    -- node to another, and we must have the relevant nodes brought online at
    -- that point.
    isSimpleTx (SL._body -> txb) =
      (Seq.null $ SL._certs txb)
    ShelleyLedgerState { shelleyState } = tickedLedgerState

    epochState :: CSL.EpochState h
    epochState = SL.nesEs shelleyState

    ledgerEnv :: STS.LedgerEnv
    ledgerEnv = STS.LedgerEnv {
        ledgerSlotNo   = tickedSlotNo
      , ledgerIx       = 0 -- TODO Ix
      , ledgerPp       = SL.esPp epochState
      , ledgerAccount  = SL.esAccountState epochState
      }

    utxoSt :: CSL.UTxOState h
    utxoSt =
        SL._utxoState
      . SL.esLState
      $ epochState

    dpState :: CSL.DPState h
    dpState =
        SL._delegationState
      . SL.esLState
      $ epochState

mkGenEnv :: forall h. HashAlgorithm h
         => [CoreNode (TPraosMockCrypto h)]
         -> Gen.GenEnv h
mkGenEnv coreNodes = Gen.GenEnv keySpace constants
  where
    constants :: Gen.Constants
    constants = Gen.defaultConstants
      { Gen.frequencyMIRCert = 0
      }

    keySpace :: Gen.KeySpace h
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

{-------------------------------------------------------------------------------
  QuickCheck to MonadRandom adapter
-------------------------------------------------------------------------------}

-- | Run the generator by producing a random seed
quickCheckAdapter :: MonadRandom m => Gen a -> m a
quickCheckAdapter (MkGen g) = do
    seed <- fromIntegral <$> generateMax (fromIntegral (maxBound :: Int))
    return $ g (mkQCGen seed) 30
